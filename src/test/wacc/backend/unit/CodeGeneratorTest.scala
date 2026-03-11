package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Assertion

import wacc.backend.*
import wacc.backend.RegisterAllocator.AllocationResult
import wacc.backend.TAC
import wacc.backend.Arm

/**
  * Unit tests for the CodeGenerator
  */
class CodeGeneratorTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal RegProgram with the given body instructions */
    private def programWith(body: List[TAC.Instruction],
                            alloc: Map[TAC.VirtualReg, Location] = Map.empty): Arm.Program = {
        val prog = TAC.RegProgram(List.empty, List.empty, body)
        val allocResult = AllocationResult(alloc, Map.empty)
        val armProg = CodeGenerator.generate(prog, allocResult)
        armProg
    }

    /** Helper to check if a list contains or doesn't contain a string */
    private def checkStringListContains(list: List[String],
                                        target: String,
                                        shouldExist: Boolean): Assertion = {
        if (shouldExist) {
            list should contain (target)
        } else {
            list shouldNot contain (target)
        }
    }

    /** Check that a label exists/doesn't exist in the widget IR in a test */
    private def checkLabel(body: List[Arm.Instruction],
                           labelName: String,
                           shouldExist: Boolean = true): Assertion = {
        val extractedLabels = body.collect { 
            case Arm.LabelDef(Arm.AddrLabel(extractedLabel)) => extractedLabel 
        }
        checkStringListContains(extractedLabels, labelName, shouldExist)
    }

    /** Check that a bl instruction to a label exists/doesn't exist in the widget IR in a test */
    private def checkBranchLinkLabel(body: List[Arm.Instruction],
                                     labelName: String,
                                     shouldExist: Boolean = true) = {

        val branchLinkLabels = body.collect { 
            case Arm.Bl(Arm.AddrLabel(label)) => label 
        }
        checkStringListContains(branchLinkLabels, labelName, shouldExist)
    }

    /** Scratch register 0 used by the code generator */
    private val scratch0 = PhysicalReg.ipcScratch1

    /* ==================== Mov Tests ==================== */

    "CodeGenerator generateMov" should "generate Arm.Movz for Mov with small immediate source" in {
        val dest = TAC.VirtualReg(0, Size.W)
        val destLoc = PhysicalReg.returnRegister
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(42))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: Arm.LabelDef | _: Arm.StorePair | _: Arm.LoadPair |
                 Arm.Ret | Arm.MovReg(Size.X, _, _) => false
            case _ => true
        }

        bodyInstrs should contain (Arm.Movz(Size.W, scratch0, Arm.Immediate(42)))
    }

    it should "generate Arm.Movz and Arm.Movk for Mov with an immediate source >16 bits" in {
        val dest = TAC.VirtualReg(0, Size.W)
        val destLoc = PhysicalReg.returnRegister
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(131071))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: Arm.LabelDef | _: Arm.StorePair | _: Arm.LoadPair |
                 Arm.Ret | Arm.MovReg(Size.X, _, _) => false
            case _ => true
        }

        /* 16 bits in the bottom 16 */
        bodyInstrs should contain (Arm.Movz(Size.W, scratch0, Arm.Immediate(AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH)))

        /* 1 bit in the top 16 */
        bodyInstrs should contain (Arm.Movk(Size.W, scratch0, Arm.Immediate(1), ShiftAmounts._16)) 
    }

    it should "generate Arm.Movz, Arm.Movk and sign extension for Mov with negative imm and >16 bits to an X reg" in {
        val dest = TAC.VirtualReg(0, Size.X)
        val destLoc = PhysicalReg.returnRegister
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(-131071))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: Arm.LabelDef | _: Arm.StorePair | _: Arm.LoadPair |
                 Arm.Ret | Arm.MovReg(Size.X, _, _) => false
            case _ => true
        }

        /* 16 bits in the bottom 16 to a W register importantly */
        bodyInstrs should contain (Arm.Movz(Size.W, scratch0, Arm.Immediate(1)))

        /* 1 bit in the top 16 to a W register importantly */
        bodyInstrs should contain (Arm.Movk(Size.W, scratch0,
                                   Arm.Immediate(AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH - 1),
                                   ShiftAmounts._16)) 

        /* Check for a sign extension from the W register scratch back to an X register*/
        bodyInstrs should contain (Arm.Sxtw(scratch0, scratch0))
    }

    it should "generate Arm.MovReg when moving between registers" in {
        val src = TAC.VirtualReg(0, Size.W)
        val dest = TAC.VirtualReg(1, Size.W)
        val srcLoc = PhysicalReg.returnRegister
        val destLoc = PhysicalReg.argRegisters(1)
        val alloc = Map(
            src -> (srcLoc: Location),
            dest -> (destLoc: Location)
        )

        val result = programWith(List(TAC.Mov(dest, src)), alloc)

        /* Should contain a mov from src register to dest register (possibly through scratch) */
        val movInstrs = result.body.collect {
            case m: Arm.MovReg if m.size == Size.W => m
        }
        movInstrs should not be empty
    }

    it should "generate a load (ldur) from stack when source is on the stack" in {
        val src = TAC.VirtualReg(0, Size.W)
        val dest = TAC.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (StackSlot(-MemoryLayout.BYTES_PER_STACK_ARG): Location),
            dest -> (PhysicalReg.returnRegister: Location)
        )

        val result = programWith(List(TAC.Mov(dest, src)), alloc)

        val loads = result.body.collect { case l: Arm.Ldur => l }
        loads should not be empty
        loads.head.src shouldBe Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(-MemoryLayout.BYTES_PER_STACK_ARG))
    }

    it should "generate a store (stur) to stack when destination is on the stack" in {
        val dest = TAC.VirtualReg(0, Size.W)
        val alloc = Map(dest -> (StackSlot(-MemoryLayout.BYTES_PER_STACK_ARG): Location))

        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(5))), alloc)

        val stores = result.body.collect { case s: Arm.Stur => s }
        stores should not be empty
        stores.head.src shouldBe Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(-MemoryLayout.BYTES_PER_STACK_ARG))
    }

    /* ==================== Call Tests ==================== */

    "CodeGenerator generateCall" should "move immediate argument directly to arg register" in {
        val result = programWith(
            List(TAC.Call(TAC.Label("exit"), List(TAC.Immediate(7)), dest=None)),
            Map.empty
        )

        val movImms = result.body.collect {
            case m @ Arm.Movz(Size.W, _, Arm.Immediate(7)) => m
        }
        movImms should have length 1
        movImms.head.dest shouldBe PhysicalReg.returnRegister
    }

    it should "emit a branch-and-link instruction to the correct label" in {
        val result = programWith(
            List(TAC.Call(TAC.Label("exit"), List(TAC.Immediate(0)), dest=None)),
            Map.empty
        )

        result.body should contain (Arm.Bl(Arm.AddrLabel("exit")))
    }

    it should "move virtual register argument to arg register" in {
        val argReg = TAC.VirtualReg(0, Size.W)
        val alloc = Map(argReg -> (PhysicalReg.callerSaved(0): Location))

        val result = programWith(
            List(TAC.Call(TAC.Label("myFunc"), List(argReg), dest=None)),
            alloc
        )

        result.body should contain (Arm.Bl(Arm.AddrLabel("myFunc")))
    }

    it should "store return value to destination register" in {
        val destReg = TAC.VirtualReg(0, Size.W)
        val destPhys = PhysicalReg.callerSaved(0)
        val alloc = Map(destReg -> (destPhys: Location))

        val result = programWith(
            List(TAC.Call(TAC.Label("getVal"), List.empty, Some(destReg))),
            alloc
        )

        /* After bl, there should be a mov from w0 to the destination */
        val blIdx = result.body.indexWhere {
            case _: Arm.Bl => true
            case _ => false
        }
        blIdx should be >= 0

        val afterBl = result.body.drop(blIdx + 1)
        val movs = afterBl.collect {
            case m @ Arm.MovReg(Size.W, dest, src) if dest == destPhys => m
        }
        movs should not be empty
    }

    it should "handle multiple immediate arguments" in {
        val result = programWith(
            List(TAC.Call(TAC.Label("twoArgs"), List(TAC.Immediate(1), TAC.Immediate(2)), dest=None)),
            Map.empty
        )

        result.body should contain (Arm.Movz(Size.W, PhysicalReg.returnRegister, Arm.Immediate(1)))
        result.body should contain (Arm.Movz(Size.W, PhysicalReg.argRegisters(1), Arm.Immediate(2)))
    }

    /* ==================== Function Prologue/Epilogue Tests ==================== */

    "CodeGenerator function prologue" should "save fp and lr with pre-index addressing" in {
        val result = programWith(List.empty)

        val storePairs = result.body.collect { case s: Arm.StorePair => s }
        storePairs should not be empty

        val prologue = storePairs.head
        prologue.size shouldBe Size.X
        prologue.src1 shouldBe PhysicalReg.framePointer
        prologue.src2 shouldBe PhysicalReg.linkReg
        prologue.addr shouldBe Arm.AddrPreIndex(PhysicalReg.stackPointer,
                                                Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
    }

    it should "set frame pointer to stack pointer" in {
        val result = programWith(List.empty)

        result.body should contain (
            Arm.MovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer)
        )
    }

    "CodeGenerator function epilogue" should "restore fp and lr with post-index addressing" in {
        val result = programWith(List.empty)

        val loadPairs = result.body.collect { case l: Arm.LoadPair => l }
        loadPairs should not be empty

        val epilogue = loadPairs.last
        epilogue.size shouldBe Size.X
        epilogue.dest1 shouldBe PhysicalReg.framePointer
        epilogue.dest2 shouldBe PhysicalReg.linkReg
        epilogue.addr shouldBe Arm.AddrPostIndex(PhysicalReg.stackPointer,
                                                 Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
    }

    /* ==================== End-to-End Exit Code Generation ==================== */

    "CodeGenerator exit program" should "generate correct instruction sequence for exit 7" in {
        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Call(TAC.Label("exit"), List(TAC.Immediate(7)), dest=None))
        )
        val alloc = RegisterAllocator.allocate(prog)
        val armProg = CodeGenerator.generate(prog, alloc)

        /* Should contain: main label, prologue (stp, mov fp), mov w0 #7, bl exit, epilogue (ldp) */
        armProg.body should contain (Arm.LabelDef(Arm.AddrLabel(LabelNameFormatter.mainLabel)))
        armProg.body should contain (Arm.Movz(Size.W, PhysicalReg.returnRegister, Arm.Immediate(7)))
        armProg.body should contain (Arm.Bl(Arm.AddrLabel("exit")))
    }

    /* ==================== Function Definition Code Generation ==================== */

    "CodeGenerator generateFunc" should "generate label, prologue, epilogue, and ret" in {
        val funcDef = TAC.FunctionDef("myFunc", List.empty, List.empty)
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, Map.empty, ctx)

        instrs.head shouldBe Arm.LabelDef(Arm.AddrLabel("myFunc"))
        instrs should contain (Arm.Ret)

        /* Should have prologue (stp) and epilogue (ldp) */
        instrs.collect { case s: Arm.StorePair => s } should not be empty
        instrs.collect { case l: Arm.LoadPair => l } should not be empty
    }

    it should "include body instructions between prologue and epilogue" in {
        val bodyReg = TAC.VirtualReg(0, Size.W)
        val funcDef = TAC.FunctionDef(
            "addFunc",
            List.empty,
            List(TAC.Mov(bodyReg, TAC.Immediate(10)))
        )
        val alloc = Map(bodyReg -> (PhysicalReg.returnRegister: Location))
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        /* Mov should be between stp and ldp */
        val stpIdx = instrs.indexWhere {
            case _: Arm.StorePair => true
            case _ => false
        }
        val ldpIdx = instrs.indexWhere {
            case _: Arm.LoadPair => true
            case _ => false
        }
        val movIdx = instrs.indexWhere {
            case Arm.Movz(Size.W, _, Arm.Immediate(10)) => true
            case _ => false
        }

        stpIdx should be >= 0
        ldpIdx should be >= 0
        movIdx should be > stpIdx
        movIdx should be < ldpIdx
    }

    /* ==================== Comparison Instruction Tests ==================== */

    "CodeGenerator generateCmp" should "generate Arm.CmpReg for register-register comparison" in {
        val src1 = TAC.VirtualReg(0, Size.W)
        val src2 = TAC.VirtualReg(1, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(TAC.Cmp(src1, src2)), alloc)

        val cmpInstrs = result.body.collect { case c: Arm.CmpReg => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.W
    }

    it should "generate Arm.CmpImm for register-immediate comparison" in {
        val src1 = TAC.VirtualReg(0, Size.W)
        val alloc = Map(src1 -> (PhysicalReg.returnRegister: Location))

        val result = programWith(List(TAC.Cmp(src1, TAC.Immediate(42))), alloc)

        val cmpInstrs = result.body.collect { case c: Arm.CmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.W
        cmpInstrs.head.src2 shouldBe Arm.Immediate(42)
    }

    /* ==================== SetCond Instruction Tests ==================== */

    "CodeGenerator generateSetCond" should "generate Arm.CSet with correct condition code for EQ" in {
        val dest = TAC.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.returnRegister: Location))

        val result = programWith(List(TAC.SetCond(dest, Condition.EQ)), alloc)

        val csetInstrs = result.body.collect { case c: Arm.CSet => c }
        csetInstrs should have length 1
        csetInstrs.head.cond shouldBe Condition.EQ
    }

    it should "generate Arm.CSet with correct condition code for NE" in {
        val dest = TAC.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.returnRegister: Location))

        val result = programWith(List(TAC.SetCond(dest, Condition.NE)), alloc)

        val csetInstrs = result.body.collect { case c: Arm.CSet => c }
        csetInstrs.head.cond shouldBe Condition.NE
    }

    it should "generate Arm.CSet with correct condition code for GT" in {
        val dest = TAC.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.returnRegister: Location))

        val result = programWith(List(TAC.SetCond(dest, Condition.GT)), alloc)

        val csetInstrs = result.body.collect { case c: Arm.CSet => c }
        csetInstrs.head.cond shouldBe Condition.GT
    }

    it should "generate Arm.CSet with correct condition code for LT" in {
        val dest = TAC.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.returnRegister: Location))

        val result = programWith(List(TAC.SetCond(dest, Condition.LT)), alloc)

        val csetInstrs = result.body.collect { case c: Arm.CSet => c }
        csetInstrs.head.cond shouldBe Condition.LT
    }

    /* ==================== Boolean Operator Tests ==================== */

    "CodeGenerator generateNot" should "generate cmp + cset for boolean NOT" in {
        val src = TAC.VirtualReg(0, Size.B)
        val dest = TAC.VirtualReg(1, Size.B)
        val alloc = Map(
            src -> (PhysicalReg.returnRegister: Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(TAC.Not(dest, src)), alloc)

        // Should emit: cmp src, #0 then cset dest, eq
        val cmpInstrs = result.body.collect { case c: Arm.CmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.B
        cmpInstrs.head.src2.value shouldBe 0

        val csetInstrs = result.body.collect { case c: Arm.CSet => c }
        csetInstrs should have length 1
        csetInstrs.head.size shouldBe Size.B
        csetInstrs.head.cond shouldBe Condition.EQ
    }

    "CodeGenerator generateBinaryOp for And" should "generate Arm.AndReg for register-register AND" in {
        val src1 = TAC.VirtualReg(0, Size.B)
        val src2 = TAC.VirtualReg(1, Size.B)
        val dest = TAC.VirtualReg(2, Size.B)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val result = programWith(List(TAC.And(dest, src1, src2)), alloc)

        val andInstrs = result.body.collect { case a: Arm.AndReg => a }
        andInstrs should have length 1
        andInstrs.head.size shouldBe Size.B
    }

    "CodeGenerator generateBinaryOp for Or" should "generate Arm.OrReg for register-register OR" in {
        val src1 = TAC.VirtualReg(0, Size.B)
        val src2 = TAC.VirtualReg(1, Size.B)
        val dest = TAC.VirtualReg(2, Size.B)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val result = programWith(List(TAC.Or(dest, src1, src2)), alloc)

        val orInstrs = result.body.collect { case o: Arm.OrReg => o }
        orInstrs should have length 1
        orInstrs.head.size shouldBe Size.B
    }

    /* ==================== Binary Operator Tests ==================== */

    "CodeGenerator for generateMul" should "generate safe multiplicaton for Mul" in {
        val src1 = TAC.VirtualReg(0, Size.W)
        val src2 = TAC.VirtualReg(1, Size.W)
        val dest = TAC.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val prog = TAC.RegProgram(List.empty, List.empty, List(TAC.Mul(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val armProg  = CodeGenerator.generate(prog, allocResult)

        armProg.body.collect { case m: Arm.SMull => m } should not be empty
        armProg.body.collect { case s: Arm.Sxtw => s } should not be empty
        armProg.body.collect { case c: Arm.CmpReg if c.size == Size.X => c } should not be empty
        
        val branches = armProg.body.collect { case b: Arm.BranchCond => b }
        branches.exists(b => b.cond == Condition.NE && b.label.name == IntOverflowErrorWidget.label) shouldBe true

        checkLabel(armProg.body, IntOverflowErrorWidget.label)
    }

    "CodeGenerator for generateDivMod" should "generate safe division for Div" in {
        val src1 = TAC.VirtualReg(0, Size.W)
        val src2 = TAC.VirtualReg(1, Size.W)
        val dest = TAC.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )
        val prog = TAC.RegProgram(List.empty, List.empty, List(TAC.Div(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val armProg = CodeGenerator.generate(prog, allocResult)

        val cbzs = armProg.body.collect { case c: Arm.Cbz => c }
        cbzs.exists(_.label.name == DivByZeroErrorWidget.label) shouldBe true

        armProg.body.collect { case x: Arm.DivReg => x } should not be empty
        
        checkLabel(armProg.body, DivByZeroErrorWidget.label)
    }

    it should "generate safe division for Mod" in {
        val src1 = TAC.VirtualReg(0, Size.W)
        val src2 = TAC.VirtualReg(1, Size.W)
        val dest = TAC.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.returnRegister: Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )
        val prog = TAC.RegProgram(List.empty, List.empty, List(TAC.Mod(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val armProg = CodeGenerator.generate(prog, allocResult)

        val cbzs = armProg.body.collect { case c: Arm.Cbz => c }
        cbzs.exists(_.label.name == DivByZeroErrorWidget.label) shouldBe true

        armProg.body.collect { case x: Arm.DivReg => x } should not be empty
        armProg.body.collect { case m: Arm.MSub => m } should not be empty
        
        checkLabel(armProg.body, DivByZeroErrorWidget.label)
    }

    /* ==================== Widget Tests ==================== */

    "CodeGenerator for widgets" should "register PrintIntWidget when Print instruction is used" in {

        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Print(TAC.Immediate(5), PrintIntWidget, false))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)
        
        checkLabel(armProg.body, PrintIntWidget.label)
    }

    it should "register PrintlnWidget when Print instruction has newline=true" in {
        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Print(TAC.Immediate(2), PrintIntWidget, true))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)

        checkLabel(armProg.body, PrintIntWidget.label)
        checkLabel(armProg.body, PrintlnWidget.label)
        checkBranchLinkLabel(armProg.body, CLibLabels.putsLabel)
    }

    it should "NOT use register widgets that are not used" in {
        val prog = TAC.RegProgram(
            List.empty, 
            List.empty, 
            List(TAC.Mov(TAC.VirtualReg(0, Size.W), TAC.Immediate(1)))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)

        checkLabel(armProg.body, PrintStringWidget.label, shouldExist=false)
        checkLabel(armProg.body, MallocWidget.label, shouldExist=false)
    }

    it should "register ReadIntegerWidget when Read instruction is used" in {
        val dest = TAC.VirtualReg(0, Size.W)
        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Read(dest, ReadIntegerWidget))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)

        checkLabel(armProg.body, ReadIntegerWidget.label)
        checkBranchLinkLabel(armProg.body, CLibLabels.scanfLabel)
    }

    it should "register ReadCharWidget when Read instruction is used" in {
        val dest = TAC.VirtualReg(0, Size.B)
        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Read(dest, ReadCharWidget))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)

        checkLabel(armProg.body, ReadCharWidget.label)
        checkBranchLinkLabel(armProg.body, CLibLabels.scanfLabel)
    }

    "CodeGenerator for Chr widgets" should "register widget for invalid ascii int" in {
        val src = TAC.VirtualReg(0, Size.W)
        val dest = TAC.VirtualReg(1, Size.W)
        val prog = TAC.RegProgram(
            List.empty,
            List.empty,
            List(TAC.Chr(dest, src, Some(BadCharWidgetError)))
        )

        val alloc = RegisterAllocator.allocate(prog)

        val armProg = CodeGenerator.generate(prog, alloc)
        
        checkLabel(armProg.body, BadCharWidgetError.label)

        checkBranchLinkLabel(armProg.body, LabelNameFormatter.exitLabel)
    }

    /* ==================== Unary Operator Tests ==================== */

    "CodeGenerator for negation" should "generate Arm.Negs" in {
        val src = TAC.VirtualReg(0, Size.W)
        val dest = TAC.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (PhysicalReg.returnRegister: Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(TAC.Neg(dest, src)), alloc)

        val negInstrs = result.body.collect { case n: Arm.Negs => n }
        negInstrs should have length 1
        negInstrs.head.size shouldBe Size.W
    }

    "CodeGenerator chr" should "generate bounds check and move" in {
        val src = TAC.VirtualReg(0, Size.W)
        val dest = TAC.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (PhysicalReg.returnRegister: Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(TAC.Chr(dest, src, Some(BadCharWidgetError))), alloc)

        val cmpInstrs = result.body.collect { case c: Arm.CmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.src2.value shouldBe Constants.ASCII_MAX

        val branchInstrs = result.body.collect { case b: Arm.BranchCond => b }
        branchInstrs.exists(_.cond == Condition.HI) shouldBe true

        /* If src and dest are different registers we need the mov.
           Didn't have to check this in negate because the neg instr in aarch64
           handles it. */
        val movInstrs = result.body.collect { case m: Arm.MovReg => m }
        movInstrs should not be empty
    }

    /* ==================== Stack Layout & Callee-Saved Tests ==================== */

    "CodeGenerator stack layout" should "push callee-saved registers x19-x28 if used" in {
        val funcDef = TAC.FunctionDef("safeFunc", List.empty, List.empty)
        
        // Force allocator to use x19 and x20
        val v1 = TAC.VirtualReg(1, Size.X)
        val v2 = TAC.VirtualReg(2, Size.X)
        val alloc = Map(
            v1 -> (PhysicalReg.calleeSaved(0): Location), // x19
            v2 -> (PhysicalReg.calleeSaved(1): Location)  // x20
        )
        
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        // Prologue: stp x19, x20, [sp, #-16]!
        val pushes = instrs.collect { 
            case s @ Arm.StorePair(Size.X, r1, _, _) 
                if r1 == PhysicalReg.calleeSaved(0) => s 
        }
        pushes should not be empty
        
        // Epilogue: ldp x19, x20, [sp], #16
        val pops = instrs.collect { 
            case l @ Arm.LoadPair(Size.X, r1, _, _) 
                if r1 == PhysicalReg.calleeSaved(0) => l 
        }
        pops should not be empty
    }

    it should "reserve stack space for locals using sub sp" in {
        val funcDef = TAC.FunctionDef("localFunc", List.empty, List.empty)
        
        // Force a stack slot at -24 (allocator puts locals at negative offsets)
        val v1 = TAC.VirtualReg(1, Size.W)
        val alloc = Map(v1 -> (StackSlot(-MemoryLayout.BYTES_PER_STACK_ARG): Location))
        
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        // Should be sub sp, sp, #32 (because aligned to 16 bytes)
        val subs = instrs.collect { 
            case Arm.SubImm(
                Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer,
                Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
            ) => true
        }
        subs should not be empty
    }

    /* ==================== Negative Offset (LDUR/STUR) Tests ==================== */

    "CodeGenerator load/store" should "generate LDUR for negative stack offsets" in {
        val dest = TAC.VirtualReg(0, Size.W)
        // Stack slot at -8 (within -256 range)
        val alloc = Map(dest -> (StackSlot(-MemoryLayout.BYTES_PER_STACK_ARG): Location)) 
        
        // generate: Mov(x0, v0) -> Load v0 into scratch, move scratch to x0
        val result = programWith(
            List(TAC.Mov(TAC.VirtualReg(1, Size.W), dest)), // v1 = v0(stack)
            alloc + (TAC.VirtualReg(1, Size.W) -> PhysicalReg.returnRegister)
        )

        // Expect Arm.Ldur because offset is -8
        val ldurs = result.body.collect { case i: Arm.Ldur => i }
        ldurs should not be empty
        ldurs.head.src shouldBe Arm.AddrImmOffset(PhysicalReg.framePointer,
                                                  Arm.Immediate(-MemoryLayout.BYTES_PER_STACK_ARG))
    }

    it should "generate STUR for negative stack offsets" in {
        val dest = TAC.VirtualReg(0, Size.W)
        val alloc = Map(dest -> (StackSlot(-MemoryLayout.BYTES_PER_STACK_ARG * 2): Location))
        
        // generate: Mov(v0, 10)
        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(10))), alloc)

        // Expect Arm.Stur because offset is -16
        val sturs = result.body.collect { case i: Arm.Stur => i }
        sturs should not be empty
        sturs.head.src shouldBe Arm.AddrImmOffset(PhysicalReg.framePointer,
                                                  Arm.Immediate(-MemoryLayout.BYTES_PER_STACK_ARG * 2))
    }

    it should "handle large stack offsets by materializing offset in scratch register" in {
        val dest = TAC.VirtualReg(0, Size.W)
        // Large negative offset outside -256 range
        val largeOffset = -1024
        val alloc = Map(dest -> (StackSlot(largeOffset): Location))
        
        val result = programWith(List(TAC.Mov(dest, TAC.Immediate(10))), alloc)

        // -1024 in hex is FFFFFC00
        // Lower 16 bits = 64512 (0xFC00), upper 16 bits = 65535 (0xFFFF)
        
        val movz = result.body.collect { 
            case Arm.Movz(Size.W, reg, Arm.Immediate(64512)) 
                if reg == PhysicalReg.ipcScratch2 => true 
        }
        movz should not be empty

        val movk = result.body.collect { 
            case Arm.Movk(Size.W, reg, Arm.Immediate(AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH), shift) 
                if reg == PhysicalReg.ipcScratch2 => true 
        }
        movk should not be empty

        val stores = result.body.collect { 
            case Arm.Store(_, _, Arm.AddrRegOffset(PhysicalReg.framePointer, offsetReg))
                if offsetReg == PhysicalReg.ipcScratch2 => true
        }
        stores should not be empty
    }

    /* ========================= Function Tests ========================= */

    "CodeGenerator for functions" should "allocate an aligned stack block for > 8 arguments" in {
        val args = List.fill(9)(TAC.Immediate(1))
        val result = programWith(List(TAC.Call(TAC.Label("getNinth"), args, dest=None)), Map.empty)

        /* Get instr where tihe stack pointer is moved */
        val stackAllocInstr = result.body.collectFirst {
            case Arm.SubImm(Size.X, dest, src, Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)) 
                if dest == PhysicalReg.stackPointer && src == PhysicalReg.stackPointer => true
        }
        stackAllocInstr shouldBe Some(true)

        /* Stack pointer points to the 9th arg i.e. 0 offset */
        val stackStore = result.body.collectFirst {
            case Arm.Store(Size.W, _, Arm.AddrImmOffset(PhysicalReg.stackPointer, Arm.Immediate(0))) => true
        }

        stackStore shouldBe Some(true)
    }

    it should "clean up the stack block after the function returns" in {

        /* 3 spilled stack args that need to be aligned i.e. 32 bytes allocated */
        val args = List.fill(11)(TAC.Immediate(1))
        val result = programWith(List(TAC.Call(TAC.Label("manyArgs"), args, dest=None)), Map.empty)

        val branchLinkIndex = result.body.indexWhere {
            case _: Arm.Bl => true
            case _ => false
        }

        /* Post BL we restore the stack pointer */
        val afterBl = result.body.drop(branchLinkIndex + 1)
        val stackCleanup = afterBl.collectFirst {
        case Arm.AddImm(Size.X, dest, _, Arm.Immediate(imm)) 
            if dest == PhysicalReg.stackPointer && 
            imm == AARch64Limits.ARM_STACK_ALIGNMENT_BYTES * 2 => true
        }
        
        stackCleanup shouldBe Some(true)
    }

    it should "move the return value from w0/x0 into the specified destination" in {
        val destVReg = TAC.VirtualReg(0, Size.W)
        val alloc = Map(destVReg -> (PhysicalReg.calleeSaved(0): Location)) 

        val result = programWith(List(TAC.Call(TAC.Label("getVal"), List.empty, Some(destVReg))), alloc)

        val branchLinkIndex = result.body.indexWhere {
            case _: Arm.Bl => true
            case _ => false
        }

        /* Must be a Mov after the BL function call to move the return value to
           the desired callee reg location */
        val afterBl = result.body.drop(branchLinkIndex + 1)
        val captureMov = afterBl.collectFirst {
            case Arm.MovReg(Size.W, dest, src) 
                if dest == PhysicalReg.calleeSaved(0) && src == PhysicalReg.returnRegister => true
        }

        captureMov shouldBe Some(true)
    }

    /* ========================= Pair Tests ========================= */

    "CodeGenerator for pairs" should "generate bl _malloc for HeapAlloc" in {
        val dest = TAC.VirtualReg(0, Size.X)
        val allocMap = Map[TAC.VirtualReg, Location](dest -> PhysicalReg.argRegisters(1)) // Put dest in x1
        
        // generate HeapAlloc(v0, 16)
        val result = programWith(
            List(TAC.HeapAlloc(dest, TAC.Immediate(MemoryLayout.PAIR_HEAP_SIZE_B))),
            allocMap
        )

        // check we load 16 into x0
        val movs = result.body.collect { case m: Arm.Movz => m }
        movs.exists(m => m.dest == PhysicalReg.returnRegister && m.imm == Arm.Immediate(MemoryLayout.PAIR_HEAP_SIZE_B)) shouldBe true

        // check we branch to _malloc
        val bls = result.body.collect { case b: Arm.Bl => b }
        bls.exists(_.label.name == MallocWidget.label) shouldBe true
    }

    it should "generate cmp and b.eq to _errNull for NullCheck" in {
        val ptr = TAC.VirtualReg(0, Size.X)
        val allocMap = Map[TAC.VirtualReg, Location](ptr -> PhysicalReg.returnRegister) // ptr in x0
        
        val result = programWith(List(TAC.NullCheck(ptr)), allocMap)

        // should compare against 0
        val cmps = result.body.collect { case c: Arm.CmpImm => c }
        cmps should not be empty
        cmps.head.src2 shouldBe Arm.Immediate(0)

        // Should branch conditionally (EQ) to _errNull
        val branches = result.body.collect { case b: Arm.BranchCond => b }
        branches should not be empty
        branches.head.cond shouldBe Condition.EQ
        branches.head.label.name shouldBe NullDerefErrorWidget.label 
    }

    it should "generate correct memory offsets for PairLoad" in {
        val ptr = TAC.VirtualReg(0, Size.X)
        val dest = TAC.VirtualReg(1, Size.X)
        val allocMap = Map[TAC.VirtualReg, Location](
            ptr -> PhysicalReg.returnRegister,
            dest -> PhysicalReg.argRegisters(1)
        )
        
        // generate: PairLoad(ptr, Snd, dest)
        val result = programWith(List(TAC.PairLoad(ptr, PairPos.Snd, dest)), allocMap)

        // Expect a Load using the scratch register with an 8-byte offset for Snd
        val loads = result.body.collect { case l: Arm.Load => l }
        loads should not be empty
        
        // Check that the offset is #8
        val memoryAddr = (loads.last.src: @unchecked) match {
            case x: Arm.AddrImmOffset => x
        }
        memoryAddr.offset shouldBe Arm.Immediate(MemoryLayout.PAIR_SLOT_SIZE)
    }

    /* ========================= Free Tests ========================= */
    "The code generator for free statements" should "generate a branch to _freepair for pairs" in {
        val ptrReg = TAC.VirtualReg(0, Size.X)
        val freeInstr = TAC.Free(ptrReg, FreeType.Pair)

        val allocMap = Map[TAC.VirtualReg, Location](
            ptrReg -> PhysicalReg.returnRegister
        )
        
        // 2. Create a minimal RegProgram with just our Free instruction
        val armProg = programWith(List(freeInstr), allocMap)
        
        // should emit a branch to _freepair
        val branchInstrs = armProg.body.collect { case Arm.Bl(label) => label.name }
        branchInstrs should contain (FreePairWidget.label)
    }

    it should "generate a branch to _freearray for arrays" in {
        val ptrReg = TAC.VirtualReg(0, Size.X)
        val freeInstr = TAC.Free(ptrReg, FreeType.Array)

        val allocMap = Map[TAC.VirtualReg, Location](
            ptrReg -> PhysicalReg.returnRegister
        )
        
        val armProg = programWith(List(freeInstr), allocMap)
        
        val branchInstrs = armProg.body.collect { case Arm.Bl(label) => label.name }
        branchInstrs should contain (FreeArrayWidget.label)
    }

    /* ========================================================================================== */
    /* ================================= EXTENSION TESTS ======================================== */
    /* ========================================================================================== */

    "CodeGenerator for tailrec" should "emit an uncond branch to func body label instead of bl" in {
        val result = programWith(
            List(TAC.SelfTailCall(TAC.Label("func"), List.empty)),
            Map.empty
        )

        val blInstrs = result.body.collect { case b: Arm.Bl => b }
        blInstrs shouldBe empty

        val branchInstrs = result.body.collect { case b: Arm.BranchUncond => b }
        branchInstrs should not be empty
        branchInstrs.exists(_.label.name == CodeGenerator.formatFuncBodyLabel("func").name) shouldBe true
    }

    it should "put new tailrec call arguments into the right physical parameter registers" in {
        val args1 = List(TAC.Immediate(10), TAC.Immediate(20))
        val args2 = List(TAC.Immediate(30), TAC.Immediate(40))
        
        val result = programWith(
            List(TAC.SelfTailCall(TAC.Label("func1"), args1),
                 TAC.GeneralTailCall(TAC.Label("func2"), args2)),
            Map.empty
        )

        val movzInstrs = result.body.collect { case m: Arm.Movz => m }
        
        val selfArgZeroMapped = movzInstrs.exists(m => 
            m.dest == PhysicalReg.argRegisters(0) && m.imm == Arm.Immediate(10)
        )
        val selfArgOneMapped = movzInstrs.exists(m => 
            m.dest == PhysicalReg.argRegisters(1) && m.imm == Arm.Immediate(20)
        )

        val generalArgZeroMapped = movzInstrs.exists(m => 
            m.dest == PhysicalReg.argRegisters(0) && m.imm == Arm.Immediate(30)
        )
        val generalArgOneMapped = movzInstrs.exists(m => 
            m.dest == PhysicalReg.argRegisters(1) && m.imm == Arm.Immediate(40)
        )

        selfArgZeroMapped shouldBe true
        selfArgOneMapped shouldBe true
        generalArgZeroMapped shouldBe true
        generalArgOneMapped shouldBe true
    }

    it should "overwrite caller stack args (>8 args) with positive fp offset in parent frame slots" in {

        val args = List.fill(9)(TAC.Immediate(2))

        val result = programWith(
            List(TAC.SelfTailCall(TAC.Label("func1"), args),
                 TAC.GeneralTailCall(TAC.Label("func2"), args)),
            Map.empty
        )

        val stores = result.body.collect { case s: Arm.Store => s }
        val callerStackStore = stores.collect { s =>
            s.src match {
                case Arm.AddrImmOffset(reg, Arm.Immediate(offset)) => 
                    reg == PhysicalReg.framePointer && offset == AARch64Limits.ARM_STACK_ALIGNMENT_BYTES
                case _ => false
            }
        }

        callerStackStore.length shouldBe 2
    }

    it should "not allocate new stack space for self tailrec arguments" in {

        val args = List.fill(10)(TAC.Immediate(2))
        
        val result = programWith(
            List(TAC.SelfTailCall(TAC.Label("func"), args)),
            Map.empty
        )

        /* Grab stack allocations */
        val stackAllocations = result.body.collect {
            case sub @ Arm.SubImm(Size.X, dest, src, _) 
                if dest == PhysicalReg.stackPointer && src == PhysicalReg.stackPointer => sub
        }

        stackAllocations shouldBe empty
    }

    it should "inline the function epilogue for a generalised tailrec call" in {

        val args = List.fill(3)(TAC.Immediate(2))
        
        val result = programWith(
            List(TAC.GeneralTailCall(TAC.Label("func"), args)),
            Map.empty
        )

        /* Expect two epilogues checking by looking for the lr + fp restore */
        val specRegRestores = result.body.collect {
            case load @ Arm.LoadPair(
                Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg, 
                Arm.AddrPostIndex(PhysicalReg.stackPointer,
                                  Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
            ) => load
        }
        specRegRestores.length shouldBe 2

        /* But inlined prologue should not have a return statement */
        val returnStmts = result.body.collect {
            case ret @ Arm.Ret => ret
        }
        returnStmts.length shouldBe 1
    }
    
    it should "emit for a general tail call an uncond branch to func label (pre-prologue) instead of bl" in {
        val result = programWith(
            List(TAC.GeneralTailCall(TAC.Label("func"), List.empty)),
            Map.empty
        )

        val blInstrs = result.body.collect { case b: Arm.Bl => b }
        blInstrs shouldBe empty

        val branchInstrs = result.body.collect { case b: Arm.BranchUncond => b }
        branchInstrs should not be empty
        branchInstrs.exists(_.label.name == "func") shouldBe true
    }
}
