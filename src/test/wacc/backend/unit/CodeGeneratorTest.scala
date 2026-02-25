package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*
import wacc.backend.RegisterAllocator.AllocationResult
import wacc.backend.IR

/**
  * Unit tests for the CodeGenerator
  */
class CodeGeneratorTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal RegProgram with the given body instructions */
    private def programWith(body: List[IR.Instruction],
                            alloc: Map[IR.VirtualReg, Location] = Map.empty): ArmProgram = {
        val prog = IR.RegProgram(List.empty, List.empty, body)
        val allocResult = AllocationResult(alloc, Map.empty)
        val (armProg, _) = CodeGenerator.generate(prog, allocResult)
        armProg
    }

    /** Scratch register 0 used by the code generator */
    private val scratch0 = PhysicalReg.ipcScratch(0)

    /* ==================== Mov Tests ==================== */

    "CodeGenerator generateMov" should "generate ArmMovz for Mov with small immediate source" in {
        val dest = IR.VirtualReg(0, Size.W)
        val destLoc = PhysicalReg.argRegisters(0)
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(IR.Mov(dest, IR.Immediate(42))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: ArmLabelDef | _: ArmStorePair | _: ArmLoadPair |
                 ArmRet | ArmMovReg(Size.X, _, _) => false
            case _ => true
        }

        bodyInstrs should contain (ArmMovz(Size.W, scratch0, ArmImmediate(42)))
    }

    it should "generate ArmMovz and ArmMovk for Mov with an immediate source >16 bits" in {
        val dest = IR.VirtualReg(0, Size.W)
        val destLoc = PhysicalReg.argRegisters(0)
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(IR.Mov(dest, IR.Immediate(131071))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: ArmLabelDef | _: ArmStorePair | _: ArmLoadPair |
                 ArmRet | ArmMovReg(Size.X, _, _) => false
            case _ => true
        }

        /* 16 bits in the bottom 16 */
        bodyInstrs should contain (ArmMovz(Size.W, scratch0, ArmImmediate(65535)))

        /* 1 bit in the top 16 */
        bodyInstrs should contain (ArmMovk(Size.W, scratch0, ArmImmediate(1), ShiftAmounts._16)) 
    }

    it should "generate ArmMovz, ArmMovk and sign extension for Mov with negative imm and >16 bits to an X reg" in {
        val dest = IR.VirtualReg(0, Size.X)
        val destLoc = PhysicalReg.argRegisters(0)
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(IR.Mov(dest, IR.Immediate(-131071))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: ArmLabelDef | _: ArmStorePair | _: ArmLoadPair |
                 ArmRet | ArmMovReg(Size.X, _, _) => false
            case _ => true
        }

        /* 16 bits in the bottom 16 to a W register importantly */
        bodyInstrs should contain (ArmMovz(Size.W, scratch0, ArmImmediate(1)))

        /* 1 bit in the top 16 to a W register importantly */
        bodyInstrs should contain (ArmMovk(Size.W, scratch0, ArmImmediate(65534), ShiftAmounts._16)) 

        /* Check for a sign extension from the W register scratch back to an X register*/
        bodyInstrs should contain (ArmSxtw(scratch0, scratch0))
    }

    it should "generate ArmMovReg when moving between registers" in {
        val src = IR.VirtualReg(0, Size.W)
        val dest = IR.VirtualReg(1, Size.W)
        val srcLoc = PhysicalReg.argRegisters(0)
        val destLoc = PhysicalReg.argRegisters(1)
        val alloc = Map(
            src -> (srcLoc: Location),
            dest -> (destLoc: Location)
        )

        val result = programWith(List(IR.Mov(dest, src)), alloc)

        /* Should contain a mov from src register to dest register (possibly through scratch) */
        val movInstrs = result.body.collect {
            case m: ArmMovReg if m.size == Size.W => m
        }
        movInstrs should not be empty
    }

    it should "generate a load (ldur) from stack when source is on the stack" in {
        val src = IR.VirtualReg(0, Size.W)
        val dest = IR.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (StackSlot(-8): Location),
            dest -> (PhysicalReg.argRegisters(0): Location)
        )

        val result = programWith(List(IR.Mov(dest, src)), alloc)

        val loads = result.body.collect { case l: ArmLdur => l }
        loads should not be empty
        loads.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8))
    }

    it should "generate a store (stur) to stack when destination is on the stack" in {
        val dest = IR.VirtualReg(0, Size.W)
        val alloc = Map(dest -> (StackSlot(-8): Location))

        val result = programWith(List(IR.Mov(dest, IR.Immediate(5))), alloc)

        val stores = result.body.collect { case s: ArmStur => s }
        stores should not be empty
        stores.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8))
    }

    /* ==================== Call Tests ==================== */

    "CodeGenerator generateCall" should "move immediate argument directly to arg register" in {
        val result = programWith(
            List(IR.Call(IR.Label("exit"), List(IR.Immediate(7)), None)),
            Map.empty
        )

        val movImms = result.body.collect {
            case m @ ArmMovz(Size.W, reg, ArmImmediate(7)) => m
        }
        movImms should have length 1
        movImms.head.dest shouldBe PhysicalReg.argRegisters(0)
    }

    it should "emit a branch-and-link instruction to the correct label" in {
        val result = programWith(
            List(IR.Call(IR.Label("exit"), List(IR.Immediate(0)), None)),
            Map.empty
        )

        result.body should contain (ArmBl(ArmAddrLabel("exit")))
    }

    it should "move virtual register argument to arg register" in {
        val argReg = IR.VirtualReg(0, Size.W)
        val alloc = Map(argReg -> (PhysicalReg.callerSaved(0): Location))

        val result = programWith(
            List(IR.Call(IR.Label("myFunc"), List(argReg), None)),
            alloc
        )

        result.body should contain (ArmBl(ArmAddrLabel("myFunc")))
    }

    it should "store return value to destination register" in {
        val destReg = IR.VirtualReg(0, Size.W)
        val destPhys = PhysicalReg.callerSaved(0)
        val alloc = Map(destReg -> (destPhys: Location))

        val result = programWith(
            List(IR.Call(IR.Label("getVal"), List.empty, Some(destReg))),
            alloc
        )

        /* After bl, there should be a mov from w0 to the destination */
        val blIdx = result.body.indexWhere {
            case _: ArmBl => true
            case _ => false
        }
        blIdx should be >= 0

        val afterBl = result.body.drop(blIdx + 1)
        val movs = afterBl.collect {
            case m @ ArmMovReg(Size.W, dest, src) if dest == destPhys => m
        }
        movs should not be empty
    }

    it should "handle multiple immediate arguments" in {
        val result = programWith(
            List(IR.Call(IR.Label("twoArgs"), List(IR.Immediate(1), IR.Immediate(2)), None)),
            Map.empty
        )

        result.body should contain (ArmMovz(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(1)))
        result.body should contain (ArmMovz(Size.W, PhysicalReg.argRegisters(1), ArmImmediate(2)))
    }

    /* ==================== Function Prologue/Epilogue Tests ==================== */

    "CodeGenerator function prologue" should "save fp and lr with pre-index addressing" in {
        val result = programWith(List.empty)

        val storePairs = result.body.collect { case s: ArmStorePair => s }
        storePairs should not be empty

        val prologue = storePairs.head
        prologue.size shouldBe Size.X
        prologue.src1 shouldBe PhysicalReg.framePointer
        prologue.src2 shouldBe PhysicalReg.linkReg
        prologue.addr shouldBe ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
    }

    it should "set frame pointer to stack pointer" in {
        val result = programWith(List.empty)

        result.body should contain (
            ArmMovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer)
        )
    }

    "CodeGenerator function epilogue" should "restore fp and lr with post-index addressing" in {
        val result = programWith(List.empty)

        val loadPairs = result.body.collect { case l: ArmLoadPair => l }
        loadPairs should not be empty

        val epilogue = loadPairs.last
        epilogue.size shouldBe Size.X
        epilogue.dest1 shouldBe PhysicalReg.framePointer
        epilogue.dest2 shouldBe PhysicalReg.linkReg
        epilogue.addr shouldBe ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
    }

    /* ==================== End-to-End Exit Code Generation ==================== */

    "CodeGenerator exit program" should "generate correct instruction sequence for exit 7" in {
        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Call(IR.Label("exit"), List(IR.Immediate(7)), None))
        )
        val alloc = RegisterAllocator.allocate(prog)
        val (armProg, _) = CodeGenerator.generate(prog, alloc)

        /* Should contain: main label, prologue (stp, mov fp), mov w0 #7, bl exit, epilogue (ldp) */
        armProg.body should contain (ArmLabelDef(ArmAddrLabel("main")))
        armProg.body should contain (ArmMovz(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(7)))
        armProg.body should contain (ArmBl(ArmAddrLabel("exit")))
    }

    /* ==================== Function Definition Code Generation ==================== */

    "CodeGenerator generateFunc" should "generate label, prologue, epilogue, and ret" in {
        val funcDef = IR.FunctionDef("myFunc", List.empty, List.empty)
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, Map.empty, ctx)

        instrs.head shouldBe ArmLabelDef(ArmAddrLabel("myFunc"))
        instrs should contain (ArmRet)

        /* Should have prologue (stp) and epilogue (ldp) */
        instrs.collect { case s: ArmStorePair => s } should not be empty
        instrs.collect { case l: ArmLoadPair => l } should not be empty
    }

    it should "include body instructions between prologue and epilogue" in {
        val bodyReg = IR.VirtualReg(0, Size.W)
        val funcDef = IR.FunctionDef(
            "addFunc",
            List.empty,
            List(IR.Mov(bodyReg, IR.Immediate(10)))
        )
        val alloc = Map(bodyReg -> (PhysicalReg.argRegisters(0): Location))
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        /* Mov should be between stp and ldp */
        val stpIdx = instrs.indexWhere {
            case _: ArmStorePair => true
            case _ => false
        }
        val ldpIdx = instrs.indexWhere {
            case _: ArmLoadPair => true
            case _ => false
        }
        val movIdx = instrs.indexWhere {
            case ArmMovz(Size.W, _, ArmImmediate(10)) => true
            case _ => false
        }

        stpIdx should be >= 0
        ldpIdx should be >= 0
        movIdx should be > stpIdx
        movIdx should be < ldpIdx
    }

    /* ==================== Comparison Instruction Tests ==================== */

    "CodeGenerator generateCmp" should "generate ArmCmpReg for register-register comparison" in {
        val src1 = IR.VirtualReg(0, Size.W)
        val src2 = IR.VirtualReg(1, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(IR.Cmp(src1, src2)), alloc)

        val cmpInstrs = result.body.collect { case c: ArmCmpReg => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.W
    }

    it should "generate ArmCmpImm for register-immediate comparison" in {
        val src1 = IR.VirtualReg(0, Size.W)
        val alloc = Map(src1 -> (PhysicalReg.argRegisters(0): Location))

        val result = programWith(List(IR.Cmp(src1, IR.Immediate(42))), alloc)

        val cmpInstrs = result.body.collect { case c: ArmCmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.W
        cmpInstrs.head.src2 shouldBe ArmImmediate(42)
    }

    /* ==================== SetCond Instruction Tests ==================== */

    "CodeGenerator generateSetCond" should "generate ArmCSet with correct condition code for EQ" in {
        val dest = IR.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.argRegisters(0): Location))

        val result = programWith(List(IR.SetCond(dest, Condition.EQ)), alloc)

        val csetInstrs = result.body.collect { case c: ArmCSet => c }
        csetInstrs should have length 1
        csetInstrs.head.cond shouldBe Condition.EQ
    }

    it should "generate ArmCSet with correct condition code for NE" in {
        val dest = IR.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.argRegisters(0): Location))

        val result = programWith(List(IR.SetCond(dest, Condition.NE)), alloc)

        val csetInstrs = result.body.collect { case c: ArmCSet => c }
        csetInstrs.head.cond shouldBe Condition.NE
    }

    it should "generate ArmCSet with correct condition code for GT" in {
        val dest = IR.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.argRegisters(0): Location))

        val result = programWith(List(IR.SetCond(dest, Condition.GT)), alloc)

        val csetInstrs = result.body.collect { case c: ArmCSet => c }
        csetInstrs.head.cond shouldBe Condition.GT
    }

    it should "generate ArmCSet with correct condition code for LT" in {
        val dest = IR.VirtualReg(0, Size.B)
        val alloc = Map(dest -> (PhysicalReg.argRegisters(0): Location))

        val result = programWith(List(IR.SetCond(dest, Condition.LT)), alloc)

        val csetInstrs = result.body.collect { case c: ArmCSet => c }
        csetInstrs.head.cond shouldBe Condition.LT
    }

    /* ==================== Boolean Operator Tests ==================== */

    "CodeGenerator generateNot" should "generate cmp + cset for boolean NOT" in {
        val src = IR.VirtualReg(0, Size.B)
        val dest = IR.VirtualReg(1, Size.B)
        val alloc = Map(
            src -> (PhysicalReg.argRegisters(0): Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(IR.Not(dest, src)), alloc)

        // Should emit: cmp src, #0 then cset dest, eq
        val cmpInstrs = result.body.collect { case c: ArmCmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.size shouldBe Size.B
        cmpInstrs.head.src2.value shouldBe 0

        val csetInstrs = result.body.collect { case c: ArmCSet => c }
        csetInstrs should have length 1
        csetInstrs.head.size shouldBe Size.B
        csetInstrs.head.cond shouldBe Condition.EQ
    }

    "CodeGenerator generateBinaryOp for And" should "generate ArmAndReg for register-register AND" in {
        val src1 = IR.VirtualReg(0, Size.B)
        val src2 = IR.VirtualReg(1, Size.B)
        val dest = IR.VirtualReg(2, Size.B)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val result = programWith(List(IR.And(dest, src1, src2)), alloc)

        val andInstrs = result.body.collect { case a: ArmAndReg => a }
        andInstrs should have length 1
        andInstrs.head.size shouldBe Size.B
    }

    "CodeGenerator generateBinaryOp for Or" should "generate ArmOrReg for register-register OR" in {
        val src1 = IR.VirtualReg(0, Size.B)
        val src2 = IR.VirtualReg(1, Size.B)
        val dest = IR.VirtualReg(2, Size.B)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val result = programWith(List(IR.Or(dest, src1, src2)), alloc)

        val orInstrs = result.body.collect { case o: ArmOrReg => o }
        orInstrs should have length 1
        orInstrs.head.size shouldBe Size.B
    }

    /* ==================== Binary Operator Tests ==================== */

    "CodeGenerator for generateMul" should "generate safe multiplicaton for Mul" in {
        val src1 = IR.VirtualReg(0, Size.W)
        val src2 = IR.VirtualReg(1, Size.W)
        val dest = IR.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )

        val prog = IR.RegProgram(List.empty, List.empty, List(IR.Mul(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val (armProg, dispatcher) = CodeGenerator.generate(prog, allocResult)

        armProg.body.collect { case m: ArmSMull => m } should not be empty
        armProg.body.collect { case s: ArmSxtw => s } should not be empty
        armProg.body.collect { case c: ArmCmpReg if c.size == Size.X => c } should not be empty
        
        val branches = armProg.body.collect { case b: ArmBranchCond => b }
        branches.exists(b => b.cond == Condition.NE && b.label.name == IntOverflowErrorWidget.label) shouldBe true

        dispatcher.getCollectedAssembly should include (IntOverflowErrorWidget.label)
    }

    "CodeGenerator for generateDivMod" should "generate safe division for Div" in {
        val src1 = IR.VirtualReg(0, Size.W)
        val src2 = IR.VirtualReg(1, Size.W)
        val dest = IR.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )
        val prog = IR.RegProgram(List.empty, List.empty, List(IR.Div(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val (armProg, dispatcher) = CodeGenerator.generate(prog, allocResult)

        val cbzs = armProg.body.collect { case c: ArmCbz => c }
        cbzs.exists(_.label.name == DivByZeroErrorWidget.label) shouldBe true

        armProg.body.collect { case x: ArmDivReg => x } should not be empty
        
        dispatcher.getCollectedAssembly should include (DivByZeroErrorWidget.label)
    }

    it should "generate safe division for Mod" in {
        val src1 = IR.VirtualReg(0, Size.W)
        val src2 = IR.VirtualReg(1, Size.W)
        val dest = IR.VirtualReg(2, Size.W)
        val alloc = Map(
            src1 -> (PhysicalReg.argRegisters(0): Location),
            src2 -> (PhysicalReg.argRegisters(1): Location),
            dest -> (PhysicalReg.argRegisters(2): Location)
        )
        val prog = IR.RegProgram(List.empty, List.empty, List(IR.Mod(dest, src1, src2)))
        val allocResult = AllocationResult(alloc, Map.empty)
        val (armProg, dispatcher) = CodeGenerator.generate(prog, allocResult)

        val cbzs = armProg.body.collect { case c: ArmCbz => c }
        cbzs.exists(_.label.name == DivByZeroErrorWidget.label) shouldBe true

        armProg.body.collect { case x: ArmDivReg => x } should not be empty
        armProg.body.collect { case m: ArmMSub => m } should not be empty
        
        dispatcher.getCollectedAssembly should include (DivByZeroErrorWidget.label)
    }

    /* ==================== Widget Tests ==================== */

    "CodeGenerator for widgets" should "register PrintIntWidget when Print instruction is used" in {

        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Print(IR.Immediate(5), PrintIntWidget, false))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)

        val widgetAssembly = dispatcher.getCollectedAssembly
        
        widgetAssembly should include ("_printi:")
        widgetAssembly should include ("%d") 
    }

    it should "register PrintlnWidget when Print instruction has newline=true" in {
        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Print(IR.Immediate(2), PrintIntWidget, true))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)
        val widgetAssembly = dispatcher.getCollectedAssembly

        widgetAssembly should include ("_printi:")
        widgetAssembly should include ("_println:")
        widgetAssembly should include ("puts") // what println uses to make the new line
    }

    it should "NOT register widgets that are not used" in {
        val prog = IR.RegProgram(
            List.empty, 
            List.empty, 
            List(IR.Mov(IR.VirtualReg(0, Size.W), IR.Immediate(1)))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)
        val widgetAssembly = dispatcher.getCollectedAssembly

        widgetAssembly should not include ("_prints:")
        widgetAssembly should not include ("malloc")
    }

    it should "register ReadIntegerWidget when Read instruction is used" in {
        val dest = IR.VirtualReg(0, Size.W)
        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Read(dest, ReadIntegerWidget))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)
        val widgetAssembly = dispatcher.getCollectedAssembly

        widgetAssembly should include ("_readi:")
        widgetAssembly should include ("%d")
        widgetAssembly should include ("scanf")
    }

    it should "register ReadCharWidget when Read instruction is used" in {
        val dest = IR.VirtualReg(0, Size.B)
        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Read(dest, ReadCharWidget))
        )
        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)
        val widgetAssembly = dispatcher.getCollectedAssembly

        widgetAssembly should include ("_readc:")
        widgetAssembly should include ("%c")
        widgetAssembly should include ("scanf")
    }

    "CodeGenerator for Chr widgets" should "register widget for invalid ascii int" in {
        val src = IR.VirtualReg(0, Size.W)
        val dest = IR.VirtualReg(1, Size.W)
        val prog = IR.RegProgram(
            List.empty,
            List.empty,
            List(IR.Chr(dest, src, BadCharWidgetError))
        )

        val alloc = RegisterAllocator.allocate(prog)

        val (_, dispatcher) = CodeGenerator.generate(prog, alloc)
        val widgetAssembly = dispatcher.getCollectedAssembly
        
        widgetAssembly should include ("_errBadChar:")
        widgetAssembly should include ("bl exit") 
    }

    /* ==================== Unary Operator Tests ==================== */

    "CodeGenerator for negation" should "generate ArmNegs" in {
        val src = IR.VirtualReg(0, Size.W)
        val dest = IR.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (PhysicalReg.argRegisters(0): Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(IR.Neg(dest, src)), alloc)

        val negInstrs = result.body.collect { case n: ArmNegs => n }
        negInstrs should have length 1
        negInstrs.head.size shouldBe Size.W
    }

    "CodeGenerator chr" should "generate bounds check and move" in {
        val src = IR.VirtualReg(0, Size.W)
        val dest = IR.VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (PhysicalReg.argRegisters(0): Location),
            dest -> (PhysicalReg.argRegisters(1): Location)
        )

        val result = programWith(List(IR.Chr(dest, src, BadCharWidgetError)), alloc)

        val cmpInstrs = result.body.collect { case c: ArmCmpImm => c }
        cmpInstrs should have length 1
        cmpInstrs.head.src2.value shouldBe 127

        val branchInstrs = result.body.collect { case b: ArmBranchCond => b }
        branchInstrs.exists(_.cond == Condition.HI) shouldBe true

        /* If src and dest are different registers we need the mov.
           Didn't have to check this in negate because the neg instr in aarch64
           handles it. */
        val movInstrs = result.body.collect { case m: ArmMovReg => m }
        movInstrs should not be empty
    }

    /* ==================== Stack Layout & Callee-Saved Tests ==================== */

    "CodeGenerator stack layout" should "push callee-saved registers x19-x28 if used" in {
        val funcDef = IR.FunctionDef("safeFunc", List.empty, List.empty)
        
        // Force allocator to use x19 and x20
        val v1 = IR.VirtualReg(1, Size.X)
        val v2 = IR.VirtualReg(2, Size.X)
        val alloc = Map(
            v1 -> (PhysicalReg.calleeSaved(0): Location), // x19
            v2 -> (PhysicalReg.calleeSaved(1): Location)  // x20
        )
        
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        // Prologue: stp x19, x20, [sp, #-16]!
        val pushes = instrs.collect { 
            case s @ ArmStorePair(Size.X, r1, _, _) 
                if r1 == PhysicalReg.calleeSaved(0) => s 
        }
        pushes should not be empty
        
        // Epilogue: ldp x19, x20, [sp], #16
        val pops = instrs.collect { 
            case l @ ArmLoadPair(Size.X, r1, _, _) 
                if r1 == PhysicalReg.calleeSaved(0) => l 
        }
        pops should not be empty
    }

    it should "reserve stack space for locals using sub sp" in {
        val funcDef = IR.FunctionDef("localFunc", List.empty, List.empty)
        
        // Force a stack slot at -24 (allocator puts locals at negative offsets)
        val v1 = IR.VirtualReg(1, Size.W)
        val alloc = Map(v1 -> (StackSlot(-24): Location))
        
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, alloc, ctx)

        // Should be sub sp, sp, #32 (because aligned to 16 bytes)
        val subs = instrs.collect { 
            case ArmSubImm(
                Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer, ArmImmediate(32)
            ) => true
        }
        subs should not be empty
    }

    /* ==================== Negative Offset (LDUR/STUR) Tests ==================== */

    "CodeGenerator load/store" should "generate LDUR for negative stack offsets" in {
        val dest = IR.VirtualReg(0, Size.W)
        // Stack slot at -8 (within -256 range)
        val alloc = Map(dest -> (StackSlot(-8): Location)) 
        
        // generate: Mov(x0, v0) -> Load v0 into scratch, move scratch to x0
        val result = programWith(
            List(IR.Mov(IR.VirtualReg(1, Size.W), dest)), // v1 = v0(stack)
            alloc + (IR.VirtualReg(1, Size.W) -> PhysicalReg.argRegisters(0))
        )

        // Expect ArmLdur because offset is -8
        val ldurs = result.body.collect { case i: ArmLdur => i }
        ldurs should not be empty
        ldurs.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8))
    }

    it should "generate STUR for negative stack offsets" in {
        val dest = IR.VirtualReg(0, Size.W)
        val alloc = Map(dest -> (StackSlot(-16): Location))
        
        // generate: Mov(v0, 10)
        val result = programWith(List(IR.Mov(dest, IR.Immediate(10))), alloc)

        // Expect ArmStur because offset is -16
        val sturs = result.body.collect { case i: ArmStur => i }
        sturs should not be empty
        sturs.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-16))
    }

    it should "handle large stack offsets by materializing offset in scratch register" in {
        val dest = IR.VirtualReg(0, Size.W)
        // Large negative offset outside -256 range
        val largeOffset = -1024
        val alloc = Map(dest -> (StackSlot(largeOffset): Location))
        
        val result = programWith(List(IR.Mov(dest, IR.Immediate(10))), alloc)

        // -1024 in hex is FFFFFC00
        // Lower 16 bits = 64512 (0xFC00), upper 16 bits = 65535 (0xFFFF)
        
        val movz = result.body.collect { 
            case ArmMovz(Size.W, reg, ArmImmediate(64512)) 
                if reg == PhysicalReg.ipcScratch(1) => true 
        }
        movz should not be empty

        val movk = result.body.collect { 
            case ArmMovk(Size.W, reg, ArmImmediate(65535), shift) 
                if reg == PhysicalReg.ipcScratch(1) => true 
        }
        movk should not be empty

        val stores = result.body.collect { 
            case ArmStore(_, _, ArmAddrRegOffset(PhysicalReg.framePointer, offsetReg))
                if offsetReg == PhysicalReg.ipcScratch(1) => true
        }
        stores should not be empty
    }

    /* ========================= Function Tests ========================= */

    "CodeGenerator for functions" should "allocate an aligned stack block for > 8 arguments" in {
        val args = List.fill(9)(IR.Immediate(1))
        val result = programWith(List(IR.Call(IR.Label("getNinth"), args, None)), Map.empty)

        /* Get instr where the stack pointer is moved */
        val stackAllocInstr = result.body.collectFirst {
            case ArmSubImm(Size.X, dest, src, ArmImmediate(16)) 
                if dest == PhysicalReg.stackPointer && src == PhysicalReg.stackPointer => true
        }
        stackAllocInstr shouldBe Some(true)

        /* Stack pointer points to the 9th arg i.e. 0 offset */
        val stackStore = result.body.collectFirst {
            case ArmStore(Size.W, _, ArmAddrImmOffset(PhysicalReg.stackPointer, ArmImmediate(0))) => true
        }

        stackStore shouldBe Some(true)
    }

    it should "clean up the stack block after the function returns" in {

        /* 3 spilled stack args that need to be aligned i.e. 32 bytes allocated */
        val args = List.fill(11)(IR.Immediate(1))
        val result = programWith(List(IR.Call(IR.Label("manyArgs"), args, None)), Map.empty)

        val branchLinkIndex = result.body.indexWhere {
            case _: ArmBl => true
            case _ => false
        }

        /* Post BL we restore the stack pointer */
        val afterBl = result.body.drop(branchLinkIndex + 1)
        val stackCleanup = afterBl.collectFirst {
            case ArmAddImm(Size.X, dest, src, ArmImmediate(32)) 
                if dest == PhysicalReg.stackPointer => true
        }
        
        stackCleanup shouldBe Some(true)
    }

    it should "move the return value from w0/x0 into the specified destination" in {
        val destVReg = IR.VirtualReg(0, Size.W)
        val alloc = Map(destVReg -> (PhysicalReg.calleeSaved(0): Location)) 

        val result = programWith(List(IR.Call(IR.Label("getVal"), List.empty, Some(destVReg))), alloc)

        val branchLinkIndex = result.body.indexWhere {
            case _: ArmBl => true
            case _ => false
        }

        /* Must be a Mov after the BL function call to move the return value to
           the desired callee reg location */
        val afterBl = result.body.drop(branchLinkIndex + 1)
        val captureMov = afterBl.collectFirst {
            case ArmMovReg(Size.W, dest, src) 
                if dest == PhysicalReg.calleeSaved(0) && src == PhysicalReg.argRegisters(0) => true
        }

        captureMov shouldBe Some(true)
    }

    /* ========================= Pair Tests ========================= */

    "CodeGenerator for pairs" should "generate bl _malloc for HeapAlloc" in {
        val dest = IR.VirtualReg(0, Size.X)
        val allocMap = Map[IR.VirtualReg, Location](dest -> PhysicalReg.argRegisters(1)) // Put dest in x1
        
        // generate HeapAlloc(v0, 16)
        val result = programWith(
            List(IR.HeapAlloc(dest, IR.Immediate(16))),
            allocMap
        )

        // check we load 16 into x0
        val movs = result.body.collect { case m: ArmMovz => m }
        movs.exists(m => m.dest == PhysicalReg.argRegisters(0) && m.imm == ArmImmediate(16)) shouldBe true

        // check we branch to _malloc
        val bls = result.body.collect { case b: ArmBl => b }
        bls.exists(_.label.name == "_malloc") shouldBe true
    }

    it should "generate cmp and b.eq to _errNull for NullCheck" in {
        val ptr = IR.VirtualReg(0, Size.X)
        val allocMap = Map[IR.VirtualReg, Location](ptr -> PhysicalReg.argRegisters(0)) // ptr in x0
        
        val result = programWith(List(IR.NullCheck(ptr)), allocMap)

        // should compare against 0
        val cmps = result.body.collect { case c: ArmCmpImm => c }
        cmps should not be empty
        cmps.head.src2 shouldBe ArmImmediate(0)

        // Should branch conditionally (EQ) to _errNull
        val branches = result.body.collect { case b: ArmBranchCond => b }
        branches should not be empty
        branches.head.cond shouldBe Condition.EQ
        branches.head.label.name shouldBe "_errNull"
    }

    it should "generate correct memory offsets for PairLoad" in {
        val ptr = IR.VirtualReg(0, Size.X)
        val dest = IR.VirtualReg(1, Size.X)
        val allocMap = Map[IR.VirtualReg, Location](
            ptr -> PhysicalReg.argRegisters(0),
            dest -> PhysicalReg.argRegisters(1)
        )
        
        // generate: PairLoad(ptr, Snd, dest)
        val result = programWith(List(IR.PairLoad(ptr, PairPos.Snd, dest)), allocMap)

        // Expect a Load using the scratch register with an 8-byte offset for Snd
        val loads = result.body.collect { case l: ArmLoad => l }
        loads should not be empty
        
        // Check that the offset is #8
        val memoryAddr = (loads.last.src: @unchecked) match {
            case x: ArmAddrImmOffset => x
        }
        memoryAddr.offset shouldBe ArmImmediate(8)
    }

    /* ========================= Free Tests ========================= */
    "The code generator for free statements" should "generate a branch to _freepair for pairs" in {
        val ptrReg = IR.VirtualReg(0, Size.X)
        val freeInstr = IR.Free(ptrReg, FreeType.Pair)

        val allocMap = Map[IR.VirtualReg, Location](
            ptrReg -> PhysicalReg.argRegisters(0)
        )
        
        // 2. Create a minimal RegProgram with just our Free instruction
        val armProg = programWith(List(freeInstr), allocMap)
        
        // should emit a branch to _freepair
        val branchInstrs = armProg.body.collect { case ArmBl(label) => label.name }
        branchInstrs should contain ("_freepair")
    }

    it should "generate a branch to _freearray for arrays" in {
        val ptrReg = IR.VirtualReg(0, Size.X)
        val freeInstr = IR.Free(ptrReg, FreeType.Array)

        val allocMap = Map[IR.VirtualReg, Location](
            ptrReg -> PhysicalReg.argRegisters(0)
        )
        
        val armProg = programWith(List(freeInstr), allocMap)
        
        val branchInstrs = armProg.body.collect { case ArmBl(label) => label.name }
        branchInstrs should contain ("_freearray")
    }
}
