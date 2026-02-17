package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*
import wacc.backend.RegisterAllocator.AllocationResult

/**
  * Unit tests for the CodeGenerator
  */
class CodeGeneratorTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal RegProgram with the given body instructions */
    private def programWith(body: List[Instruction],
                            alloc: Map[VirtualReg, Location] = Map.empty): ArmProgram = {
        val prog = RegProgram(List.empty, List.empty, body)
        val allocResult = AllocationResult(alloc, Map.empty)
        CodeGenerator.generate(prog, allocResult)
    }

    /** Scratch register 0 used by the code generator */
    private val scratch0 = PhysicalReg.ipcScratch(0)

    /* ==================== Mov Tests ==================== */

    "CodeGenerator generateMov" should "generate ArmMovImm for Mov with immediate source" in {
        val dest = VirtualReg(0, Size.W)
        val destLoc = PhysicalReg.argRegisters(0)
        val alloc = Map(dest -> (destLoc: Location))

        val result = programWith(List(Mov(dest, Immediate(42))), alloc)

        /* Filter to only the body instructions (skip prologue/epilogue) */
        val bodyInstrs = result.body.filter {
            case _: ArmLabelDef | _: ArmStorePair | _: ArmLoadPair |
                 ArmRet | ArmMovReg(Size.X, _, _) => false
            case _ => true
        }

        bodyInstrs should contain (ArmMovImm(Size.W, scratch0, ArmImmediate(42)))
    }

    it should "generate ArmMovReg when moving between registers" in {
        val src = VirtualReg(0, Size.W)
        val dest = VirtualReg(1, Size.W)
        val srcLoc = PhysicalReg.argRegisters(0)
        val destLoc = PhysicalReg.argRegisters(1)
        val alloc = Map(
            src -> (srcLoc: Location),
            dest -> (destLoc: Location)
        )

        val result = programWith(List(Mov(dest, src)), alloc)

        /* Should contain a mov from src register to dest register (possibly through scratch) */
        val movInstrs = result.body.collect {
            case m: ArmMovReg if m.size == Size.W => m
        }
        movInstrs should not be empty
    }

    it should "generate a load from stack when source is on the stack" in {
        val src = VirtualReg(0, Size.W)
        val dest = VirtualReg(1, Size.W)
        val alloc = Map(
            src -> (StackSlot(-8): Location),
            dest -> (PhysicalReg.argRegisters(0): Location)
        )

        val result = programWith(List(Mov(dest, src)), alloc)

        val loads = result.body.collect { case l: ArmLoad => l }
        loads should not be empty
        loads.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8))
    }

    it should "generate a store to stack when destination is on the stack" in {
        val dest = VirtualReg(0, Size.W)
        val alloc = Map(dest -> (StackSlot(-8): Location))

        val result = programWith(List(Mov(dest, Immediate(5))), alloc)

        val stores = result.body.collect { case s: ArmStore => s }
        stores should not be empty
        stores.head.src shouldBe ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8))
    }

    /* ==================== Call Tests ==================== */

    "CodeGenerator generateCall" should "move immediate argument directly to arg register" in {
        val result = programWith(
            List(Call(Label("exit"), List(Immediate(7)), None)),
            Map.empty
        )

        val movImms = result.body.collect {
            case m @ ArmMovImm(Size.W, reg, ArmImmediate(7)) => m
        }
        movImms should have length 1
        movImms.head.reg shouldBe PhysicalReg.argRegisters(0)
    }

    it should "emit a branch-and-link instruction to the correct label" in {
        val result = programWith(
            List(Call(Label("exit"), List(Immediate(0)), None)),
            Map.empty
        )

        result.body should contain (ArmBl(ArmAddrLabel("exit")))
    }

    it should "move virtual register argument to arg register" in {
        val argReg = VirtualReg(0, Size.W)
        val alloc = Map(argReg -> (PhysicalReg.callerSaved(0): Location))

        val result = programWith(
            List(Call(Label("myFunc"), List(argReg), None)),
            alloc
        )

        result.body should contain (ArmBl(ArmAddrLabel("myFunc")))
    }

    it should "store return value to destination register" in {
        val destReg = VirtualReg(0, Size.W)
        val destPhys = PhysicalReg.callerSaved(0)
        val alloc = Map(destReg -> (destPhys: Location))

        val result = programWith(
            List(Call(Label("getVal"), List.empty, Some(destReg))),
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
            List(Call(Label("twoArgs"), List(Immediate(1), Immediate(2)), None)),
            Map.empty
        )

        result.body should contain (ArmMovImm(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(1)))
        result.body should contain (ArmMovImm(Size.W, PhysicalReg.argRegisters(1), ArmImmediate(2)))
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
        val prog = RegProgram(
            List.empty,
            List.empty,
            List(Call(Label("exit"), List(Immediate(7)), None))
        )
        val alloc = RegisterAllocator.allocate(prog)
        val armProg = CodeGenerator.generate(prog, alloc)

        /* Should contain: main label, prologue (stp, mov fp), mov w0 #7, bl exit, epilogue (ldp) */
        armProg.body should contain (ArmLabelDef(ArmAddrLabel("main")))
        armProg.body should contain (ArmMovImm(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(7)))
        armProg.body should contain (ArmBl(ArmAddrLabel("exit")))
    }

    /* ==================== Function Definition Code Generation ==================== */

    "CodeGenerator generateFunc" should "generate label, prologue, epilogue, and ret" in {
        val funcDef = FunctionDef("myFunc", List.empty, List.empty)
        val ctx = new GlobalGenContext()
        val instrs = CodeGenerator.generateFunc(funcDef, Map.empty, ctx)

        instrs.head shouldBe ArmLabelDef(ArmAddrLabel("myFunc"))
        instrs should contain (ArmRet)

        /* Should have prologue (stp) and epilogue (ldp) */
        instrs.collect { case s: ArmStorePair => s } should not be empty
        instrs.collect { case l: ArmLoadPair => l } should not be empty
    }

    it should "include body instructions between prologue and epilogue" in {
        val bodyReg = VirtualReg(0, Size.W)
        val funcDef = FunctionDef(
            "addFunc",
            List.empty,
            List(Mov(bodyReg, Immediate(10)))
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
            case ArmMovImm(Size.W, _, ArmImmediate(10)) => true
            case _ => false
        }

        stpIdx should be >= 0
        ldpIdx should be >= 0
        movIdx should be > stpIdx
        movIdx should be < ldpIdx
    }
}
