package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*

/**
  * Unit tests for the Low-Level IR types
  */
class LowLevelIRTest extends AnyFlatSpec with Matchers {

    /* ==================== Address Mode Tests ==================== */

    "Arm.AddrPreIndex" should "store base register and offset" in {
        val addr = Arm.AddrPreIndex(PhysicalReg.stackPointer,
                                    Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        addr.baseAddr shouldBe PhysicalReg.stackPointer
        addr.offset shouldBe Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
    }

    it should "be an Arm.Address" in {
        val addr: Arm.Address = Arm.AddrPreIndex(PhysicalReg.stackPointer,
                                                 Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        addr shouldBe a [Arm.AddrPreIndex]
    }

    "Arm.AddrPostIndex" should "store base register and offset" in {
        val addr = Arm.AddrPostIndex(PhysicalReg.stackPointer, Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        addr.baseAddr shouldBe PhysicalReg.stackPointer
        addr.offset shouldBe Arm.Immediate(16)
    }

    it should "be an Arm.Address" in {
        val addr: Arm.Address = Arm.AddrPostIndex(PhysicalReg.stackPointer,
                                                  Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        addr shouldBe a [Arm.AddrPostIndex]
    }

    /* ==================== Store/Load Pair Tests ==================== */

    "Arm.StorePair" should "store size, two source registers, and address" in {
        val instr = Arm.StorePair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            Arm.AddrPreIndex(PhysicalReg.stackPointer, Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        )
        instr.size shouldBe Size.X
        instr.src1 shouldBe PhysicalReg.framePointer
        instr.src2 shouldBe PhysicalReg.linkReg
        instr.addr shouldBe Arm.AddrPreIndex(PhysicalReg.stackPointer,
                                             Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
    }

    it should "be an Arm.Instruction" in {
        val instr: Arm.Instruction = Arm.StorePair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            Arm.AddrPreIndex(PhysicalReg.stackPointer,
                             Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        )
        instr shouldBe a [Arm.StorePair]
    }

    "Arm.LoadPair" should "store size, two destination registers, and address" in {
        val instr = Arm.LoadPair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            Arm.AddrPostIndex(PhysicalReg.stackPointer, Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        )
        instr.size shouldBe Size.X
        instr.dest1 shouldBe PhysicalReg.framePointer
        instr.dest2 shouldBe PhysicalReg.linkReg
        instr.addr shouldBe Arm.AddrPostIndex(PhysicalReg.stackPointer,
                                              Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
    }

    it should "be an Arm.Instruction" in {
        val instr: Arm.Instruction = Arm.LoadPair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            Arm.AddrPostIndex(PhysicalReg.stackPointer,
                              Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
        )
        instr shouldBe a [Arm.LoadPair]
    }

    /* ==================== Equality Tests ==================== */

    "Arm.StorePair" should "equal another with the same fields" in {
        val a = Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPreIndex(PhysicalReg.stackPointer,
                             Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        val b = Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPreIndex(PhysicalReg.stackPointer,
                             Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        a shouldBe b
    }

    it should "not equal one with different address" in {
        val a = Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPreIndex(PhysicalReg.stackPointer,
                             Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        val b = Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPostIndex(PhysicalReg.stackPointer,
                              Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        a should not be b
    }

    "Arm.LoadPair" should "equal another with the same fields" in {
        val a = Arm.LoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPostIndex(PhysicalReg.stackPointer,
                              Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        val b = Arm.LoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            Arm.AddrPostIndex(PhysicalReg.stackPointer,
                              Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        a shouldBe b
    }

    /* ==================== Arm.Program Tests ==================== */

    "Arm.Program" should "hold data directives and body instructions" in {
        val dir = Arm.DataDirective(Arm.AddrLabel(".msg_0"), "hello")
        val body = List(
            Arm.LabelDef(Arm.AddrLabel(LabelNameFormatter.mainLabel)),
            Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                Arm.AddrPreIndex(PhysicalReg.stackPointer,
                                 Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            Arm.Ret
        )
        val prog = Arm.Program(List(dir), body)
        prog.data should have length 1
        prog.body should have length 3
    }
}
