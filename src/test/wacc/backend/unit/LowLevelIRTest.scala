package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*

/**
  * Unit tests for the Low-Level IR types
  */
class LowLevelIRTest extends AnyFlatSpec with Matchers {

    /* ==================== Address Mode Tests ==================== */

    "ArmAddrPreIndex" should "store base register and offset" in {
        val addr = ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
        addr.baseAddr shouldBe PhysicalReg.stackPointer
        addr.offset shouldBe ArmImmediate(-16)
    }

    it should "be an ArmAddress" in {
        val addr: ArmAddress = ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
        addr shouldBe a [ArmAddrPreIndex]
    }

    "ArmAddrPostIndex" should "store base register and offset" in {
        val addr = ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
        addr.baseAddr shouldBe PhysicalReg.stackPointer
        addr.offset shouldBe ArmImmediate(16)
    }

    it should "be an ArmAddress" in {
        val addr: ArmAddress = ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
        addr shouldBe a [ArmAddrPostIndex]
    }

    /* ==================== Store/Load Pair Tests ==================== */

    "ArmStorePair" should "store size, two source registers, and address" in {
        val instr = ArmStorePair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
        )
        instr.size shouldBe Size.X
        instr.src1 shouldBe PhysicalReg.framePointer
        instr.src2 shouldBe PhysicalReg.linkReg
        instr.addr shouldBe ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
    }

    it should "be an ArmInstruction" in {
        val instr: ArmInstruction = ArmStorePair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
        )
        instr shouldBe a [ArmStorePair]
    }

    "ArmLoadPair" should "store size, two destination registers, and address" in {
        val instr = ArmLoadPair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
        )
        instr.size shouldBe Size.X
        instr.dest1 shouldBe PhysicalReg.framePointer
        instr.dest2 shouldBe PhysicalReg.linkReg
        instr.addr shouldBe ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
    }

    it should "be an ArmInstruction" in {
        val instr: ArmInstruction = ArmLoadPair(
            Size.X,
            PhysicalReg.framePointer,
            PhysicalReg.linkReg,
            ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))
        )
        instr shouldBe a [ArmLoadPair]
    }

    /* ==================== Equality Tests ==================== */

    "ArmStorePair" should "equal another with the same fields" in {
        val a = ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16)))
        val b = ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16)))
        a shouldBe b
    }

    it should "not equal one with different address" in {
        val a = ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16)))
        val b = ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16)))
        a should not be b
    }

    "ArmLoadPair" should "equal another with the same fields" in {
        val a = ArmLoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16)))
        val b = ArmLoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
            ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16)))
        a shouldBe b
    }

    /* ==================== ArmProgram Tests ==================== */

    "ArmProgram" should "hold data directives and body instructions" in {
        val dir = ArmDataDirective(ArmAddrLabel(".msg_0"), "hello")
        val body = List(
            ArmLabelDef(ArmAddrLabel("main")),
            ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))),
            ArmRet
        )
        val prog = ArmProgram(List(dir), body)
        prog.data should have length 1
        prog.body should have length 3
    }
}
