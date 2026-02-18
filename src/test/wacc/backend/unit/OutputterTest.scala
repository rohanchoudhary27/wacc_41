package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*

/**
  * Unit tests for the Outputter, focusing on the store pair, load pair,
  * pre-index, and post-index assembly formatting added on this branch.
  */
class OutputterTest extends AnyFlatSpec with Matchers {

    /** Helper: emit a single-instruction program and return only the instruction line */
    private def emitInstr(instr: ArmInstruction): String = {
        val prog = ArmProgram(List.empty, List(instr))
        Outputter.emit(prog)
    }

    /* ==================== Store Pair Tests ==================== */

    "Outputter" should "emit stp with pre-index addressing for prologue" in {
        val output = emitInstr(
            ArmStorePair(Size.X,
                PhysicalReg.framePointer,
                PhysicalReg.linkReg,
                ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16)))
        )
        output should include ("stp fp, lr, [sp, #-16]!")
    }

    it should "emit ldp with post-index addressing for epilogue" in {
        val output = emitInstr(
            ArmLoadPair(Size.X,
                PhysicalReg.framePointer,
                PhysicalReg.linkReg,
                ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16)))
        )
        output should include ("ldp fp, lr, [sp], #16")
    }

    it should "emit stp with immediate offset addressing" in {
        val output = emitInstr(
            ArmStorePair(Size.X,
                PhysicalReg.argRegisters(0),
                PhysicalReg.argRegisters(1),
                ArmAddrImmOffset(PhysicalReg.stackPointer, ArmImmediate(0)))
        )
        output should include ("stp x0, x1, [sp, #0]")
    }

    it should "emit ldp with immediate offset addressing" in {
        val output = emitInstr(
            ArmLoadPair(Size.X,
                PhysicalReg.argRegisters(0),
                PhysicalReg.argRegisters(1),
                ArmAddrImmOffset(PhysicalReg.stackPointer, ArmImmediate(0)))
        )
        output should include ("ldp x0, x1, [sp, #0]")
    }

    it should "use w-registers for 32-bit store pairs" in {
        val output = emitInstr(
            ArmStorePair(Size.W,
                PhysicalReg.argRegisters(0),
                PhysicalReg.argRegisters(1),
                ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-8)))
        )
        output should include ("stp w0, w1, [sp, #-8]!")
    }

    it should "use w-registers for 32-bit load pairs" in {
        val output = emitInstr(
            ArmLoadPair(Size.W,
                PhysicalReg.argRegisters(2),
                PhysicalReg.argRegisters(3),
                ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(8)))
        )
        output should include ("ldp w2, w3, [sp], #8")
    }

    /* ==================== MovImm Tests ==================== */

    it should "emit mov with immediate value" in {
        val output = emitInstr(
            ArmMovImm(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(7))
        )
        output should include ("mov w0, #7")
    }

    it should "emit mov with negative immediate" in {
        val output = emitInstr(
            ArmMovImm(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(-1))
        )
        output should include ("mov w0, #-1")
    }

    /* ==================== MovReg Tests ==================== */

    it should "emit mov between registers" in {
        val output = emitInstr(
            ArmMovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer)
        )
        output should include ("mov fp, sp")
    }

    /* ==================== Branch and Link Tests ==================== */

    it should "emit bl instruction" in {
        val output = emitInstr(ArmBl(ArmAddrLabel("exit")))
        output should include ("bl exit")
    }

    it should "emit ret instruction" in {
        val output = emitInstr(ArmRet)
        output should include ("ret")
    }

    /* ==================== Label Tests ==================== */

    it should "emit label definition without indentation" in {
        val output = emitInstr(ArmLabelDef(ArmAddrLabel("main")))
        output should include ("main:")
    }

    /* ==================== Full Program Tests ==================== */

    it should "emit a complete exit 7 program" in {
        val prog = ArmProgram(
            List.empty,
            List(
                ArmLabelDef(ArmAddrLabel("main")),
                ArmStorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                    ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))),
                ArmMovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer),
                ArmMovImm(Size.W, PhysicalReg.argRegisters(0), ArmImmediate(7)),
                ArmBl(ArmAddrLabel("exit")),
                ArmLoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                    ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16)))
            )
        )
        val output = Outputter.emit(prog)

        output should include (".text")
        output should include (".global main")
        output should include ("main:")
        output should include ("stp fp, lr, [sp, #-16]!")
        output should include ("mov fp, sp")
        output should include ("mov w0, #7")
        output should include ("bl exit")
        output should include ("ldp fp, lr, [sp], #16")
    }

    it should "emit data directives when present" in {
        val prog = ArmProgram(
            List(ArmDataDirective(ArmAddrLabel(".msg_0"), "hello")),
            List(ArmLabelDef(ArmAddrLabel("main")), ArmRet)
        )
        val output = Outputter.emit(prog)

        output should include (".data")
        output should include (".msg_0:")
        output should include (".asciz \"hello\"")
    }

    it should "not emit data section when no directives" in {
        val prog = ArmProgram(
            List.empty,
            List(ArmLabelDef(ArmAddrLabel("main")), ArmRet)
        )
        val output = Outputter.emit(prog)

        output should not include (".data")
    }

    "Outputter for print" should "include the correct print widget" in {

        val prog = ArmProgram(
            List.empty,
            List(
                ArmLabelDef(ArmAddrLabel("main")), 
                ArmBl(ArmAddrLabel("_printi")), 
                ArmRet
            )
        )

        val mainOutput = Outputter.emit(prog)
        val widgetOutput = PrintIntWidget.source
        val fullOutput = s"$mainOutput\n\n$widgetOutput"

        fullOutput should include ("_printi:") // check for label
        fullOutput should include ("bl printf") // check for implementation detail
    }

    "Outputter for ldur/stur" should "emit ldur with signed immediate offset" in {
        val output = emitInstr(
            ArmLdur(Size.W, PhysicalReg.argRegisters(0), 
                ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(-8)))
        )
        output should include ("ldur w0, [fp, #-8]")
    }

    it should "emit stur with signed immediate offset" in {
        val output = emitInstr(
            ArmStur(Size.X, PhysicalReg.argRegisters(1), 
                ArmAddrImmOffset(PhysicalReg.stackPointer, ArmImmediate(-16)))
        )
        output should include ("stur x1, [sp, #-16]")
    }
}
