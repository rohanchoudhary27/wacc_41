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
    private def emitInstr(instr: Arm.Instruction): String = {
        val prog = Arm.Program(List.empty, List(instr))
        Outputter.emit(prog)
    }

    /* ==================== Store Pair Tests ==================== */

    "Outputter store pair" should "emit stp with pre-index addressing for prologue" in {
        val output = emitInstr(
            Arm.StorePair(Size.X,
                PhysicalReg.framePointer,
                PhysicalReg.linkReg,
                Arm.AddrPreIndex(PhysicalReg.stackPointer, Arm.Immediate(-16)))
        )
        output should include ("stp fp, lr, [sp, #-16]!")
    }

    it should "emit ldp with post-index addressing for epilogue" in {
        val output = emitInstr(
            Arm.LoadPair(Size.X,
                PhysicalReg.framePointer,
                PhysicalReg.linkReg,
                Arm.AddrPostIndex(PhysicalReg.stackPointer, Arm.Immediate(16)))
        )
        output should include ("ldp fp, lr, [sp], #16")
    }

    it should "emit stp with immediate offset addressing" in {
        val output = emitInstr(
            Arm.StorePair(Size.X,
                PhysicalReg.returnRegister,
                PhysicalReg.argRegisters(1),
                Arm.AddrImmOffset(PhysicalReg.stackPointer, Arm.Immediate(0)))
        )
        output should include ("stp x0, x1, [sp, #0]")
    }

    it should "emit ldp with immediate offset addressing" in {
        val output = emitInstr(
            Arm.LoadPair(Size.X,
                PhysicalReg.returnRegister,
                PhysicalReg.argRegisters(1),
                Arm.AddrImmOffset(PhysicalReg.stackPointer, Arm.Immediate(0)))
        )
        output should include ("ldp x0, x1, [sp, #0]")
    }

    it should "use w-registers for 32-bit store pairs" in {
        val output = emitInstr(
            Arm.StorePair(Size.W,
                PhysicalReg.returnRegister,
                PhysicalReg.argRegisters(1),
                Arm.AddrPreIndex(PhysicalReg.stackPointer, Arm.Immediate(-8)))
        )
        output should include ("stp w0, w1, [sp, #-8]!")
    }

    it should "use w-registers for 32-bit load pairs" in {
        val output = emitInstr(
            Arm.LoadPair(Size.W,
                PhysicalReg.argRegisters(2),
                PhysicalReg.argRegisters(3),
                Arm.AddrPostIndex(PhysicalReg.stackPointer, Arm.Immediate(8)))
        )
        output should include ("ldp w2, w3, [sp], #8")
    }

    /* ==================== MovImm Tests ==================== */

    "Outputter mov imm" should "emit movz with immediate value" in {
        val output = emitInstr(
            Arm.Movz(Size.W, PhysicalReg.returnRegister, Arm.Immediate(7))
        )
        output should include ("movz w0, #7")
    }

    /* ==================== MovReg Tests ==================== */

    "Outputter mov reg" should "emit mov between registers" in {
        val output = emitInstr(
            Arm.MovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer)
        )
        output should include ("mov fp, sp")
    }

    /* ==================== Branch and Link Tests ==================== */

    "Outputter bl" should "emit bl instruction" in {
        val output = emitInstr(Arm.Bl(Arm.AddrLabel("exit")))
        output should include ("bl exit")
    }

    it should "emit ret instruction" in {
        val output = emitInstr(Arm.Ret)
        output should include ("ret")
    }

    /* ==================== Label Tests ==================== */

    "Outputter label outputting" should "emit label definition without indentation" in {
        val output = emitInstr(Arm.LabelDef(Arm.AddrLabel("main")))
        output should include ("main:")
    }

    /* ==================== Comparison Instruction Tests ==================== */

    "Outputter cmp" should "emit cmp for register-register comparison" in {
        val output = emitInstr(
            Arm.CmpReg(Size.W, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1))
        )
        output should include ("cmp w0, w1")
    }

    it should "emit cmp for register-immediate comparison" in {
        val output = emitInstr(
            Arm.CmpImm(Size.W, PhysicalReg.returnRegister, Arm.Immediate(42))
        )
        output should include ("cmp w0, #42")
    }

    /* ==================== Boolean Operator Instruction Tests ==================== */

    "Outputter boolean op" should "emit and for register-register AND" in {
        val output = emitInstr(
            Arm.AndReg(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("and w0, w1, w2")
    }

    it should "emit and for register-immediate AND" in {
        val output = emitInstr(
            Arm.AndImm(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), Arm.Immediate(1))
        )
        output should include ("and w0, w1, #1")
    }

    it should "emit orr for register-register OR" in {
        val output = emitInstr(
            Arm.OrReg(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("orr w0, w1, w2")
    }

    it should "emit orr for register-immediate OR" in {
        val output = emitInstr(
            Arm.OrImm(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), Arm.Immediate(1))
        )
        output should include ("orr w0, w1, #1")
    }

    /* ==================== Binary Operator Instruction Tests ==================== */

    "Outputter binary operations" should "emit add for register-register addition" in {
        val output = emitInstr(
            Arm.AddReg(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("add w0, w1, w2")
    }

    it should "emit add for register-immediate addition" in {
        val output = emitInstr(
            Arm.AddImm(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), Arm.Immediate(5))
        )
        output should include ("add w0, w1, #5")
    }

    it should "emit sub for register-register subtraction" in {
        val output = emitInstr(
            Arm.SubReg(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("sub w0, w1, w2")
    }

    it should "emit sub for register-immediate subtraction" in {
        val output = emitInstr(
            Arm.SubImm(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), Arm.Immediate(5))
        )
        output should include ("sub w0, w1, #5")
    }

    it should "emit mul for register-register multiplication" in {
        val output = emitInstr(
            Arm.MulReg(Size.B, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("mul w0, w1, w2")
    }

    it should "emit smull for signed multiply long (32 bit to 64 bit)" in {
        val output = emitInstr(
            Arm.SMull(PhysicalReg.mathScratch, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("smull x8, w1, w2")
    }

    it should "emit sdiv for signed division" in {
        val output = emitInstr(
            Arm.DivReg(Size.W, PhysicalReg.returnRegister, PhysicalReg.argRegisters(1), PhysicalReg.argRegisters(2))
        )
        output should include ("sdiv w0, w1, w2")
    }

    it should "emit sxtx for sign extending 32 to 64 bit" in {
        val output = emitInstr(
            Arm.Sxtw(PhysicalReg.mathScratch, PhysicalReg.returnRegister)
        )
        output should include ("sxtw x8, w0")
    }

    it should "emit msub for multiply subtract in modulo" in {
        val output = emitInstr(
            Arm.MSub(Size.W, PhysicalReg.returnRegister, 
            PhysicalReg.argRegisters(1), 
            PhysicalReg.argRegisters(2), 
            PhysicalReg.argRegisters(3))
        )
        output should include ("msub w0, w1, w2, w3")
    }

    /* ==================== Conditional Set Instruction Tests ==================== */

    "Outputter cset" should "emit cset with EQ condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.EQ)
        )
        output should include ("cset w0, eq")
    }

    it should "emit cset with NE condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.NE)
        )
        output should include ("cset w0, ne")
    }

    it should "emit cset with GT condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.GT)
        )
        output should include ("cset w0, gt")
    }

    it should "emit cset with LT condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.LT)
        )
        output should include ("cset w0, lt")
    }

    it should "emit cset with GE condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.GE)
        )
        output should include ("cset w0, ge")
    }

    it should "emit cset with LE condition" in {
        val output = emitInstr(
            Arm.CSet(Size.B, PhysicalReg.returnRegister, Condition.LE)
        )
        output should include ("cset w0, le")
    }

    it should "emit neg for integer negation" in {
        val output = emitInstr(
            Arm.Neg(Size.W, PhysicalReg.argRegisters(1), PhysicalReg.returnRegister)
        )
        output should include ("neg w1, w0")
    }

    it should "emit mov for chr identity assignment" in {
        val output = emitInstr(
            Arm.MovReg(Size.W, PhysicalReg.argRegisters(1), PhysicalReg.returnRegister)
        )
        output should include ("mov w1, w0")
    }

    it should "emit cbz for compare and branch on zero" in {
        val output = emitInstr(
            Arm.Cbz(Size.W, PhysicalReg.returnRegister, Arm.AddrLabel("_errDivZero"))
        )
        output should include ("cbz w0, _errDivZero")
    }

    it should "emit cbnz for compare and branch on non zero" in {
        val output = emitInstr(
            Arm.Cbnz(Size.X, PhysicalReg.mathScratch, Arm.AddrLabel("_errOverflow"))
        )
        output should include ("cbnz x8, _errOverflow")
    }

    /* ==================== Full Program Tests ==================== */

    "Outputter full prog tests" should "emit a complete exit 7 program" in {
        val prog = Arm.Program(
            List.empty,
            List(
                Arm.LabelDef(Arm.AddrLabel("main")),
                Arm.StorePair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                    Arm.AddrPreIndex(PhysicalReg.stackPointer, Arm.Immediate(-16))),
                Arm.MovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer),
                Arm.Movz(Size.W, PhysicalReg.returnRegister, Arm.Immediate(7)),
                Arm.Bl(Arm.AddrLabel("exit")),
                Arm.LoadPair(Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg,
                    Arm.AddrPostIndex(PhysicalReg.stackPointer, Arm.Immediate(16)))
            )
        )
        val output = Outputter.emit(prog)

        output should include (".text")
        output should include (".global main")
        output should include ("main:")
        output should include ("stp fp, lr, [sp, #-16]!")
        output should include ("mov fp, sp")
        output should include ("movz w0, #7")
        output should include ("bl exit")
        output should include ("ldp fp, lr, [sp], #16")
    }

    it should "emit data directives when present" in {
        val prog = Arm.Program(
            List(Arm.DataDirective(Arm.AddrLabel(".msg_0"), "hello")),
            List(Arm.LabelDef(Arm.AddrLabel("main")), Arm.Ret)
        )
        val output = Outputter.emit(prog)

        output should include (".data")
        output should include (".msg_0:")
        output should include (".asciz \"hello\"")
    }

    it should "not emit data section when no directives" in {
        val prog = Arm.Program(
            List.empty,
            List(Arm.LabelDef(Arm.AddrLabel("main")), Arm.Ret)
        )
        val output = Outputter.emit(prog)

        output should not include (".data")
    }

    it should "emit the full data section with the correct Chr error message" in {
        val errorMsg = "fatal error: int %d is not ascii character 0-127\n"
        
        val prog = Arm.Program(
            List(Arm.DataDirective(Arm.AddrLabel("_errBadChar"), errorMsg)),
            List(Arm.LabelDef(Arm.AddrLabel("main")), Arm.Ret)
        )

        val output = Outputter.emit(prog)

        output should include (".data")
        output should include ("_errBadChar:")
        output should include (errorMsg)
    }

    "Outputter for print" should "include the correct print widget" in {

        val prog = Arm.Program(
            List.empty,
            List(
                Arm.LabelDef(Arm.AddrLabel("main")), 
                Arm.Bl(Arm.AddrLabel("_printi")), 
                Arm.Ret
            )
        )

        val mainOutput = Outputter.emit(prog)
        val widgetOutput = PrintIntWidget.source
        val fullOutput = s"$mainOutput\n\n$widgetOutput"

        fullOutput should include ("_printi:") // check for label
        fullOutput should include ("bl printf") // check for implementation detail
    }

    "Outputter for read" should "include the correct read integer widget" in {

        val prog = Arm.Program(
            List.empty,
            List(
                Arm.LabelDef(Arm.AddrLabel("main")),
                Arm.Bl(Arm.AddrLabel("_readi")),
                Arm.Ret
            )
        )

        val mainOutput = Outputter.emit(prog)
        val widgetOutput = ReadIntegerWidget.source
        val fullOutput = s"$mainOutput\n\n$widgetOutput"

        fullOutput should include ("_readi:") // check for label
        fullOutput should include ("bl scanf") // check for implementation detail
    }

    it should "include the correct read char widget" in {

        val prog = Arm.Program(
            List.empty,
            List(
                Arm.LabelDef(Arm.AddrLabel("main")),
                Arm.Bl(Arm.AddrLabel("_readc")),
                Arm.Ret
            )
        )

        val mainOutput = Outputter.emit(prog)
        val widgetOutput = ReadCharWidget.source
        val fullOutput = s"$mainOutput\n\n$widgetOutput"

        fullOutput should include ("_readc:") // check for label
        fullOutput should include ("bl scanf") // check for implementation detail
    }

    "Outputter for ldur/stur" should "emit ldur with signed immediate offset" in {
        val output = emitInstr(
            Arm.Ldur(Size.W, PhysicalReg.returnRegister, 
                Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(-8)))
        )
        output should include ("ldur w0, [fp, #-8]")
    }

    it should "emit stur with signed immediate offset" in {
        val output = emitInstr(
            Arm.Stur(Size.X, PhysicalReg.argRegisters(1), 
                Arm.AddrImmOffset(PhysicalReg.stackPointer, Arm.Immediate(-16)))
        )
        output should include ("stur x1, [sp, #-16]")
    }
}
