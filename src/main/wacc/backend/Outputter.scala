package wacc.backend

import wacc.backend.Arm
import java.io.BufferedWriter

/** Assembly-level string constants used when emitting the final `.s` file */
private object AsmDirectives {

    /* ======================== Section headers ========================  */

    /** Marks the start of the data section. */
    val DataSection: String = ".data\n"

    /** Marks the start of the text (code) section. */
    val TextSection: String = ".text\n"

    /** Marks the `main` symbol as globally visible. */
    val GlobalMain: String = ".global main\n"

    /** Aligns the next item to a 4-byte boundary (word alignment). */
    val Align4: String = s".align ${Constants.ALIGN_WORD}\n"

    /* ======================== Data directive keywords ======================== */

    /**
      * Assembler keyword for a 32-bit word value.
      * Used to emit the length header that precedes every WACC string in
      * the data section.
      */
    val WordKeyword: String = ".word"

    /**
      * Assembler keyword for a null terminated ASCII string.
      * Used to emit string literal payloads in the data section.
      */
    val AscizKeyword: String = ".asciz"
}

/**
  * Outputter for generating final ARM64 assembly from the low-level IR.
  * 
  * Translates an `Arm.Program` to the textual assembly format expected by the
  * GNU assembler (`aarch64-linux-gnu-gcc`). 
  */
object Outputter {
    
    /** Indentation for assembly instructions */
    private val INDENT = "\t"
    
    /**
      * Write complete assembly output from an ARM program to a BufferedWriter.
      * 
      * @param prog the ARM program containing data directives and instructions
      * @param bw the buffered writer to write to
      */
    def emit(prog: Arm.Program, bw: BufferedWriter): Unit = {

        /* Data section */
        if (prog.data.nonEmpty) {
            bw.write(AsmDirectives.DataSection)
            bw.write(AsmDirectives.Align4)
            prog.data.foreach(d => emitDirective(d, bw))
            bw.write("\n")
        }
        
        /* GNU assembler directives and code section */
        bw.write(AsmDirectives.TextSection)
        bw.write(AsmDirectives.GlobalMain)
        prog.body.foreach(i => emitInstruction(i, bw))
    }

    /**
      * Emit a data directive as assembly text.
      * 
      * @param dir the data directive to emit
      * @param bw the buffered writer to write to
      */
    private def emitDirective(dir: Arm.DataDirective, bw: BufferedWriter): Unit = {
        bw.write(s"${INDENT}${AsmDirectives.WordKeyword} ${dir.storedValue.length}\n")
        bw.write(s"${dir.label.name}:\n")
        bw.write(s"${INDENT}${AsmDirectives.AscizKeyword} \"${dir.storedValue}\"\n")
        bw.write(s"${AsmDirectives.Align4}\n")
    }
    
    /**
      * Emit a single ARM instruction as assembly text.
      * 
      * @param instr the instruction to emit
      * @param bw the buffered writer to write to
      */
    private def emitInstruction(instr: Arm.Instruction, bw: BufferedWriter): Unit = {
        bw.write(formatInstruction(instr))
    }

    /**
      * Format a single ARM instruction as an assembly string.
      * 
      * @param instr the instruction to format
      * @return the assembly string for this instruction
      */
    private def formatInstruction(instr: Arm.Instruction): String = instr match {
        /* Arithmetic */
        case Arm.AddReg(size, dest, src1, src2) => emitBinary("add", size, dest, src1, src2)
        case Arm.AddImm(size, dest, src1, imm) => emitBinary("add", size, dest, src1, imm)
        case Arm.AddsReg(size, dest, src1, src2) => emitBinary("adds", size, dest, src1, src2)
        case Arm.AddsImm(size, dest, src1, src2) => emitBinary("adds", size, dest, src1, src2)

        case Arm.SubReg(size, dest, src1, src2) => emitBinary("sub", size, dest, src1, src2)
        case Arm.SubImm(size, dest, src1, imm) => emitBinary("sub", size, dest, src1, imm)
        case Arm.SubsReg(size, dest, src1, imm) => emitBinary("subs", size, dest, src1, imm)
        case Arm.SubsImm(size, dest, src1, imm) => emitBinary("subs", size, dest, src1, imm)

        case Arm.MulReg(size, dest, src1, src2) => emitBinary("mul", size, dest, src1, src2)
        case Arm.DivReg(size, dest, src1, src2) => emitBinary("sdiv", size, dest, src1, src2)
        case Arm.Neg(size, dest, src) => emitUnary("neg", size, dest, src)
        case Arm.Negs(size, dest, src) => emitUnary("negs", size, dest, src)
        
        /* Logical */
        case Arm.AndReg(size, dest, src1, src2) => emitBinary("and", size, dest, src1, src2)
        case Arm.AndImm(size, dest, src1, imm) => emitBinary("and", size, dest, src1, imm)
        case Arm.OrReg(size, dest, src1, src2) => emitBinary("orr", size, dest, src1, src2)
        case Arm.OrImm(size, dest, src1, imm) => emitBinary("orr", size, dest, src1, imm)
        
        /* Shifts */
        case Arm.LslReg(size, dest, src1, src2) => emitBinary("lsl", size, dest, src1, src2)
        case Arm.LslImm(size, dest, src1, imm) => emitBinary("lsl", size, dest, src1, imm)
        case Arm.LsrReg(size, dest, src1, src2) => emitBinary("lsr", size, dest, src1, src2)
        case Arm.LsrImm(size, dest, src1, imm) => emitBinary("lsr", size, dest, src1, imm)
        case Arm.AsrReg(size, dest, src1, src2) => emitBinary("asr", size, dest, src1, src2)
        case Arm.AsrImm(size, dest, src1, imm) => emitBinary("asr", size, dest, src1, imm)
        
        /* Data Movement */
        case Arm.MovReg(size, dest, src) => emitUnary("mov", size, dest, src)
        case Arm.Movz(size, dest, imm) => 
            indented(s"movz ${formatReg(dest, size)}, ${formatImmediate(imm)}")
        case Arm.Movk(size, dest, imm, shift) => 
            indented(s"movk ${formatReg(dest, size)}, ${formatImmediate(imm)}, lsl #${ShiftAmounts.value(shift)}")

        case Arm.Load(size, dest, addr) => emitMemOp("ldr", size, dest, addr)
        case Arm.Store(size, src, addr) => emitMemOp("str", size, src, addr)
        case Arm.Ldur(size, dest, addr) => emitMemOp("ldur", size, dest, addr)
        case Arm.Stur(size, src, addr) => emitMemOp("stur", size, src, addr)
        case Arm.LoadByte(size, dest, addr) => emitMemOp("ldrb", size, dest, addr)
        case Arm.StoreByte(size, src, addr) => emitMemOp("strb", size, src, addr)
        case Arm.Lea(dest, label) => emitLea(dest, label)
        
        /* Comparison */
        case Arm.CmpReg(size, src1, src2) => emitCompare(size, src1, src2)
        case Arm.CmpImm(size, src1, imm) => emitCompare(size, src1, imm)
        case Arm.CSet(size, dest, cond) => 
            indented(s"cset ${formatReg(dest, size)}, ${formatCondition(cond)}")
        case Arm.Csel(size, dest, src1, src2, cond) =>
            val d = formatReg(dest, size)
            val s1 = formatReg(src1, size)
            val s2 = formatReg(src2, size)
            val c = formatCondition(cond)
            indented(s"csel $d, $s1, $s2, $c")
        
        /* Control Flow */
        case Arm.BranchUncond(label) => indented(s"b ${label.name}")
        case Arm.BranchCond(cond, label) => indented(s"b${formatCondition(cond)} ${label.name}")
        case Arm.LabelDef(label) => s"${label.name}:\n"
        case Arm.Bl(label) => indented(s"bl ${label.name}")
        case Arm.Ret => indented("ret")
        
        /* Stack Operations */
        case Arm.LoadPair(size, dest1, dest2, addr) => emitLoadPair(size, dest1, dest2, addr)
        case Arm.StorePair(size, src1, src2, addr) => emitStorePair(size, src1, src2, addr)

        /* Special Instructions */
        case Arm.MSub(size, dest, mult1, mult2, sub) =>
            val d = formatReg(dest, size)
            val m1 = formatReg(mult1, size)
            val m2 = formatReg(mult2, size)
            val s = formatReg(sub, size)
            indented(s"msub $d, $m1, $m2, $s")

        case Arm.SMull(dest, src1, src2) => 
            indented(
                s"smull ${formatReg(dest, Size.X)}, " +
                  s"${formatReg(src1, Size.W)}, " +
                  s"${formatReg(src2, Size.W)}"
                )

        case Arm.Sxtw(dest, src) => 
            indented(s"sxtw ${formatReg(dest, Size.X)}, ${formatReg(src, Size.W)}")

        case Arm.Cbz(size, reg, label) => 
            indented(s"cbz ${formatReg(reg, size)}, ${label.name}")
            
        case Arm.Cbnz(size, reg, label) => 
            indented(s"cbnz ${formatReg(reg, size)}, ${label.name}")

    }

    /** Format a store pair instruction */
    private def emitStorePair(size: Size,
                             src1: PhysicalReg,
                             src2: PhysicalReg,
                             addr: Arm.Address) =
        indented(s"stp ${formatReg(src1, size)}, ${formatReg(src2, size)}, ${formatAddress(addr)}")


    /** Format a load pair instruction */
    private def emitLoadPair(size: Size,
                             dest1: PhysicalReg,
                             dest2: PhysicalReg,
                             addr: Arm.Address) =
        indented(s"ldp ${formatReg(dest1, size)}, ${formatReg(dest2, size)}, ${formatAddress(addr)}")

    /** Emit a binary operation: op dest, src1, src2 */
    private def emitBinary(op: String, size: Size, dest: PhysicalReg, src1: PhysicalReg, 
                          src2: PhysicalReg | Arm.Immediate): String = {
        val src2Str = src2 match {
            case reg: PhysicalReg => formatReg(reg, size)
            case imm: Arm.Immediate => formatImmediate(imm)
        }
        indented(s"${op} ${formatReg(dest, size)}, ${formatReg(src1, size)}, ${src2Str}")
    }
    
    /** Emit a unary operation: op dest, src */
    private def emitUnary(op: String, size: Size, dest: PhysicalReg, src: PhysicalReg): String =
        indented(s"${op} ${formatReg(dest, size)}, ${formatReg(src, size)}")
    
    /** Emit a comparison: cmp src1, src2 */
    private def emitCompare(size: Size, src1: PhysicalReg, 
                           src2: PhysicalReg | Arm.Immediate): String = {
        val src2Str = src2 match {
            case reg: PhysicalReg => formatReg(reg, size)
            case imm: Arm.Immediate => formatImmediate(imm)
        }
        indented(s"cmp ${formatReg(src1, size)}, ${src2Str}")
    }
    
    /** Emit a load/store: op reg, [addr]  s*/
    private def emitMemOp(op: String, size: Size, reg: PhysicalReg, addr: Arm.Address): String =
        indented(s"${op} ${formatReg(reg, size)}, ${formatAddress(addr)}")

    /** Emit a Lea (load effective address) instruction */
    private def emitLea(reg: PhysicalReg, label: Arm.Address): String = {
        indented(s"adrp ${formatReg(reg, Size.X)}, ${formatAddress(label)}") +
        indented(s"add ${formatReg(reg, Size.X)}, ${formatReg(reg, Size.X)}, :lo12:${formatAddress(label)}")
    }

    /* ========== Formatting Helpers ========== */
    
    /** Wrap an instruction string with indentation and newline */
    private def indented(s: String): String = s"${INDENT}${s}\n"
    
    /** Format a register with the appropriate size prefix */
    private def formatReg(reg: PhysicalReg, size: Size): String = {
        reg.index match {
            case PhysicalReg.SP_INDEX => if (size == Size.X) "sp" else "wsp"
            case PhysicalReg.LR_INDEX => if (size == Size.X) "lr" else "w30"
            case PhysicalReg.FP_INDEX => "fp"
            case _ => size match {
                case Size.X => s"x${reg.index}"
                case _ => s"w${reg.index}"
            }
        }
    }

    /** Format an immediate value */
    private def formatImmediate(imm: Arm.Immediate): String = s"#${imm.value}"
    
    /** Format an address operand */
    private def formatAddress(addr: Arm.Address): String = addr match {
        case Arm.AddrRegOffset(base, offset) => 
            s"[${formatReg(base, Size.X)}, ${formatReg(offset, Size.X)}]"

        case Arm.AddrImmOffset(base, imm) => 
            s"[${formatReg(base, Size.X)}, ${formatImmediate(imm)}]"

        case Arm.AddrRegShift(base, offset, shift) => 
            s"[${formatReg(base, Size.X)}, ${formatReg(offset, Size.X)}, " +
            s"lsl #${ShiftAmounts.value(shift)}]"

        case Arm.AddrPostIndex(baseAddr, immOffset) => 
            s"[${formatReg(baseAddr, Size.X)}], ${formatImmediate(immOffset)}"

        case Arm.AddrPreIndex(baseAddr, immOffset) => 
            s"[${formatReg(baseAddr, Size.X)}, ${formatImmediate(immOffset)}]!"
            
        case Arm.AddrLabel(name) => 
            s"$name"
    }
    
    /** Format a condition code */
    private def formatCondition(cond: Condition): String = cond match {
        case Condition.EQ => "eq"
        case Condition.NE => "ne"
        case Condition.LT => "lt"
        case Condition.LE => "le"
        case Condition.GT => "gt"
        case Condition.GE => "ge"
        case Condition.HI => "hi"
        case Condition.VS => "vs"
    }
}
