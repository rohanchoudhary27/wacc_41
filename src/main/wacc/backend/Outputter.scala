package wacc.backend

/**
  * Outputter for generating final ARM64 assembly from the low-level IR.
  */
object Outputter {
    
    /** Indentation for assembly instructions */
    private val INDENT = "\t"
    
    /**
      * Generate complete assembly output from an ARM program.
      * 
      * @param prog the ARM program containing data directives and instructions
      * @return the complete assembly source as a string
      */
    def emit(prog: ArmProgram): String = {
        val sb = StringBuilder()

        /* Data section */
        if (prog.data.nonEmpty) {
            sb ++= ".data\n"
            sb ++= ".align 4\n"
            prog.data.foreach(d => sb ++= emitDirective(d))
            sb ++= "\n"
        }
        
        /* GNU assembler directives and code section */
        sb ++= ".text\n"
        sb ++= ".global main\n"
        prog.body.foreach(i => sb ++= emitInstruction(i))
        
        /* Runtime library widgets */
        sb ++= "\n// Runtime Library\n"
        sb ++= CodeGenerator.getWidgetAssembly
        
        sb.toString
    }

    /**
      * Emit a data directive as assembly text.
      * 
      * @param dir the data directive to emit
      * @return the assembly string for this directive
      */
    private def emitDirective(dir: ArmDataDirective): String = {
        s"${INDENT}.word ${dir.storedValue.length}\n" +
        s"${dir.label.name}:\n" +
        s"${INDENT}.asciz \"${dir.storedValue}\"\n" +
        ".align 4\n"
    }
    
    /**
      * Emit a single ARM instruction as assembly text.
      * 
      * @param instr the instruction to emit
      * @return the assembly string for this instruction
      */
    private def emitInstruction(instr: ArmInstruction): String = instr match {
        /* Arithmetic */
        case ArmAddReg(size, dest, src1, src2) => emitBinary("add", size, dest, src1, src2)
        case ArmAddImm(size, dest, src1, imm) => emitBinary("add", size, dest, src1, imm)
        case ArmSubReg(size, dest, src1, src2) => emitBinary("sub", size, dest, src1, src2)
        case ArmSubImm(size, dest, src1, imm) => emitBinary("sub", size, dest, src1, imm)
        case ArmMulReg(size, dest, src1, src2) => ???
        case ArmDivReg(size, dest, src1, src2) => ???
        case ArmNeg(size, reg) => ???
        
        /* Logical */
        case ArmAndReg(size, dest, src1, src2) => ???
        case ArmAndImm(size, dest, src1, imm) => ???
        case ArmOrReg(size, dest, src1, src2) => ???
        case ArmOrImm(size, dest, src1, imm) => ???
        case ArmNotReg(size, dest, src) => ???
        
        /* Shifts */
        case ArmLslReg(size, dest, src1, src2) => ???
        case ArmLslImm(size, dest, src1, imm) => ???
        case ArmLsrReg(size, dest, src1, src2) => ???
        case ArmLsrImm(size, dest, src1, imm) => ???
        case ArmAsrReg(size, dest, src1, src2) => ???
        case ArmAsrImm(size, dest, src1, imm) => ???
        
        /* Data Movement */
        case ArmMovReg(size, dest, src) => emitUnary("mov", size, dest, src)
        case ArmMovImm(size, dest, imm) => 
            indented(s"mov ${formatReg(dest, size)}, ${formatImmediate(imm)}")
        case ArmMovKeep(size, dest, imm, shift) => ???
        case ArmLoad(size, dest, addr) => emitMemOp("ldr", size, dest, addr)
        case ArmStore(size, src, addr) => emitMemOp("str", size, src, addr)
        case ArmLea(dest, label) => emitLea(dest, label)
        
        /* Comparison */
        case ArmCmpReg(size, src1, src2) => emitCompare(size, src1, src2)
        case ArmCmpImm(size, src1, imm) => emitCompare(size, src1, imm)
        
        /* Control Flow */
        case ArmBranchUncond(label) => indented(s"b ${label.name}")
        case ArmBranchCond(cond, label) => indented(s"b${formatCondition(cond)} ${label.name}")
        case ArmLabelDef(label) => s"${label.name}:\n"
        case ArmBl(label) => indented(s"bl ${label.name}")
        case ArmRet => indented("ret")
        
        /* Stack Operations */
        case ArmStorePair(size, src1, src2, addr) => emitStorePair(size, src1, src2, addr)

        case ArmLoadPair(size, dest1, dest2, addr) => emitLoadPair(size, dest1, dest2, addr)        
        /* Special Instructions */
        case ArmMSub(size, dest, mult1, mult2, sub) => ???
        case ArmCSet(size, dest, cond) => ???
        case ArmSMull(dest, src1, src2) => ???
        case ArmSxtw(dest, src) => ???
        case ArmCbz(size, reg, label) => ???
        case ArmCbnz(size, reg, label) => ???
    }

    /** Format a store pair instruction */
    private def emitStorePair(size: Size,
                             src1: PhysicalReg,
                             src2: PhysicalReg,
                             addr: ArmAddress) =
        indented(s"stp ${formatReg(src1, size)}, ${formatReg(src2, size)}, ${{formatAddress(addr)}}")


    /** Format a load pair instruction */
    private def emitLoadPair(size: Size,
                             dest1: PhysicalReg,
                             dest2: PhysicalReg,
                             addr: ArmAddress) =
        indented(s"ldp ${formatReg(dest1, size)}, ${formatReg(dest2, size)}, ${{formatAddress(addr)}}")

    /** Emit a binary operation: op dest, src1, src2 */
    private def emitBinary(op: String, size: Size, dest: PhysicalReg, src1: PhysicalReg, 
                          src2: PhysicalReg | ArmImmediate): String = {
        val src2Str = src2 match {
            case reg: PhysicalReg => formatReg(reg, size)
            case imm: ArmImmediate => formatImmediate(imm)
        }
        indented(s"${op} ${formatReg(dest, size)}, ${formatReg(src1, size)}, ${src2Str}")
    }
    
    /** Emit a unary operation: op dest, src */
    private def emitUnary(op: String, size: Size, dest: PhysicalReg, src: PhysicalReg): String =
        indented(s"${op} ${formatReg(dest, size)}, ${formatReg(src, size)}")
    
    /** Emit a comparison: cmp src1, src2 */
    private def emitCompare(size: Size, src1: PhysicalReg, 
                           src2: PhysicalReg | ArmImmediate): String = {
        val src2Str = src2 match {
            case reg: PhysicalReg => formatReg(reg, size)
            case imm: ArmImmediate => formatImmediate(imm)
        }
        indented(s"cmp ${formatReg(src1, size)}, ${src2Str}")
    }
    
    /** Emit a load/store: op reg, [addr]  s*/
    private def emitMemOp(op: String, size: Size, reg: PhysicalReg, addr: ArmAddress): String =
        indented(s"${op} ${formatReg(reg, size)}, ${formatAddress(addr)}")

    /** Emit a Lea (load effective address) instruction */
    private def emitLea(reg: PhysicalReg, label: ArmAddress): String = {
        indented(s"adrp ${formatReg(reg, Size.X)}, ${formatAddress(label)}") +
        indented(s"add ${formatReg(reg, Size.X)}, ${formatReg(reg, Size.X)}, :lo12:${{formatAddress(label)}}")
    }

    /* ========== Formatting Helpers ========== */
    
    /** Wrap an instruction string with indentation and newline */
    private def indented(s: String): String = s"${INDENT}${s}\n"
    
    /** Format a register with the appropriate size prefix */
    private def formatReg(reg: PhysicalReg, size: Size): String = {
        reg.index match {
            case 31 => if (size == Size.X) then "sp" else "wsp"
            case 30 => "lr"
            case 29 => "fp"
            case _ => size match {
                case Size.X => s"x${reg.index}"
                case _ => s"w${reg.index}"
            }
        }
    }

    /** Format an immediate value */
    private def formatImmediate(imm: ArmImmediate): String = s"#${imm.value}"
    
    /** Format an address operand */
    private def formatAddress(addr: ArmAddress): String = addr match {
        case ArmAddrRegOffset(base, offset) => 
            s"[${formatReg(base, Size.X)}, ${formatReg(offset, Size.X)}]"

        case ArmAddrImmOffset(base, imm) => 
            s"[${formatReg(base, Size.X)}, ${formatImmediate(imm)}]"

        case ArmAddrRegShift(base, offset, shift) => 
            s"[${formatReg(base, Size.X)}, ${formatReg(offset, Size.X)}, " +
            s"lsl #${ShiftAmounts.value(shift)}]"

        case ArmAddrPostIndex(baseAddr, immOffset) => 
            s"[${formatReg(baseAddr, Size.X)}], ${{formatImmediate(immOffset)}}"

        case ArmAddrPreIndex(baseAddr, immOffset) => 
            s"[${formatReg(baseAddr, Size.X)}, ${{formatImmediate(immOffset)}}]!"
            
        case ArmAddrLabel(name) => 
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
    }
}
