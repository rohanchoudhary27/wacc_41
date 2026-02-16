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
        case ArmAddReg(size, dest, src1, src2) => ???
        case ArmAddImm(size, dest, src1, imm) => ???
        case ArmSubReg(size, dest, src1, src2) => ???
        case ArmSubImm(size, dest, src1, imm) => ???
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
        case ArmMovReg(size, dest, src) => ???
        case ArmMovImm(size, dest, imm) => 
            indented(s"mov ${formatReg(dest, size)}, ${formatImmediate(imm)}")
        case ArmMovKeep(size, dest, imm, shift) => ???
        case ArmLoad(size, dest, addr) => ???
        case ArmStore(size, src, addr) => ???
        case ArmLea(dest, label) => ???
        
        /* Comparison */
        case ArmCmpReg(size, src1, src2) => ???
        case ArmCmpImm(size, src1, imm) => ???
        
        /* Control Flow */
        case ArmBranchUncond(label) => indented(s"b ${label.name}")
        case ArmBranchCond(cond, label) => indented(s"b${formatCondition(cond)} ${label.name}")
        case ArmLabelDef(label) => s"${label.name}:\n"
        case ArmBl(label) => indented(s"bl ${label.name}")
        case ArmRet => indented("ret")
        
        /* Stack Operations */
        case ArmPushTwo(size, src1, src2) => ???
        case ArmPopTwo(size, dest1, dest2) => ???
        
        /* Special Instructions */
        case ArmMSub(size, dest, mult1, mult2, sub) => ???
        case ArmCSet(size, dest, cond) => ???
        case ArmSMull(dest, src1, src2) => ???
        case ArmSxtw(dest, src) => ???
        case ArmCbz(size, reg, label) => ???
        case ArmCbnz(size, reg, label) => ???
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
