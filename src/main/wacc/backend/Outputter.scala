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
      * @return the assembly string for this directive
      */
    private def emitInstruction(instr: ArmInstruction): String = ???
}
