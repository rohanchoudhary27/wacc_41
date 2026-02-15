package wacc.backend

import scala.collection.mutable.ListBuffer

/**
  * State class managing code generation within a single function or program body.
  * Tracks the allocation map, generated instructions, and provides fresh label IDs.
  *
  * @param alloc the register allocation map (VirtualReg -> Location)
  */
class GenState(val alloc: Map[VirtualReg, Location]) {
    private var nextLabel: Int = 0
    private val instrs = ListBuffer[ArmInstruction]()
    
    /** Get the final instruction list */
    def finalInstrs: List[ArmInstruction] = instrs.toList
    
    /** Add an instruction to the output */
    def +=(instr: ArmInstruction): Unit = instrs += instr
    
    /** Generate a fresh label ID */
    def fresh(): Int = {
        val i = nextLabel
        nextLabel += 1
        i
    }
}

/**
  * Code generator for translating High-Level IR to Low-Level IR.
  * 
  * Register allocation happens externally and is passed in via the alloc parameter.
  */
object CodeGenerator {
    
    /** Widget management */
    private val dispatcher = RuntimeLibraryDispatcher()
    
    /** Mark a widget as used (will be included in final assembly) */
    def useWidget(widget: Widget): Unit = dispatcher.use(widget)
    
    /** Get all collected widget assembly */
    def getWidgetAssembly: String = dispatcher.getCollectedAssembly
    
    /**
      * Generate Low-Level IR for the main program body.
      * 
      * @param instrs the high-level IR instruction list
      * @param alloc the register allocation map
      * @return the low-level ARM instruction list
      */
    def generate(instrs: List[Instruction], alloc: Map[VirtualReg, Location]): List[ArmInstruction] = {
        given st: GenState = GenState(alloc)
        // TODO: implementation
        st.finalInstrs
    }
    
    /**
      * Generate Low-Level IR for a function definition.
      * 
      * @param func the function definition
      * @param alloc the register allocation map for this function
      * @return the low-level ARM instruction list
      */
    def generateFunc(func: FunctionDef, alloc: Map[VirtualReg, Location]): List[ArmInstruction] = {
        given st: GenState = GenState(alloc)
        // TODO: implementation
        st.finalInstrs
    }
    
    /**
      * Generate a fresh compiler-generated label with given prefix.
      * 
      * This helper is only for generating unique labels during code generation - for fixed 
      * labels (e.g., widget labels, function names), use `ArmAddrLabel` directly.
      *
      * @param prefix the label prefix (default ".L")
      * @return a fresh label
      */
    private def label(prefix: String = ".L")(using st: GenState): ArmAddrLabel = 
        ArmAddrLabel(s"${prefix}_${st.fresh()}")
    
    /** Emit a single instruction to the output */
    private def emit(instr: ArmInstruction)(using st: GenState): Unit = 
        st += instr
    
    /** Look up the location of a virtual register */
    private def locOf(vReg: VirtualReg)(using st: GenState): Location = 
        st.alloc(vReg)
}
