package wacc.backend

/* A data location (register or stack) */
sealed trait Location

/**
  * A stack location
  *
  * @param offset Offset from the frame pointer where the data is. Can be negative or positive. 
  */
case class StackSlot(offset: Int) extends Location

/**
  * A physical register in the low level IR. The constructor is private as there are only
  * a certain number of defined registers
  *
  * @param index The register number in aarch64
  * @param allocatable True if the register is allocatable by the allocator, false otherwise.
  * Scratch registers are used in the code generator for temporary computations and are thus
  * not allocatable.
  */
case class PhysicalReg private (val index: Int, val allocatable: Boolean) extends Location

/**
  * Companion object containing pre-defined physical registers and utility functions
  */
object PhysicalReg {

    private val ARG_START = 0
    private val ARG_END   = 7

    private val INDIRECT_RESULT_REG = 8
    
    private val CALLER_START = 9
    private val CALLER_END   = 15

    private val IPC_SCRATCH_1 = 16
    private val IPC_SCRATCH_2 = 17
    
    private val CALLEE_START = 19
    private val CALLEE_END   = 28

    private val FP_INDEX = 29
    private val LR_INDEX = 30
    private val SP_INDEX = 31

    /**
      * Create a list of physical registers from a range
      *
      * @param start The start register index (inclusive)
      * @param stop The stop register index (inclusive)
      * @param allocatable True fi the registers are allocatable and false if not
      * @return A list of `PhysicalReg` objects
      */
    private def genRegList(start: Int, stop: Int, allocatable: Boolean): List[PhysicalReg] = {
        val result = for (i <- start to stop) yield {
            new PhysicalReg(i, allocatable)
        }
        result.toList
    }

    /* Registers for function arguments */
    val argRegisters = genRegList(ARG_START, ARG_END, allocatable = true)

    /* 3rd scratch register for complex expressions */
    val mathScratch = new PhysicalReg(INDIRECT_RESULT_REG, allocatable = false)

    /* Caller saved general purpose registers */
    val callerSaved = genRegList(CALLER_START, CALLER_END, allocatable = true)
    
    /* Scratch registers */
    val ipcScratch = genRegList(IPC_SCRATCH_1, IPC_SCRATCH_2, allocatable = false) 
    
    /* Callee saved general purpose registers */
    val calleeSaved = genRegList(CALLEE_START, CALLEE_END, allocatable = true)

    /* Stack and function linking registers */
    val framePointer = new PhysicalReg(FP_INDEX, allocatable = false)
    val linkReg = new PhysicalReg(LR_INDEX, allocatable = false)
    val stackPointer = new PhysicalReg(SP_INDEX, allocatable = false)

    /**
      * Retrieves the allocatable registers
      *
      * @return A list of `PhysicalReg` objects which are allocatable
      */
    def getAllocatable: List[PhysicalReg] = {
        calleeSaved
    }
}