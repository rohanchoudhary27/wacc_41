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
    val argRegisters = genRegList(0, 7, allocatable = true)

    /* 3rd scratch register for comlext expressions */
    val mathScratch = new PhysicalReg(8, allocatable = false)

    /* Caller saved general purpose registers */
    val callerSaved = genRegList(9, 15, allocatable = true)
    
    /* Scratch registers */
    val ipcScratch = genRegList(16, 17, allocatable = false) 
    
    /* Callee saved general purpose registers */
    val calleeSaved = genRegList(19, 28, allocatable = true)

    /* Stack and function linking registers */
    val framePointer = new PhysicalReg(29, allocatable = false)
    val linkReg = new PhysicalReg(30, allocatable = false)
    val stackPointer = new PhysicalReg(31, allocatable = false)

    /**
      * Retrieves the allocatable registers
      *
      * @return A list of `PhysicalReg` objects which are allocatable
      */
    def getAllocatable: List[PhysicalReg] = {
        calleeSaved
    }
}