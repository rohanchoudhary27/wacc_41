package wacc.backend

import scala.collection.mutable
import wacc.backend.TAC

/**
  * The register allocator which allocates physical registers to the virtual
  * registers used in the high level IR.
  */
object RegisterAllocator {

    /**
      * Encapsulates the result of register allocation.
      *
      * @param mainAlloc allocation map for the main program body
      * @param funcAllocs allocation maps for each function, by function name
      */
    case class AllocationResult(mainAlloc: Map[TAC.VirtualReg, Location],
                                funcAllocs: Map[String, Map[TAC.VirtualReg, Location]])

    /**
      * Perform the register allocation
      *
      * @param prog the high level IR `RegProgram``
      * @return the register allocation map
      */
    def allocate(prog: TAC.RegProgram): AllocationResult = {
        val funcMaps = prog.funcs.map { f => 
            f.name -> allocateBlock(f.body, f.args) 
        }.toMap
    
        val mainMap = allocateBlock(prog.body, List.empty)
    
        AllocationResult(mainMap, funcMaps)
    }

    /**
      * Gets the offset in the stack of the next spilled argument (after)
      * using args in register 0-7. Offset is positive because we go into
      * the previous frame. We skip the first two bytes above the current frame
      * because they belong to the FP and LR. 
      *
      * @param index the virtual register index to translate to a stack offset
      * @return the stack offset to store the argument in
      */
    private def getNextArgOffset(index: Int): Int = 
        (index - AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS) * MemoryLayout.BYTES_PER_STACK_ARG + 
        AARch64Limits.ARM_STACK_ALIGNMENT_BYTES

    /**
      * Allocate registers to a block (list) of instructions
      *
      * @param instrs the instruction list to allocate registers for
      * @param args arguements to the function this block belongs to. The top level scope
      * is treated as a "main" function
      * @return the register allocation map
      */
    private def allocateBlock(instrs: List[TAC.Instruction],
                      args: List[TAC.VirtualReg]): Map[TAC.VirtualReg, Location] = {
    
        val allocationMap = mutable.Map[TAC.VirtualReg, Location]()

        /* Allocate the return register */
        allocationMap(TAC.VirtualReg(MemoryLayout.RETURN_REG_ID, Size.B)) = PhysicalReg.returnRegister
        allocationMap(TAC.VirtualReg(MemoryLayout.RETURN_REG_ID, Size.W)) = PhysicalReg.returnRegister
        allocationMap(TAC.VirtualReg(MemoryLayout.RETURN_REG_ID, Size.X)) = PhysicalReg.returnRegister
        
        /* Allocating only callee saved registers until the WACC extension */
        val freeRegs = mutable.Queue.from(PhysicalReg.getAllocatable)
        var localVarStackOffset = -MemoryLayout.BYTES_PER_STACK_ARG

        /* Allocate callee saved registers for the function parameters (the params will
           be moved into these at the start of function generation */
        args.zipWithIndex.foreach { case (arg, index) =>
            if (index < AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS) {
                if (freeRegs.nonEmpty) {
                    allocationMap(arg) = freeRegs.dequeue() 
                } else {
                    allocationMap(arg) = StackSlot(localVarStackOffset)
                    localVarStackOffset -= MemoryLayout.BYTES_PER_STACK_ARG
                }
            } else {
                allocationMap(arg) = StackSlot(getNextArgOffset(index))
            }
        }

        /**
          * Recursively populate the register allocation map
          *
          * @param obj IR structure to scan for VirtualRegs
          */
        def scan(obj: Any): Unit = obj match {
            case v: TAC.VirtualReg =>
                /* allocate only if not already assigned */
                if (!allocationMap.contains(v)) {
                    if (freeRegs.nonEmpty) {
                        allocationMap(v) = freeRegs.dequeue()
                    } else {
                        allocationMap(v) = StackSlot(localVarStackOffset)

                        /* move stack frame down */
                        localVarStackOffset -= MemoryLayout.BYTES_PER_STACK_ARG 
                    }
            }
        
            case p: Product => p.productIterator.foreach(scan)
        
            case l: Iterable[_] => l.foreach(scan)
        
            case _ =>
        }

        instrs.foreach(scan)

        allocationMap.toMap
    }
}