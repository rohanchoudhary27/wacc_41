package wacc.backend

import scala.collection.mutable

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
    case class AllocationResult(mainAlloc: Map[VirtualReg, Location],
                                funcAllocs: Map[String, Map[VirtualReg, Location]])

    /**
      * Perform the register allocation
      *
      * @param prog the high level IR `RegProgram``
      * @return the register allocation map
      */
    def allocate(prog: RegProgram): AllocationResult = {
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
      * because they belong to the FP and LR (16 bytes).
      *
      * @param index the virtual register index to translate to a stack offset
      * @return the stack offset to store the argument in
      */
    private def getNextArgOffset(index: Int): Int = (index - 8) * 8 + 16

    /**
      * Allocate registers to a block (list) of instructions
      *
      * @param instrs the instruction list to allocate registers for
      * @param args arguements to the function this block belongs to. The top level scope
      * is treated as a "main" function
      * @return the register allocation map
      */
    private def allocateBlock(instrs: List[Instruction],
                      args: List[VirtualReg]): Map[VirtualReg, Location] = {
    
        val allocationMap = mutable.Map[VirtualReg, Location]()

        allocationMap(VirtualReg(-1, Size.B)) = PhysicalReg.argRegisters(0)
        allocationMap(VirtualReg(-1, Size.W)) = PhysicalReg.argRegisters(0)
        allocationMap(VirtualReg(-1, Size.X)) = PhysicalReg.argRegisters(0)
        
        /* Allocate the argument registers/stack positions first */
        args.zipWithIndex.foreach { case (arg, index) =>
            if (index < 8) {
                allocationMap(arg) = PhysicalReg.argRegisters(index)
            } else {
                allocationMap(arg) = StackSlot(getNextArgOffset(index))
            }
        }

        /* Can't allocate the registers used for arguments so remove from pool */
        val freePool = PhysicalReg.getAllocatable.filterNot(p => p.index < math.min(8, args.length))
        val freeRegs = mutable.Queue.from(freePool)

        /* Growing the stack downwards for local variables. Local stack slots are assigned
           negative offsets from the FP */
        var localVarStackOffset = -8

        /**
          * Recursively populate the register allocation map
          *
          * @param obj IR structure to scan for VirtualRegs
          */
        def scan(obj: Any): Unit = obj match {
            case v: VirtualReg =>
                /* allocate only if not already assigned */
                if (!allocationMap.contains(v)) {
                    if (freeRegs.nonEmpty) {
                        allocationMap(v) = freeRegs.dequeue()
                    } else {
                        allocationMap(v) = StackSlot(localVarStackOffset)
                        localVarStackOffset -= 8 /* move stack frame down */
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