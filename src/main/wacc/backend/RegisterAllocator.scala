package wacc.backend

import scala.collection.mutable

/**
  * The register allocator which allocates physical registers to the virtual
  * registers used in the high level IR.
  */
object RegisterAllocator {

    /**
      * Represents a storage location assigned to a virtual register.
      * A location can either be a physical machine register or a stack
      * slot location relative to the frame pointer.
      */
    sealed trait Location
    case class PhysReg(name: String) extends Location
    case class StackSlot(offset: Int) extends Location

    /* List of general-purpose registers available for allocation. Reserved registers (x0-x7 
        or arguments, x8, x16-x17, x18 PR, x29 FP, x30 LR, x31 SP) are excluded. */
    val generalPurposeRegisters = List(
        "x9", "x10", "x11", "x12", "x13", "x14", "x15",
        "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"
    )

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
        
        /* Allocate the argument registers/stack positions first */
        args.zipWithIndex.foreach { case (arg, index) =>
            if (index < 8) {
                allocationMap(arg) = PhysReg(s"x$index")
            } else {
                allocationMap(arg) = StackSlot(getNextArgOffset(index))
            }
        }

        val freeRegs = mutable.Queue.from(generalPurposeRegisters)

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
                        allocationMap(v) = PhysReg(freeRegs.dequeue())
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