package wacc.backend

import scala.collection.mutable

object RegisterAllocator {

    sealed trait Location
    case class PhysReg(name: String) extends Location
    case class StackSlot(offset: Int) extends Location

    val usableRegisters = List(
        "x9", "x10", "x11", "x12", "x13", "x14", "x15",
        "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"
    )

    case class AllocationResult(mainAlloc: Map[VirtualReg, Location], funcAllocs: Map[String, Map[VirtualReg, Location]])

    def allocate(prog: RegProgram): AllocationResult = {
        val funcMaps = prog.funcs.map { f => 
            f.name -> allocateBlock(f.body) 
        }.toMap
    
        val mainMap = allocateBlock(prog.body)
    
        AllocationResult(mainMap, funcMaps)
    }

    def allocateBlock(instrs: List[Instruction]): Map[VirtualReg, Location] = {
    
        val allocationMap = mutable.Map[VirtualReg, Location]()
        val freeRegs = mutable.Queue.from(usableRegisters)
        var stackOffset = 0

        def scan(obj: Any): Unit = obj match {
        case v: VirtualReg =>
            if (!allocationMap.contains(v)) {
                if (freeRegs.nonEmpty) {
                    allocationMap(v) = PhysReg(freeRegs.dequeue())
                } else {
                allocationMap(v) = StackSlot(stackOffset)
                stackOffset += 8
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