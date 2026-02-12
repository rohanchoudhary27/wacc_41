package wacc.backend

class PhsicalReg(regName: String, size: Size)

class RegisterAllocator {

    val SP  = PhysicalReg("sp", _8)
    val FP  = PhysicalReg("x29", _8)
    val LR  = PhysicalReg("x30", _8)
    val X0  = PhysicalReg("x0", _8)

    val Scratch1 = PhysicalReg("x9", _8)
    val Scratch2 = PhysicalReg("x10", _8)

    def allocate(prog: RegProgram): RegProgram = {
        val allocatedFuncs = prog.funcs.map(allocateFunction)
        val allocatedBody = allocateBlock(prog.mainBody)
        RegProgram(allocatedFuncs, prog.data, allocatedBody)
  }

    private def allocateFunction(func: FunctionDef): FunctionDef = {
        val newBody = allocateBlock(func.body)
        FunctionDef(func.name, newBody)
    }

    private def allocateBlock(instrs: List[Instruction]): List[Instruction] = {
        val output = ListBuffer[Instruction]()

        val stackMap = scala.collection.mutable.Map[VirtualReg, Int]()
        var currentOffset = 0
        
        val allVirtuals = findAllVirtualRegs(instrs)
        for (vReg <- allVirtuals) {
            stackMap(vReg) = currentOffset
            currentOffset += 8
        }
        val totalStackSize = if (currentOffset % 16 == 0) currentOffset else currentOffset + 8

        if (totalStackSize > 0) {
            output += Sub(SP, SP, Immediate(totalStackSize))
        }

        for (instr <- instrs) {
            instr match {
                case Add(dest: VirtualReg, src1: VirtualReg, src2: VirtualReg) =>
                    output ++= load(src1, Scratch1, stackMap)
                    output ++= load(src2, Scratch2, stackMap)
                    output += Add(Scratch1, Scratch1, Scratch2)
                    output ++= store(dest, Scratch1, stackMap)

                case Mov(dest: VirtualReg, imm: Immediate) =>
                    output += Mov(Scratch1, imm)
                    output ++= store(dest, Scratch1, stackMap)

                case Mov(dest: VirtualReg, src: VirtualReg) =>
                    output ++= load(src, Scratch1, stackMap)
                    output ++= store(dest, Scratch1, stackMap)

                case Ret =>
                    if (totalStackSize > 0) {
                        output += Add(SP, SP, Immediate(totalStackSize))
                    }
                    output += Ret

                case Call(label, args, destOpt) =>
                    args.zipWithIndex.foreach {
                        case (vReg: VirtualReg, idx) if idx < 8 =>
                            val argReg = PhysicalReg(s"x$idx", _8)
                            output ++= load(vReg, argReg, stackMap)
                        case _ =>
                    }
                output += Call(label, List(), None)
                    destOpt.foreach { dest =>
                        output ++= store(dest, X0, stackMap)
                    }
                case other => output += other              
                }
            }      
        output.toList
    }
}