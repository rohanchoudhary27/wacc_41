package wacc.backend

import scala.util.chaining.*

object PeepholeOptimisation{
    def allOptimisations(prog: Arm.Program): Arm.Program = {
        val Arm.Program(data, body) = prog
        val optimisedBody = body
            .pipe(optimisedBranches)
            .pipe(selfMove)
            .pipe(constBinary)

        Arm.Program(data, optimisedBody)
    }

    private def selfMove(body: List[Arm.Instruction]): List[Arm.Instruction] = body match {
        case Arm.MovReg(size, reg, src) :: xs if reg == src => selfMove(xs)
        case x :: xs => x :: selfMove(xs)
        case Nil => Nil
    }

    private def optimisedBranches(body: List[Arm.Instruction]): List[Arm.Instruction] = body match {
        case Arm.BranchUncond(bLabel) :: Arm.LabelDef(lLabel) :: xs if bLabel == lLabel => Arm.LabelDef(lLabel) :: optimisedBranches(xs)
        case x :: xs => x :: optimisedBranches(xs)
        case Nil => Nil
    }

    private def constBinary(body: List[Arm.Instruction]): List[Arm.Instruction] = body match {
        case (Arm.AddImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.SubImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.OrImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.LslImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.LsrImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.AsrImm(size, reg, src, Arm.Immediate(0)) :: xs) => optimisedBinaryHandler(size, src, reg, xs)
        case (Arm.AndImm(size, reg, _, Arm.Immediate(0)) :: xs) =>
            Arm.Movz(size, reg, Arm.Immediate(0)) :: constBinary(xs)
        case x :: xs => x :: constBinary(xs)
        case Nil => Nil
    }

    private def optimisedBinaryHandler(size: Size, reg: PhysicalReg, src: PhysicalReg, xs: List[Arm.Instruction]) = {
        if (reg == src) constBinary(xs)
        else Arm.MovReg(size, reg, src) :: constBinary(xs)
    }
}
