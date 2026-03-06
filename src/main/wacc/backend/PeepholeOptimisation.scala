package wacc.backend

import scala.util.chaining.*

object PeepholeOptimisation{
    def allOptimisations(prog: Arm.Program): Arm.Program = {
        var body = prog.body
        var previousLength = -1

        while(previousLength != body.length) {
            previousLength = body.length
            body = body
                .pipe(optimisedBranches)
                .pipe(selfMove)
                .pipe(constBinary)
                .pipe(redundantLoads)
        }

        Arm.Program(prog.data, body)
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
        case Arm.AddImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.SubImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.OrImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.LslImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.LsrImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.AsrImm(size, reg, src, Arm.Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case Arm.AndImm(size, reg, _, Arm.Immediate(0)) :: xs => Arm.Movz(size, reg, Arm.Immediate(0)) :: constBinary(xs)
        case x :: xs => x :: constBinary(xs)
        case Nil => Nil
    }

    private def binaryHandler(size: Size, reg: PhysicalReg, src: PhysicalReg, xs: List[Arm.Instruction]) = {
        if (reg == src) constBinary(xs)
        else Arm.MovReg(size, reg, src) :: constBinary(xs)
    }

    private def redundantLoads(body: List[Arm.Instruction]): List[Arm.Instruction] = body match {
        case Arm.Stur(sSize, sReg, sSrc) :: Arm.Ldur(lSize, lReg, lSrc) :: xs if (sSize == lSize && sReg == lReg && sSrc == lSrc) =>
                Arm.Stur(sSize, sReg, sSrc) :: redundantLoads(xs)
        case Arm.Store(sSize, sReg, sSrc) :: Arm.Load(lSize, lReg, lSrc) :: xs if (sSize == lSize && sReg == lReg && sSrc == lSrc) =>
                Arm.Store(sSize, sReg, sSrc) :: redundantLoads(xs)
        case x :: xs => x :: redundantLoads(xs)
        case Nil => Nil
    }
}
