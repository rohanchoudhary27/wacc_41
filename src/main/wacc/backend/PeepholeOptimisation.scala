package wacc.backend

import scala.util.chaining.*
import wacc.backend.Arm.*

object PeepholeOptimisation{

    @tailrec
    def allOptimisations(prog: Program): Arm.Program = {
        var body = prog.body
        var previousLength = -1

        while(previousLength != body.length) {
            previousLength = body.length
            body = body
                .pipe(optimisedBranches)
                .pipe(selfMove)
                .pipe(constBinary)
                .pipe(redundantLoads)
                .pipe(coalesceMove)
                .pipe(alternateMoves)
        }

        Program(prog.data, body)
    }

    private def selfMove(body: List[Instruction]): List[Instruction] = body match {
        case MovReg(size, reg, src) :: xs if reg == src => selfMove(xs)
        case x :: xs => x :: selfMove(xs)
        case Nil => Nil
    }

    private def optimisedBranches(body: List[Instruction]): List[Instruction] = body match {
        case BranchUncond(bLabel) :: LabelDef(lLabel) :: xs if bLabel == lLabel => LabelDef(lLabel) :: optimisedBranches(xs)
        case x :: xs => x :: optimisedBranches(xs)
        case Nil => Nil
    }

    private def constBinary(body: List[Instruction]): List[Instruction] = body match {
        case AddImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case SubImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case OrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case LslImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case LsrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case AsrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case AndImm(size, reg, _, Immediate(0)) :: xs => Movz(size, reg, Immediate(0)) :: constBinary(xs)
        case x :: xs => x :: constBinary(xs)
        case Nil => Nil
    }

    private def binaryHandler(size: Size, reg: PhysicalReg, src: PhysicalReg, xs: List[Instruction]) = {
        if (reg == src) constBinary(xs)
        else MovReg(size, reg, src) :: constBinary(xs)
    }

    private def redundantLoads(body: List[Instruction]): List[Instruction] = body match {
        case Stur(sSize, sReg, sSrc) :: Ldur(lSize, lReg, lSrc) :: xs if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg)
                Stur(sSize, sReg, sSrc) :: redundantLoads(xs)
            else
                Stur(sSize, sReg, sSrc) :: MovReg(sSize, lReg, sReg) :: redundantLoads(xs)
        case Store(sSize, sReg, sSrc) :: Load(lSize, lReg, lSrc) :: xs if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg)
                Store(sSize, sReg, sSrc) :: redundantLoads(xs)
            else
                Store(sSize, sReg, sSrc) :: MovReg(sSize, lReg, sReg) :: redundantLoads(xs)
        case x :: xs => x :: redundantLoads(xs)
        case Nil => Nil
    }

    private def coalesceMove(body: List[Instruction]): List[Instruction] = body match {
        case Movz(s1, scratch, imm) :: MovReg(s2, dest, src) :: xs if scratch == src && s1 == s2 =>
            Movz(s2, dest, imm) :: coalesceMove(xs)
        case x :: xs => x :: coalesceMove(xs)
        case Nil => Nil
    }

    private def alternateMoves(body: List[Instruction]): List[Instruction] = body match {
        case MovReg(size1, dest1, src1) :: MovReg(size2, dest2, src2) :: xs if size1 == size2 && dest1 == src2 && dest2 == src1 =>
            MovReg(size1, dest1, src1) :: alternateMoves(xs)
        case x :: xs => x :: alternateMoves(xs)
        case Nil => Nil
    }
}
