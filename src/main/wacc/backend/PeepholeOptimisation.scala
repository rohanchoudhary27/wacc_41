package wacc.backend

import scala.util.chaining.*
import wacc.backend.Arm.*

object PeepholeOptimisation{

    /**
      * Peephole optimiser that takes in the low level IR, runs through a pass of the optimising pipeline
      * and repeats until the assembly output doesnt change
      *
      * @param prog: low level IR representation of the Arm assembly
      */
    def allOptimisations(prog: Program): Arm.Program = {
        var body = prog.body
        var previousLength = -1

        /* Repeat until length constant, this will always terminate as optimisations
           cause the length to be smaller than or equal to previous size. */
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

        /* Wrap data and optimised body in Arm.Program for Outputter. */
        Program(prog.data, body)
    }

    /**
      * Removes instructions that move a to same register
      * 
      * e.g.
      * mov r1, r1
      * 
      * ->
      * 
      * (instruction is removed)
      *
      * @param body: list of low level IR instructions to be optimised
      */
    private def selfMove(body: List[Instruction]): List[Instruction] = body match {
        case MovReg(size, reg, src) :: xs if reg == src => selfMove(xs)
        case x :: xs => x :: selfMove(xs)
        case Nil => Nil
    }

    /**
      * Removes Branch unconditional if followed by a label, as this will run anyway
      *
      * e.g
      *     ...
      *     b L1
      * .L1:
      *     ...
      * 
      * ->
      * 
      *     ...
      * .L1:
      *     ...
      * 
      * @param body: list of low level IR instructions to be optimised
      */
    private def optimisedBranches(body: List[Instruction]): List[Instruction] = body match {
        case BranchUncond(bLabel) :: LabelDef(lLabel) :: xs if bLabel == lLabel => 
            LabelDef(lLabel) :: optimisedBranches(xs)
        case x :: xs => x :: optimisedBranches(xs)
        case Nil => Nil
    }

    /**
      * Removes maths instructions that dont change the output of the expression
      *
      * @param body: list of low level IR instructions to be optimised
      */
    private def constBinary(body: List[Instruction]): List[Instruction] = body match {
        case AddImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case SubImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case OrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case LslImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case LsrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case AsrImm(size, reg, src, Immediate(0)) :: xs => binaryHandler(size, reg, src, xs)
        case AndImm(size, reg, _, Immediate(0)) :: xs => 
            Movz(size, reg, Immediate(0)) :: constBinary(xs)
        case x :: xs => x :: constBinary(xs)
        case Nil => Nil
    }

    /* Helper function for constBinary */
    private def binaryHandler(size: Size, 
                              reg: PhysicalReg, 
                              src: PhysicalReg, 
                              xs: List[Instruction]) = {
        if (reg == src) constBinary(xs)
        else MovReg(size, reg, src) :: constBinary(xs)
    }

    /**
      * Removes redundant store then load for registers
      *
      * @param body: list of low level IR instructions to be optimised
      */
    private def redundantLoads(body: List[Instruction]): List[Instruction] = body match {
        case Stur(sSize, sReg, sSrc) :: Ldur(lSize, lReg, lSrc) :: xs 
        if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg)
                Stur(sSize, sReg, sSrc) :: redundantLoads(xs)
            else
                Stur(sSize, sReg, sSrc) :: MovReg(sSize, lReg, sReg) :: redundantLoads(xs)
        case Store(sSize, sReg, sSrc) :: Load(lSize, lReg, lSrc) :: xs 
        if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg)
                Store(sSize, sReg, sSrc) :: redundantLoads(xs)
            else
                Store(sSize, sReg, sSrc) :: MovReg(sSize, lReg, sReg) :: redundantLoads(xs)
        case x :: xs => x :: redundantLoads(xs)
        case Nil => Nil
    }

    /**
      * Combines move operations if writing then reading the same register
      *
      * @param body: list of low level IR instructions to be optimised
      */
    private def coalesceMove(body: List[Instruction]): List[Instruction] = body match {
        case Movz(s1, scratch, imm) :: MovReg(s2, dest, src) :: xs 
        if scratch == src && s1 == s2 =>
            Movz(s2, dest, imm) :: coalesceMove(xs)
        case x :: xs => x :: coalesceMove(xs)
        case Nil => Nil
    }

    /**
      * Removes moving to a register and back.
      * 
      * e.g
      * mov r1, r2
      * mov r2, r1
      * 
      * ->
      * 
      * mov r1, r2
      *
      * @param body: list of low level IR instructions to be optimised
      */
    private def alternateMoves(body: List[Instruction]): List[Instruction] = body match {
        case MovReg(size1, dest1, src1) :: MovReg(size2, dest2, src2) :: xs 
        if size1 == size2 && dest1 == src2 && dest2 == src1 =>
            MovReg(size1, dest1, src1) :: alternateMoves(xs)
        case x :: xs => x :: alternateMoves(xs)
        case Nil => Nil
    }
}
