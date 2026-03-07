package wacc.backend

import scala.util.chaining.*
import wacc.backend.Arm.*
import scala.annotation.tailrec

object PeepholeOptimisation{

    /**
      * Peephole optimiser that takes in the low level IR, runs through a pass of the optimising pipeline
      * and repeats until the assembly output doesnt change
      *
      * @param prog: low level IR representation of the Arm assembly
      */
    def allOptimisations(prog: Program): Arm.Program = {
        var body = prog.body
        var prev: List[Instruction] = Nil
        var iteration = 0

        /* Repeat until length constant, this will always terminate as optimisations
           cause the length to be smaller than or equal to previous size. */
        while(prev != body) {
            iteration += 1
            prev = body
            body = body
                .pipe(optimisedBranches(_, Nil))
                .pipe(selfMove(_, Nil))
                .pipe(constBinary(_, Nil))
                .pipe(redundantLoads(_, Nil))
                .pipe(coalesceMove(_, Nil))
                .pipe(alternateMoves(_, Nil))
        }

        /* Wrap data and optimised body in Arm.Program for Outputter. */
        println(iteration)
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
    @tailrec
    private def selfMove(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case MovReg(size, reg, src) :: xs if reg == src => selfMove(xs, acc)
        case x :: xs => selfMove(xs, x :: acc)
        case Nil => acc.reverse
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
    @tailrec
    private def optimisedBranches(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case BranchUncond(bLabel) :: LabelDef(lLabel) :: xs if bLabel == lLabel => 
            optimisedBranches(xs, LabelDef(lLabel) :: acc)
        case x :: xs => optimisedBranches(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
      * Removes maths instructions that dont change the output of the expression
      *
      * @param body: list of low level IR instructions to be optimised
      */
    @tailrec
    private def constBinary(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case AddImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case SubImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case OrImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case LslImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case LsrImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case AsrImm(size, reg, src, Immediate(0)) :: xs =>
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case AndImm(size, reg, _, Immediate(0)) :: xs => 
            constBinary(xs, Movz(size, reg, Immediate(0)) :: acc)
        case x :: xs => constBinary(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
      * Removes redundant store then load for registers
      *
      * @param body: list of low level IR instructions to be optimised
      */
    @tailrec
    private def redundantLoads(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case Stur(sSize, sReg, sSrc) :: Ldur(lSize, lReg, lSrc) :: xs 
        if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg) redundantLoads(xs, Stur(sSize, sReg, sSrc):: acc)
            else redundantLoads(xs, MovReg(sSize, lReg, sReg) :: Stur(sSize, sReg, sSrc) :: acc)

        case Store(sSize, sReg, sSrc) :: Load(lSize, lReg, lSrc) :: xs 
        if (sSize == lSize && sSrc == lSrc) =>
            if (sReg == lReg) redundantLoads(xs, Store(sSize, sReg, sSrc) :: acc)
            else redundantLoads(xs, MovReg(sSize, lReg, sReg) :: Store(sSize, sReg, sSrc) :: acc)

        case x :: xs => redundantLoads(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
      * Combines move operations if writing then reading the same register
      *
      * @param body: list of low level IR instructions to be optimised
      */
    @tailrec
    private def coalesceMove(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case Movz(s1, scratch, imm) :: MovReg(s2, dest, src) :: xs 
        if scratch == src && s1 == s2 =>
            coalesceMove(xs, Movz(s2, dest, imm) :: acc)
        case x :: xs => coalesceMove(xs, x :: acc)
        case Nil => acc.reverse
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
    @tailrec
    private def alternateMoves(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case MovReg(size1, dest1, src1) :: MovReg(size2, dest2, src2) :: xs 
        if size1 == size2 && dest1 == src2 && dest2 == src1 =>
            alternateMoves(xs, MovReg(size1, dest1, src1) :: acc)
        case x :: xs => alternateMoves(xs, x :: acc)
        case Nil => acc.reverse
    }
}
