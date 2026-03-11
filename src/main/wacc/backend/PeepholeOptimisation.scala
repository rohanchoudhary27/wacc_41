package wacc.backend

import scala.util.chaining.*
import wacc.backend.Arm.*
import scala.annotation.tailrec
import scala.collection.mutable

object PeepholeOptimisation{

    var iterations = 0
    val ruleHits = mutable.Map[String, Int]().withDefaultValue(0)

    /**
      * Peephole optimiser that takes in the low level IR, runs through a pass of the optimising pipeline
      * and repeats until the assembly output doesnt change
      *
      * @param prog: low level IR representation of the Arm assembly
      */
    def allOptimisations(prog: Program): Arm.Program = {
        var body = prog.body
        var prev: List[Instruction] = Nil
        iterations = 0
        ruleHits.clear()

        /* Repeat until length constant, this will always terminate as optimisations
           cause the length to be smaller than or equal to previous size. */
        while(prev != body) {
            iterations += 1
            prev = body
            body = body
                .pipe(constBinary(_, Nil))
                .pipe(optimisedBranches(_, Nil))
                .pipe(redundantLoads(_, Nil))
                .pipe(immediateMinimise(_, Nil))
                .pipe(alternateMoves(_, Nil))
                .pipe(copyRegisters(_,Nil))
                .pipe(removeOverwrite(_,Nil))
                .pipe(coalesceMove(_, Nil))
                .pipe(selfMove(_, Nil))
        }

        /* Wrap data and optimised body in Arm.Program for Outputter. */
        println(s"METRIC_ITERATIONS: ${iterations}")
        ruleHits.foreach {case(rule, count) => println(s"METRIC_RULE ${rule}: $count")}
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
        case MovReg(size, reg, src) :: xs if reg == src => 
            ruleHits("selfMove") += 1
            selfMove(xs, acc)
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
            ruleHits("optimisedBranches") += 1
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
            ruleHits("constBinaryAdd") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case SubImm(size, reg, src, Immediate(0)) :: xs =>
            ruleHits("constBinarySub") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case OrImm(size, reg, src, Immediate(0)) :: xs =>
            ruleHits("constBinaryOr") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case LslImm(size, reg, src, Immediate(0)) :: xs =>
            ruleHits("constBinaryLsl") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case LsrImm(size, reg, src, Immediate(0)) :: xs =>
            ruleHits("constBinaryLsr") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case AsrImm(size, reg, src, Immediate(0)) :: xs =>
            ruleHits("constBinaryAsr") += 1
            if (reg == src) constBinary(xs, acc) else constBinary(xs, MovReg(size, reg, src) :: acc)
        case AndImm(size, reg, _, Immediate(0)) :: xs => 
            ruleHits("constBinaryAnd") += 1
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
            ruleHits("redundantLoadsStur") += 1
            if (sReg == lReg) redundantLoads(xs, Stur(sSize, sReg, sSrc):: acc)
            else redundantLoads(xs, MovReg(sSize, lReg, sReg) :: Stur(sSize, sReg, sSrc) :: acc)

        case Store(sSize, sReg, sSrc) :: Load(lSize, lReg, lSrc) :: xs 
        if (sSize == lSize && sSrc == lSrc) =>
            ruleHits("redundantLoadsStore") += 1
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
            ruleHits("coalesceMove") += 1
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
            ruleHits("alternateMoves") += 1
            alternateMoves(xs, MovReg(size1, dest1, src1) :: acc)
        case x :: xs => alternateMoves(xs, x :: acc)
        case Nil => acc.reverse
    }

    @tailrec
    private def immediateMinimise(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case AddImm(s1, d1, src1, Immediate(i1)) :: AddImm(s2, d2, src2, Immediate(i2)) :: xs
        if (s1 == s2 && d1 == d2 && src1 == src2 && d1 == src1) => 
            ruleHits("immediateMinimiseAdd") += 1
            immediateMinimise(xs, AddImm(s1, d1, src1, Immediate(i1 + i2)) :: acc)
        
        case SubImm(s1, d1, src1, Immediate(i1)) :: SubImm(s2, d2, src2, Immediate(i2)) :: xs
        if (s1 == s2 && d1 == d2 && src1 == src2 && d1 == src1) => 
            ruleHits("immediateMinimiseSub") += 1
            immediateMinimise(xs, SubImm(s1, d1, src1, Immediate(i1 + i2)) :: acc)
        
        case x :: xs => immediateMinimise(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
      * Remove overwriting to registers immediately after
      *
      * @param body list of low level IR instructions to be optimised
      * @param acc list of low level IR instructions that have been optimised
      */
    @tailrec
    private def removeOverwrite(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case MovReg(_, d1, _) :: Movz(s2, d2, imm) :: xs if d1 == d2 =>
            ruleHits("removeOverwriteMovReg") += 1
            removeOverwrite(xs, Movz(s2, d2, imm) :: acc)

        case Movz(_, d1, _) :: Movz(s2, d2, imm) :: xs if d1 == d2 =>
            ruleHits("removeOverwriteMovz") += 1
            removeOverwrite(xs, Movz(s2, d2, imm) :: acc)
        
        case x :: xs => removeOverwrite(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
      * Removes redundant register copy chains
      * 
      * mov w26, w25
      * mov w20, w26
      * 
      * -> 
      * 
      * mov w26, w25
      * mow w20, w25
      *
      * @param body list of low level IR instructions to be optimised
      * @param acc list of low level IR instructions that have been optimised
      */
    @tailrec
    private def copyRegisters(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case MovReg(s1, d1, src1) :: MovReg(s2, d2, src2) :: xs if s1 == s2 && d1 == src2 =>
            ruleHits("copyRegisters") += 1
            copyRegisters(xs, MovReg(s2, d2, src1) :: MovReg(s1, d1, src1) :: acc)

        case x :: xs => copyRegisters(xs, x :: acc)
        case Nil => acc.reverse
    }
}
