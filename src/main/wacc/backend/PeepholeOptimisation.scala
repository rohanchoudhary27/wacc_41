package wacc.backend

import scala.util.chaining.*
import wacc.backend.Arm.*
import wacc.CompilerCLI
import scala.annotation.tailrec
import scala.collection.mutable

// Holds the state of which optimizations are turned on
case class OptConfig(
    binary: Boolean = false,
    branches: Boolean = false,
    memory: Boolean = false,
    registers: Boolean = false
) {
    // Helper to check if any optimizations are turned on
    def isOptimised: Boolean = binary || branches || memory || registers
}

object OptConfig {

    /** Create  an OptFlags from an array of flag strings */
    def fromFlags(optFlags: Seq[String]): OptConfig = {
        val allOpts = optFlags.contains(CompilerCLI.OptimisationFlag)
        def isEnabled(flag: String): Boolean = allOpts || optFlags.contains(flag)

        OptConfig(
            binary = isEnabled(CompilerCLI.OptimisationBinaryFlag),
            branches = isEnabled(CompilerCLI.OptimisationBranchesFlag),
            memory = isEnabled(CompilerCLI.OptimisationMemoryFlag),
            registers = isEnabled(CompilerCLI.OptimisationRegistersFlag)
        )
    }
}

object PeepholeOptimisation{

    var iterations = 0
    val ruleHits = mutable.Map[String, Int]().withDefaultValue(0)

    /* Peephole hit names grouped into their peephole groups */
    sealed trait OptimisationGroup
    object OptRegisters extends OptimisationGroup {
        val selfMove = "selfMove"
        val coalesceMove = "coalesceMove"
        val alternateMoves = "alternateMoves"
        val removeOverwriteMovReg = "removeOverwriteMovReg"
        val removeOverwriteMovz = "removeOverwriteMovz"
        val copyRegisters = "copyRegisters"
    }

    object OptBinary extends OptimisationGroup {
        val constAdd = "constBinaryAdd"
        val constSub = "constBinarySub"
        val constOr = "constBinaryOr"
        val constLsl = "constBinaryLsl"
        val constLsr = "constBinaryLsr"
        val constAsr = "constBinaryAsr"
        val constAnd = "constBinaryAnd"
        val immediateMinimiseAdd = "immediateMinimiseAdd"
        val immediateMinimiseSub = "immediateMinimiseSub"
    }

    object OptMemory extends OptimisationGroup {
        val redundantLoadsStur = "redundantLoadsStur"
        val redundantLoadsStore = "redundantLoadsStore"
    }

    object OptBranches extends OptimisationGroup {
        val optimisedBranches = "optimisedBranches"
    }

    /**
      * Peephole optimiser that takes in the low level IR, runs through a pass of the optimising pipeline
      * and repeats until the assembly output doesnt change
      *
      * @param prog: low level IR representation of the Arm assembly
      */
    def optimisations(prog: Program, config: OptConfig): Arm.Program = {
        var body = prog.body
        var prev: List[Instruction] = Nil
        iterations = 0
        ruleHits.clear()

        /* Repeat until length constant, this will always terminate as optimisations
           cause the length to be smaller than or equal to previous size. */
        while(prev != body) {
            iterations += 1
            prev = body
                
            if (config.binary) {
                body = body
                    .pipe(constBinary(_, Nil))
                    .pipe(immediateMinimise(_, Nil))
            }

            if (config.branches) {
                body = body
                    .pipe(optimisedBranches(_, Nil))
            }

            if (config.memory) {
                body = body
                    .pipe(redundantLoads(_, Nil))
            }

            if (config.registers) {
                body = body
                    .pipe(alternateMoves(_, Nil))
                    .pipe(copyRegisters(_, Nil))
                    .pipe(removeOverwrite(_, Nil))
                    .pipe(coalesceMove(_, Nil))
                    .pipe(selfMove(_, Nil))
            }

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
    @tailrec
    private def selfMove(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case MovReg(size, reg, src) :: xs if reg == src => 
            ruleHits(OptRegisters.selfMove) += 1
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
            ruleHits(OptBranches.optimisedBranches) += 1
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
    private def constBinary(body: List[Instruction], acc: List[Instruction]): List[Instruction] = {
        
        /** Squash redundant binary operations */
        def resolveZeroAcc(rule: String, size: Size, reg: PhysicalReg, src: PhysicalReg): List[Instruction] = {
            ruleHits(rule) += 1
            if (reg == src) acc 
            else MovReg(size, reg, src) :: acc
        }

        body match {
            case AddImm(s, r, src, Immediate(0)) :: xs => constBinary(xs, resolveZeroAcc(OptBinary.constAdd, s, r, src))
            case SubImm(s, r, src, Immediate(0)) :: xs => constBinary(xs, resolveZeroAcc(OptBinary.constSub, s, r, src))
            case OrImm(s, r, src, Immediate(0)) :: xs  => constBinary(xs, resolveZeroAcc(OptBinary.constOr, s, r, src))
            case LslImm(s, r, src, Immediate(0)) :: xs => constBinary(xs, resolveZeroAcc(OptBinary.constLsl, s, r, src))
            case LsrImm(s, r, src, Immediate(0)) :: xs => constBinary(xs, resolveZeroAcc(OptBinary.constLsr, s, r, src))
            case AsrImm(s, r, src, Immediate(0)) :: xs => constBinary(xs, resolveZeroAcc(OptBinary.constAsr, s, r, src))
            
            case AndImm(size, reg, _, Immediate(0)) :: xs => 
                ruleHits(OptBinary.constAnd) += 1
                constBinary(xs, Movz(size, reg, Immediate(0)) :: acc)
                
            case x :: xs => constBinary(xs, x :: acc)
            case Nil => acc.reverse
        }
    }


    /**
      * Removes redundant store then load for registers
      *
      * @param body: list of low level IR instructions to be optimised
      */
    @tailrec
    private def redundantLoads(body: List[Instruction], acc: List[Instruction]): List[Instruction] = {

        /* Optimise a given redundant load */
        def resolveLoad(rule: String, size: Size,
                        sReg: PhysicalReg,
                        lReg: PhysicalReg,
                        storeInst: Instruction): List[Instruction] = {
            ruleHits(rule) += 1
            if (sReg == lReg) storeInst :: acc
            else MovReg(size, lReg, sReg) :: storeInst :: acc
        }

        body match {
            case (st @ Stur(sSize, sReg, sSrc)) :: Ldur(lSize, lReg, lSrc) :: xs 
                if (sSize == lSize && sSrc == lSrc) =>
                    redundantLoads(xs, resolveLoad(OptMemory.redundantLoadsStur, sSize, sReg, lReg, st))

            case (st @ Store(sSize, sReg, sSrc)) :: Load(lSize, lReg, lSrc) :: xs 
                if (sSize == lSize && sSrc == lSrc) =>
                    redundantLoads(xs, resolveLoad(OptMemory.redundantLoadsStore, sSize, sReg, lReg, st))

            case x :: xs => redundantLoads(xs, x :: acc)
            case Nil => acc.reverse
        }
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
            ruleHits(OptRegisters.coalesceMove) += 1
            coalesceMove(xs, Movz(s2, dest, imm) :: Movz(s1, scratch, imm) :: acc)
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
            ruleHits(OptRegisters.alternateMoves) += 1
            alternateMoves(xs, MovReg(size1, dest1, src1) :: acc)
        case x :: xs => alternateMoves(xs, x :: acc)
        case Nil => acc.reverse
    }

    @tailrec
    private def immediateMinimise(body: List[Instruction], acc: List[Instruction]): List[Instruction] = body match {
        case AddImm(s1, d1, src1, Immediate(i1)) :: AddImm(s2, d2, src2, Immediate(i2)) :: xs
        if (s1 == s2 && d1 == d2 && src1 == src2 && d1 == src1) => 
            ruleHits(OptBinary.immediateMinimiseAdd) += 1
            immediateMinimise(xs, AddImm(s1, d1, src1, Immediate(i1 + i2)) :: acc)
        
        case SubImm(s1, d1, src1, Immediate(i1)) :: SubImm(s2, d2, src2, Immediate(i2)) :: xs
        if (s1 == s2 && d1 == d2 && src1 == src2 && d1 == src1) => 
            ruleHits(OptBinary.immediateMinimiseSub) += 1
            immediateMinimise(xs, SubImm(s1, d1, src1, Immediate(i1 + i2)) :: acc)
        
        case x :: xs => immediateMinimise(xs, x :: acc)
        case Nil => acc.reverse
    }

    /**
     * Removes redundant register assignments if the destination register is 
     * immediately overwritten by a const
     * 
     * mov r1, r2
     * movz r1, #5
     * 
     * goes to
     * 
     * movz r1, #5
     *
     * @param body list of low level IR instructions to be optimised
     * @param acc list of low level IR instructions that have been optimised
     */
    @tailrec
    private def removeOverwrite(body: List[Instruction], acc: List[Instruction]): List[Instruction] = {
        
        /* Resolve a given register constant overwrite */
        def resolveOverwrite(rule: String, secondInst: Instruction): List[Instruction] = {
            ruleHits(rule) += 1
            secondInst :: acc
        }

        body match {
            case MovReg(_, d1, _) :: (m2 @ Movz(_, d2, _)) :: xs if d1 == d2 =>
                removeOverwrite(xs, resolveOverwrite(OptRegisters.removeOverwriteMovReg, m2))

            case Movz(_, d1, _) :: (m2 @ Movz(_, d2, _)) :: xs if d1 == d2 =>
                removeOverwrite(xs, resolveOverwrite(OptRegisters.removeOverwriteMovz, m2))
        
            case x :: xs => removeOverwrite(xs, x :: acc)
            case Nil => acc.reverse
        }
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
            ruleHits(OptRegisters.copyRegisters) += 1
            copyRegisters(xs, MovReg(s2, d2, src1) :: MovReg(s1, d1, src1) :: acc)

        case x :: xs => copyRegisters(xs, x :: acc)
        case Nil => acc.reverse
    }
}
