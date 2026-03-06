package wacc.backend

object PeepholeOptimisation{
    def allOptimisations(prog: Arm.Program): Arm.Program = {
        val Arm.Program(data, body) = prog
        var optimisedBody = optimisedBranches(body)

        Arm.Program(data, optimisedBody)
    }

    private def optimisedBranches(body: List[Arm.Instruction]): List[Arm.instruction] = body match {
        case Arm.BranchUncond(bLabel) :: Arm.LabelDef(lLabel) :: xs if bLabel == lLabel => Arm.LabelDef(lLabel) :: optimisedBranches(xs)
        case x :: xs => x :: optimisedBranches(xs)
        case Nil => Nil
    }
}
