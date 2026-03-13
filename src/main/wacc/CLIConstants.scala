package wacc

import wacc.backend.OptimisationConstants


/** Constants for the compiler CLI. */
private object CompilerCLI {

    /** The CLI flag that enables all backend peephole optimisations */
    val OptimisationFlag: String = "-o"

    /** Expected number of non flag arguments (just the source file path) */
    val ExpectedArgCount: Int = 1

    /** File extension suffix of every WACC source file */
    val WaccExtension: String = ".wacc"

    /** File extension suffix of unoptimised assembly output files */
    val AsmExtension: String = ".s"

    /** File extension suffix of optimised assembly output files */
    val OptimisedAsmExtensionSuffix: String = "_o"

    /* Individual optimisation flags */
    val OptimisationBinaryFlag = s"${OptimisationFlag}${OptimisationConstants.BINARY_PEEPHOLE_GROUP}"
    val OptimisationBranchesFlag = s"${OptimisationFlag}${OptimisationConstants.BRANCHES_PEEPHOLE_GROUP}"
    val OptimisationMemoryFlag = s"${OptimisationFlag}${OptimisationConstants.MEMORY_PEEPHOLE_GROUP}"
    val OptimisationRegistersFlag = s"${OptimisationFlag}${OptimisationConstants.REGISTERS_PEEPHOLE_GROUP}"

     /** The complete set of flags the compiler accepts */
    val knownFlags: Set[String] = Set(
        OptimisationBinaryFlag,
        OptimisationBranchesFlag,
        OptimisationMemoryFlag,
        OptimisationRegistersFlag,
        OptimisationFlag
    )
}

