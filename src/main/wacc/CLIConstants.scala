package wacc


/** Constants for the compiler CLI. */
private object CompilerCLI {

    /** The CLI flag that enables backend optimisations */
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
    val OptimisationBinaryFlag = s"${OptimisationFlag}Binary"
    val OptimisationBranchesFlag = s"${OptimisationFlag}Branches"
    val OptimisationMemoryFlag = s"${OptimisationFlag}Memory"
    val OptimisationRegistersFlag = s"${OptimisationFlag}Registers"

     /** The complete set of flags the compiler accepts */
    val knownFlags: Set[String] = Set(
        OptimisationBinaryFlag,
        OptimisationBranchesFlag,
        OptimisationMemoryFlag,
        OptimisationRegistersFlag
    )
}

