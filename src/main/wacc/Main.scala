package wacc

import java.nio.file.{Files, Path, Paths}
import wacc.CompilerCLI
import wacc.frontend.*
import wacc.backend.*
import wacc.ExitCodes.*

/* Final exit status of the compiler */
private enum CompilerExitStatus {

    case Success(code: Int)
    case Info(msg: String, code: Int)

    def message: String = this match
        case Success(c)  => s"Success - finished with exit code $c"
        case Info(m, c)  => s"\n$m - finished with exit code $c"
}

/** 
  * Main entry point for the WACC compiler which runs the frontend and backend.
  */
object WaccCompiler {

    /** Main function for the WACC compiler. Parses command-line arguments, validates them, 
      * and drives the full compilation pipeline.
      * 
      * Argument layout: `[flags...] <path.wacc>`
      *
      *
      * @param args:
      *   Command Line arguments. Only 1 is expected (the path of the file to
      *   compile).
      */
    def main(args: Array[String]): Unit = {

        val (optFlags, remainingArgs) = args.toList.partition(_.startsWith("-"))
        
        val unknownFlags = optFlags.filterNot(CompilerCLI.knownFlags.contains)
        if (unknownFlags.nonEmpty) {
            Console.err.println(TopLevelError.FileArgumentError(unknownFlags).outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Set the flags that for the optimisations that want to be used.
           They have been grouped into Binary, Branches, Memory, Registers. */
        val config = OptConfig(
            binary = optFlags.contains(s"${CompilerCLI.OptimisationBinaryFlag}"),
            branches = optFlags.contains(s"${CompilerCLI.OptimisationBranchesFlag}"),
            memory = optFlags.contains(s"${CompilerCLI.OptimisationMemoryFlag}"),
            registers = optFlags.contains(s"${CompilerCLI.OptimisationRegistersFlag}")
        )

        if (remainingArgs.length != CompilerCLI.ExpectedArgCount) {
            Console.err.println(TopLevelError.FileSystemError("Requires a file").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Remaining arguments should just be path name */
        val path = Paths.get(remainingArgs(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(TopLevelError.FileSystemError(s"Path '$path' does not exist").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Run the compiler */
        val finalExitCode = compilePipeline(path, config) 

        /* Exit with exit code and message */
        if (finalExitCode == CompilerSuccess) {
            println(CompilerExitStatus.Success(code(finalExitCode)).message)
        } else {
            Console.err.println(CompilerExitStatus.Info("Finished", code(finalExitCode)).message)
        }

        sys.exit(code(finalExitCode))

    }

    /**
      * Run the frontend and backend successively with error handling support
      *
      * @param path The path of the wacc file to compile
      * @return An `ExitCode` representing the success/failure status of the compilation
      */
    private def compilePipeline(path: Path, config: OptConfig): ExitCode = runFrontend(path) match {
        case Left(frontendErrorCode) => frontendErrorCode
        case Right(ast, optimState) => runBackend(path, ast, optimState, config: OptConfig)
    }

    /**
    * Exit the compiler and display error messages due to a malformed program.
    *
    * @param filename: the name of the error-containing .wacc file
    * @param errorLogger: the `ErrorLogger` object with the errors
    * @param exitCode: the exit code of the program (100/200 in this case)
    */
    private def printFrontendErrors(filename: String,
                                    errorLogger: ErrorLogger,
                                    exitCode: ExitCode): Unit = {

        val errorName = ExitCodes.errorName(exitCode)

        /* Print top-level file header */
        Console.err.println(s"$errorName in $filename:\n")

        /* Print error logger errors */
        Console.err.println(errorLogger.retrieveAllErrors())
    }

    /**
      * Runs the WACC file through the WACC frontend compiler.
      *
      * @param path: path to the WACC file to be compiled
      * @return Left(ExitCode) on a failure exit (syntax/semantic error) or
      * Right(Program[SemanticData], FrontendOptimState) for the typed AST and additional
      * WACC Extension annotations
      */
    private def runFrontend(
        path: Path
    ): Either[ExitCode, (Program[SemanticData], FrontendOptimState)] = {

        /* Run the WACC code through the front end of the compiler */
        val frontendResult: FrontendCompilationResult = WaccFrontend.run(path)

        frontendResult match {

            case FrontendCompilationResult(CompilerSuccess, _, Some(ast), Some(optimState)) =>
                Right(ast, optimState)

            /* Case if the frontend does not compile succesfully (Syntax/Semantic error) */
            case FrontendCompilationResult(exitCode, Some(logger), _, _) if exitCode != CompilerSuccess => 
                printFrontendErrors(path.getFileName.toString, logger, exitCode)
                Left(exitCode)
            case _ =>
                Console.err.println(TopLevelError.FrontendPipelineError("Invalid frontend state").outputFormat)
                Left(CompilerFailure)
        }
    }

    /**
      * Runs the WACC file through the WACC frontend compiler.
      *
      * @param path: path of the file compiled by the frontend
      * @param ast: the AST produced by the front end of the compiler
      * @param optimState: `FrontendOptimState` for extra info to the AST for WACC extensions
      */
    private def runBackend(path: Path,
                           ast: Program[SemanticData],
                           optimState: FrontendOptimState,
                           config: OptConfig): ExitCode = {
        
        /* Collect optimisations that are wanted */
        val activeOpts = Seq(
            if (config.binary) OptimisationConstants.BINARY_PEEPHOLE_GROUP  else "",
            if (config.branches) OptimisationConstants.BRANCHES_PEEPHOLE_GROUP else "",
            if (config.memory) OptimisationConstants.MEMORY_PEEPHOLE_GROUP else "",
            if (config.registers) OptimisationConstants.REGISTERS_PEEPHOLE_GROUP  else ""
        ).filter(_.nonEmpty)

        val suffix = if (!config.isOptimised) {
            ""
        } else if (activeOpts.length == OptimisationConstants.NUM_PEEPHOLE_GROUPS) {
            CompilerCLI.OptimisedAsmExtensionSuffix // All optimizations
        } else {
            // Specific ablations (e.g., _oMemory_Registers)
            s"${CompilerCLI.OptimisedAsmExtensionSuffix}${activeOpts.mkString("_")}"
        }

        val outFilename = path.getFileName.toString.replaceAll(s"\\${CompilerCLI.WaccExtension}$$", s"$suffix.s")

        /* Run the ast through the backend of the compiler */
        val backendResult = WaccBackend.run(outFilename, ast, optimState, config)

        backendResult.error.foreach { 
            errMsg => Console.err.println(TopLevelError.BackendPipelineError(errMsg).outputFormat)
        }
        
        backendResult.exitCode
    }
}