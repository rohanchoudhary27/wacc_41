package wacc

import java.nio.file.{Files, Path, Paths}
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

    /** Main function for the WACC compiler.
      *
      * Runs the frontend and then the backend on the read .wacc file. 
      *
      * @param args:
      *   Command Line arguments. Only 1 is expected (the path of the file to
      *   compile).
      */
    def main(args: Array[String]): Unit = {

        /* Check optimisation flag and remove */
        val optimisationFlag = args.contains("-O")
        val filteredArgs = args.filterNot(_ == "-O")

        /* Expect one argument for file name */
        if (filteredArgs.length != 1) {
            Console.err.println(TopLevelError.FileSystemError("Requires a file").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Remaining arguments should just be path name */
        val path = Paths.get(filteredArgs(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(TopLevelError.FileSystemError(s"Path '$path' does not exist").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Run the compiler */
        val finalExitCode = compilePipeline(path, optimisationFlag) 

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
    private def compilePipeline(path: Path, optimisationFlag: Boolean): ExitCode = runFrontend(path) match {
        case Left(frontendErrorCode) => frontendErrorCode
        case Right(ast, optimState) => runBackend(path, ast, optimState, optimisationFlag)
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
    private def runFrontend(path: Path): Either[ExitCode, (Program[SemanticData], FrontendOptimState)] = {

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
                           optimisationFlag: Boolean): ExitCode = {

        /* Change the filename to a .s assembly file but keep the name the same */
        val outFilename = if (optimisationFlag)
                then path.getFileName.toString.replaceAll("\\.wacc$", ".s")
                else path.getFileName.toString.replaceAll("\\.wacc$", "_O.s")

        /* Run the ast through the backend of the compiler */
        val backendResult = WaccBackend.run(outFilename, ast, optimState, optimisationFlag)

        backendResult.error.foreach { 
            errMsg => Console.err.println(TopLevelError.BackendPipelineError(errMsg).outputFormat)
        }
        
        backendResult.exitCode
    }
}