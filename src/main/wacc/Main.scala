package wacc

import java.nio.file.{Files, Path, Paths}
import wacc.frontend.*
import wacc.backend.*
import wacc.ExitCodes.*

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

        /* Expect a singular argument */
        if (args.length != 1) {
            Console.err.println(TopLevelError.FileSystemError("Requires a file").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        val path = Paths.get(args(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(TopLevelError.FileSystemError(s"Path '$path' does not exist").outputFormat)
            sys.exit(code(CompilerFailure))
        }

        /* Run the compiler */
        val finalExitCode = compilePipeline(path)

        /* Exit with exit code and message */
        if (finalExitCode == CompilerSuccess) {
            println(s"Success - finished with exit code ${code(finalExitCode)}")
        } else {
            Console.err.println(s"\nFinished with exit code ${code(finalExitCode)}")
        }

    }

    /**
      * Run the frontend and backend successively with error handling support
      *
      * @param path The path of the wacc file to compile
      * @return An `ExitCode` representing the success/failure status of the compilation
      */
    private def compilePipeline(path: Path): ExitCode = runFrontend(path) match {
        case Left(frontendErrorCode) => frontendErrorCode
        case Right(ast) => runBackend(path, ast)
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

        val errorType = exitCode match {
            case ExitCodes.SyntaxError   => "Syntax error"
            case ExitCodes.SemanticError => "Semantic error"
            case _                       => "Internal Error"
        }

        /* Print top-level file header */
        Console.err.println(s"$errorType in $filename:\n")

        /* Print error logger errors */
        Console.err.println(errorLogger.retrieveAllErrors())
    }

    /**
      * Runs the WACC file through the WACC frontend compiler.
      *
      * @param path: path to the WACC file to be compiled
      * @return Program[SemanticData]: which is an AST
      */
    private def runFrontend(path: Path): Either[ExitCode, Program[SemanticData]] = {

        /* Run the WACC code through the front end of the compiler */
        val frontendResult: FrontendCompilationResult = WaccFrontend.run(path)

        frontendResult match {

            case FrontendCompilationResult(CompilerSuccess, _, Some(ast)) =>
                Right(ast)
            /* Case if the frontend does not compile succesfully (Syntax/Semantic error) */
            case FrontendCompilationResult(exitCode, Some(logger), _) if exitCode != CompilerSuccess => 
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
      */
    private def runBackend(path: Path, ast: Program[SemanticData]): ExitCode = {

        /* Change the filename to a .s assembly file but keep the name the same */
        val outFilename = path.getFileName.toString.replaceAll("\\.wacc$", ".s")

        /* Run the ast through the backend of the compiler */
        val backendResult = WaccBackend.run(ast, outFilename)

        backendResult.error.foreach { 
            errMsg => Console.err.println(TopLevelError.BackendPipelineError(errMsg).outputFormat)
        }
        
        backendResult.exitCode
    }
}