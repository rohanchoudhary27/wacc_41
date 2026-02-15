package wacc

import java.nio.file.{Files, Path, Paths}
import wacc.frontend.*
import wacc.backend.*
import wacc.ExitCodes._

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
            Console.err.println("Requires file.")
            sys.exit(code(ExitCodes.CompilerFailure))
        }

        val path = Paths.get(args(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(s"Error: Path '$path' does not exist.")
            sys.exit(code(CompilerFailure))
        }

        val ast: Program[SemanticData] = runFrontend(path)

        runBackend(path, ast)
    }
    
    /**
    * Exit the compiler and display error messages due to a malformed program.
    *
    * @param filename: the name of the error-containing .wacc file
    * @param errorLogger: the `ErrorLogger` object with the errors
    * @param exitCode: the exit code of the program (100/200 in this case)
    */
    private def failureExit(filename: String,
                            errorLogger: ErrorLogger,
                            exitCode: ExitCode): Unit = {

        val errorType = exitCode match {
            case ExitCodes.SyntaxError => "Syntax error"
            case ExitCodes.SemanticError  => "Semantic error"
            case _ => "Internal Error"
        }

        val allErrors = errorLogger.retrieveAllErrors()

        /* For syntax errors, position is already on the first line from ErrorBuilder.
                    For semantic errors, get the position from the first error. */
        val (header, body) = exitCode match {
            case ExitCodes.SyntaxError =>

                /* ErrorBuilder outputs "(line X, column Y):\n..."
                                Split to extract position for the header */
                val newlineIdx = allErrors.indexOf('\n')
                if (newlineIdx >= 0) {
                    val posLine = allErrors.substring(0, newlineIdx)
                    val rest = allErrors.substring(newlineIdx + 1)
                    (s"$errorType in $filename $posLine", rest)
                } else {
                    (s"$errorType in $filename:", allErrors)
                }

            case ExitCodes.SemanticError => (s"$errorType in $filename:", allErrors)
        }

        /* Print the errors */
        Console.err.println(header)
        Console.err.println(body)
        Console.err.println()
        Console.err.println(
            s"Finished with exit code ${exitCode}"
        )

        sys.exit(code(exitCode))
    }


    private def runFrontend(path: Path): Program[SemanticData] = {
        val frontendResult: FrontendCompilationResult = WaccFrontend.run(path)

        frontendResult.errors match {
            case Some(errorLogger) =>
                failureExit(path.getFileName.toString, 
                            errorLogger,
                            frontendResult.exitCode)
                throw new RuntimeException("Unreachable") 

            case None =>
                frontendResult.typedAST.get
        }
    }

    private def runBackend(path: Path, ast: Program[SemanticData]): Unit = {
        val filename = path.getFileName.toString
        val outFilename = filename.replaceAll("\\.wacc$", "") + ".s"

        val backendResult = WaccBackend.run(ast, outFilename)

        if (backendResult.exitCode == ExitCodes.Success) {
            println(s"Success - finished with exit code ${backendResult.exitCode}")
        } 
        
        sys.exit(code(backendResult.exitCode))
    }
}