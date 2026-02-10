package wacc

import java.nio.file.{Files, Paths}
import wacc.frontend.*
import wacc.backend.*
import wacc.ExitCodes._

/** 
* Main entry point for the WACC compiler which runs the frontend and backend.
*/
object WaccCompiler {

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

        /* Run the frontend of the compiler */
        val frontendCompilationResult: FrontendCompilationResult = WaccFrontend.run(path)

        frontendCompilationResult.errors match {

            /* Failure exit */
            case Some(errorLogger) => failureExit(path.getFileName.toString,
                                                  errorLogger,
                                                  frontendCompilationResult.exitCode)

            /* No errors in file - send to backend */
            case None =>
                println(
                    s"Success - finished with exit code ${frontendCompilationResult.exitCode}"
                )
                sys.exit(code(frontendCompilationResult.exitCode))
                /* Put a helper function in main, called here to call the backend (WaccBackend.run)
                   and handle any errors to avoid spaghetti code */
        }
    }

}