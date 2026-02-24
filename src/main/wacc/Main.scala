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
            Console.err.println("Requires file.")
            sys.exit(code(ExitCodes.CompilerFailure))
        }

        val path = Paths.get(args(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(s"Error: Path '$path' does not exist.")
            sys.exit(code(CompilerFailure))
        }

        /* Run frontend compiler and return an AST. */
        val ast: Program[SemanticData] = runFrontend(path)

        /* Run backend compiler using the AST from the frontend. */
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

    /**
      * Runs the WACC file through the WACC frontend compiler.
      *
      * @param path: path to the WACC file to be compiled
      * @return Program[SemanticData]: which is an AST
      */
    private def runFrontend(path: Path): Program[SemanticData] = {

        // Run the WACC code through the front end of the compiler
        val frontendResult: FrontendCompilationResult = WaccFrontend.run(path)

        frontendResult.errors match {

            // Case if the frontend does not compile succesfully
            case Some(errorLogger) =>
                failureExit(path.getFileName.toString, 
                            errorLogger,
                            frontendResult.exitCode)
                throw new RuntimeException("Unreachable") 

            // Case if the frontend pass has no errors
            case None =>
                frontendResult.typedAST.get
        }
    }

    /**
      * Runs the WACC file through the WACC frontend compiler.
      *
      * @param path: path of the file compiled by the frontend
      * @param ast: the AST produced by the front end of the compiler
      */
    private def runBackend(path: Path, ast: Program[SemanticData]): Unit = {

        // Change the filename to a .s assembly file but keep the name the same
        val filename = path.getFileName.toString
        val outFilename = filename.replaceAll("\\.wacc$", "") + ".s"

        // Run the ast through the backend of the compiler
        val backendResult = WaccBackend.run(ast, outFilename)

        if (backendResult.exitCode == CompilerSuccess) {
            println(s"Success - finished with exit code ${backendResult.exitCode}")
        } 
        
        sys.exit(code(backendResult.exitCode))
    }
}