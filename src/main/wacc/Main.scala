package wacc

import java.nio.file.{Files, Path, Paths}
import scala.io.Source

/**
* A class holding the result of compilation. `exitCode` is the exit code of the program
* compilation and `errors` is None if there were no errors and Some(ErrorLogger) if errors
* where encountered.
*/
case class CompilationResult(
    exitCode: Int,
    errors: Option[ErrorLogger] = None
)

/**
* The top level object of the WACC compiler frontend. Contains the `main` function as well as
* a `compile` function which doesn't do printing (to be re-used by the integration test runner)
*/
object WaccFrontend {

    /* Compiler exit codes */
    val successCode = 0
    val syntacticErrorCode = 100
    val semanticErrorCode = 200 
    val compilerFailure = -1

    /**
    * Compiles a WACC program, returning a Compilation with its exit code and any errors logged
    * in an ErrorLogger object.
    * 
    * @param path: The path of the file to compile
    * 
    * @return: A `CompilationResult` object which contains the exit code and Option[ErrorLogger]
    */
    def compile(path: Path): CompilationResult = {

        /* Load file as a list of code lines */
        val source = Source.fromFile(path.toFile)
        val codeLines: Vector[String] =
            try source.getLines().toVector
            finally source.close()

        /* Get the code as a single string */
        val sourceCode: String = codeLines.mkString("\n")

        /* Object to log syntax errors to */
        val syntacticErrors = new ErrorLogger(codeLines)

        /* (Phase 1 - Sanitise): Initial sanitise check of the string
           (no-non ASCII chars allowed) */
        Sanitiser.sanitise(sourceCode, syntacticErrors)

        if (syntacticErrors.hasErrors()) {
            CompilationResult(syntacticErrorCode, Some(syntacticErrors))

        } else {

            /* (Phase 2 - Parse) */
            val result = parser.parse(sourceCode)

            result match {

                /* Parser syntax error */
                case Left(parseError) =>
                    syntacticErrors.logSyntaxError(parseError)
                    CompilationResult(syntacticErrorCode, Some(syntacticErrors))
                
                case Right(syntacticAST) =>

                    /* (Phase 3 - Additional Syntax Check): Check more complex syntax
                       rules (functions must have no non-returning paths) */
                    SyntaxPass.syntaxCheck(syntacticAST, syntacticErrors)

                    if (syntacticErrors.hasErrors()) {
                        CompilationResult(syntacticErrorCode, Some(syntacticErrors))

                    } else {

                        /* Object to log semantic errors */
                        val semanticErrors = new ErrorLogger(codeLines)

                        /* (Phase 4 - Renamer) */
                        val renamedAST = Renamer.rename(syntacticAST, semanticErrors)

                        /* Retrieve table of function signatures */
                        val funcTable = renamedAST.info match {
                            case wacc.SemanticData.TopLevel(ft) => ft
                            case _ => Map.empty[String, wacc.SemanticData.Func]
                        }

                        /* (Phase 5 - Type Checker) */
                        TypeChecker.check(renamedAST, semanticErrors, funcTable)

                        /* Log any semantic errors */
                        if (semanticErrors.hasErrors()) CompilationResult(semanticErrorCode,
                                                                          Some(semanticErrors))
                        else CompilationResult(successCode, None)
                    }
            }
        }
    }

    /**
    * Main function for WACC compiler.
    *
    * Stages:
    * Sanitising: Source text checked for non-ASCII chars, Exit code 100 on failure
    * Parsing: Source text to a SyntacticAST, Exit code 100 on failure
    * Additional Syntax Check: Check SyntacticAST for non-returning paths in
    *   functions, returning exit code 100 on failure
    * Renaming: Transforms SyntacticAST to use SemanticData, Log errors on failure
    * Type Checker: Verifies Type correctness and, Log errors on failure
    *
    * Exit Codes:
    *   0 - Success
    *   100 - Syntactic Errors
    *   200 - Semantic Errors
    *   -1 - System Error
    *
    * @param args: Command Line arguments. Only 1 is expected (the path of the
    * file to compile).
    */
    def main(args: Array[String]): Unit = {

        /* Expect a singular argument */
        if(args.length != 1) {
            Console.err.println("Requires file.")
            sys.exit(compilerFailure)
        }

        val path = Paths.get(args(0))

        /* Validate file exists */
        if (!Files.exists(path)) {
            Console.err.println(s"Error: Path '$path' does not exist.")
            sys.exit(compilerFailure)
        }

        /* Compile the file */
        val compilationResult: CompilationResult = compile(path)

        compilationResult.errors match

            /* Failure exit */
            case Some(errorLogger) =>

                val errorHeader = compilationResult.exitCode match {
                    case `syntacticErrorCode` => "Failure: 100 - Syntax error:"
                    case `semanticErrorCode` => "Failure: 200 - Semantic error:"
                }

                Console.err.println(errorHeader)
                Console.err.println(errorLogger.retrieveAllErrors())

                sys.exit(compilationResult.exitCode)

            /* Successful exit */
            case None => 
                sys.exit(compilationResult.exitCode)
    }
}