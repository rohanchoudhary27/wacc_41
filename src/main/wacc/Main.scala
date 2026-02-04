package wacc

import java.nio.file.{Files, Path, Paths}
import scala.io.Source

case class CompilationResult(
    exitCode: Int,
    errors: Option[ErrorLogger] = None
)

object WaccFrontend {

    def compile(path: Path): CompilationResult = {

        /* Load file as String */
        val source = Source.fromFile(path.toFile)
        val codeLines: Vector[String] =
            try source.getLines().toVector
            finally source.close()

        val sourceCode: String = codeLines.mkString("\n")
        val syntacticErrors = new ErrorLogger(codeLines)

        Sanitiser.sanitise(sourceCode, syntacticErrors)

        if (syntacticErrors.hasErrors()) {
            CompilationResult(100, Some(syntacticErrors))
        } else {

            val result = parser.parse(sourceCode)

            result match {
                case Left(_) => CompilationResult(100, Some(syntacticErrors))
                
                case Right(syntacticAST) =>
                    val semanticErrors = new ErrorLogger(codeLines)

                    SyntaxPass.syntaxCheck(syntacticAST, syntacticErrors)
                    if (syntacticErrors.hasErrors()) {
                        CompilationResult(100, Some(syntacticErrors))

                    } else {
                        val renamedAST = Renamer.rename(syntacticAST, semanticErrors)

                        val funcTable = renamedAST.info match {
                            case wacc.SemanticData.TopLevel(ft) => ft
                            case _ => Map.empty[String, wacc.SemanticData.Func]
                        }

                        TypeChecker.check(renamedAST, semanticErrors, funcTable)

                        if (semanticErrors.hasErrors()) CompilationResult(200, Some(semanticErrors))
                        else CompilationResult(0, None)
                    }
            }
        }
    }

    /**
    * Main function for WACC compiler.
    *
    * Stages:
    * Parsing: Source text to a SyntacticAST, Exit code 100 on failure
    * Additional Syntax Check: Check SyntacticAST for non-returning paths in
    * functions, returning exit code 100 on failure
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
            println("Requires file.")
            sys.exit(-1)
        }

        val path = Paths.get(args(0))

        if (!Files.exists(path)) {
            println(s"Error: Path '$path' does not exist.")
            sys.exit(-1)
        }

        val compilationResult: CompilationResult = compile(path)

        compilationResult.errors match
            case Some(errorLogger) =>

                val errorHeader = compilationResult.exitCode match {
                    case 100 => "Failure: 100 - Syntax error:"
                    case 200 => "Failure: 200 - Semantic error:"
                }

                Console.err.println(errorHeader)
                Console.err.println(errorLogger.retrieveAllErrors())

                sys.exit(compilationResult.exitCode)

            case None => 
                sys.exit(compilationResult.exitCode)

    }

}

