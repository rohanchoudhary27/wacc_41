package wacc.frontend

import java.nio.file.Path
import scala.io.Source

/** A class holding the result of compilation. `exitCode` is the exit code of
  * the program compilation and `errors` is None if there were no errors and
  * Some(ErrorLogger) if errors where encountered.
  */
case class FrontendCompilationResult(
    exitCode: Int,
    errors: Option[ErrorLogger] = None,
    typedAST: Option[Program[SemanticData]]
)

/** The top level object of the WACC compiler frontend. Contains the `main`
  * function as well as a `compile` function which doesn't do printing (to be
  * re-used by the integration test runner)
  */
object WaccFrontend {

  /* Compiler exit codes */
  val successCode = 0
  val syntacticErrorCode = 100
  val semanticErrorCode = 200
  val compilerFailure = -1

  /** Compiles a WACC program, returning a Compilation with its exit code and
    * any errors logged in an ErrorLogger object.
    * 
    * 
    * Stages: Sanitising: Source text checked for non-ASCII chars, Exit code 100
    * on failure Parsing: Source text to a SyntacticAST, Exit code 100 on
    * failure Additional Syntax Check: Check SyntacticAST for non-returning
    * paths in functions, returning exit code 100 on failure Renaming:
    * Transforms SyntacticAST to use SemanticData, Log errors on failure Type
    * Checker: Verifies Type correctness and, Log errors on failure
    *
    * Exit Codes: 0 - Success 100 - Syntactic Errors 200 - Semantic Errors -1 -
    * System Error
    *
    * @param path:
    *   The path of the file to compile
    *
    * @return:
    *   A `FrontendCompilationResult` object which contains the exit code and
    *   Option[ErrorLogger]
    */
  def run(path: Path): FrontendCompilationResult = {

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
      FrontendCompilationResult(syntacticErrorCode, Some(syntacticErrors), None)

    } else {

      /* (Phase 2 - Parse) */
      val result = parser.parse(sourceCode)

      result match {

        /* Parser syntax error */
        case Left(parseError) =>
          syntacticErrors.logSyntaxError(parseError)
          FrontendCompilationResult(syntacticErrorCode, Some(syntacticErrors), None)

        case Right(syntacticAST) =>

          /* (Phase 3 - Additional Syntax Check): Check more complex syntax
                       rules (functions must have no non-returning paths) */
          SyntaxPass.syntaxCheck(syntacticAST, syntacticErrors)

          if (syntacticErrors.hasErrors()) {
            FrontendCompilationResult(syntacticErrorCode, Some(syntacticErrors), None)

          } else {

            /* Object to log semantic errors */
            val semanticErrors = new ErrorLogger(codeLines)

            /* (Phase 4 - Renamer) */
            val renamedAST = Renamer.rename(syntacticAST, semanticErrors)

            /* Retrieve table of function signatures */
            val funcTable = renamedAST.info match {
              case wacc.frontend.SemanticData.TopLevel(ft) => ft
              case _ => Map.empty[String, wacc.frontend.SemanticData.Func]
            }

            /* (Phase 5 - Type Checker) */
            val typedAST = TypeChecker.check(renamedAST, semanticErrors, funcTable)

            /* Log any semantic errors */
            if (semanticErrors.hasErrors())
              FrontendCompilationResult(semanticErrorCode, Some(semanticErrors), None)
            else FrontendCompilationResult(successCode, None, typedAST)
          }
      }
    }
  }

}
