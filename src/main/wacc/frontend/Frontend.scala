package wacc.frontend

import java.nio.file.Path
import scala.io.Source
import wacc.ExitCodes._
import wacc.frontend.lsa.*

/** A class holding the result of compilation. `exitCode` is the exit code of
  * the program compilation and `errors` is None if there were no errors and
  * Some(ErrorLogger) if errors where encountered.
  */
case class FrontendCompilationResult(
    exitCode: ExitCode,
    errors: Option[ErrorLogger] = None,
    typedAST: Option[Program[SemanticData]],
    optimState: Option[FrontendOptimState] = None
)

/**
  * A class holding additional information to the AST for facilitating
  * WACC extensions.
  *
  * @param tailRecCalls A set of `Call` AST nodes which are tail recursive
  */
case class FrontendOptimState(
    selfTailRecCalls: Set[Call[SemanticData]] = Set.empty,
    generalTailRecCalls: Set[Call[SemanticData]] = Set.empty,
    safePairAccesses: Set[PairElem[SemanticData]] = Set.empty,
    safeDivisions: Set[BinaryExpr[SemanticData]] = Set.empty,
    safeArrayAccesses: Set[ArrayElem[SemanticData]] = Set.empty,
    safeChrCalls: Set[UnaryExpr[SemanticData]] = Set.empty,
    safeOverflowExprs: Set[Expr[SemanticData]] = Set.empty
)

/**
  * Companion object for `FrontendOptimState`
  */
object FrontendOptimState {
    val empty: FrontendOptimState = FrontendOptimState()
}

/** The top level object of the WACC compiler frontend. Contains the `main`
  * function as well as a `compile` function which doesn't do printing (to be
  * re-used by the integration test runner)
  */
object WaccFrontend {

  /** Compiles a WACC program, returning a Compilation with its exit code and
    * any errors logged in an ErrorLogger object.
    *
    * Stages:
    *   Sanitising:      Source text checked for non-ASCII chars, Exit code 100 on failure
    *   Parsing:         Source text to a SyntacticAST, Exit code 100 on failure
    *   Import Resolution: Resolve imports & merge all imported Func nodes into the
    *                    program AST, Exit code 100 on failure
    *   Syntax Check:    Check SyntacticAST for non-returning paths in functions,
    *                    Exit code 100 on failure
    *   Renaming:        Transforms SyntacticAST to use SemanticData, Log errors on failure
    *   Type Checker:    Verifies type correctness, Log errors on failure
    *
    * Exit Codes: 0 - Success  100 - Syntactic/Import Errors  200 - Semantic Errors
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
      FrontendCompilationResult(SyntaxError, Some(syntacticErrors), None)

    } else {

      /* (Phase 2 - Parse) */
      val result = parser.parse(sourceCode)

      result match {

        /* Parser syntax error */
        case Left(parseError) =>
          syntacticErrors.logSyntaxError(parseError)
          FrontendCompilationResult(SyntaxError, Some(syntacticErrors), None)

        case Right(syntacticAST) =>

          /* (Phase 3 - Import Resolution): Resolve all imports and merge their
             Func nodes into the program. Imported functions already have their
             names qualified by the resolver (i.e. fileName.func). */
          val sourceDir = path.toAbsolutePath.getParent

          ImportResolver.resolveAll(syntacticAST.imports, sourceDir) match {
            case Left(importErrors) =>
              importErrors.foreach(err => syntacticErrors.logImportError(err.pos, err.errMsg))
              FrontendCompilationResult(SyntaxError, Some(syntacticErrors), None)

            case Right(resolvedModules) =>
              /* Merge imported funcs (already name-qualified) with the main
                 program's funcs. Imported funcs come first so their definitions
                 are visible to each other and to the main body. */
              val importedFuncs = resolvedModules.flatMap(_.funcs)
              val mergedAST = syntacticAST.copy(
                imports = Nil, // to signify that they've all been resolved
                funcs   = importedFuncs ++ syntacticAST.funcs
              )

              /* (Phase 4 - Additional Syntax Check): Check more complex syntax
                         rules (functions must have no non-returning paths) */
              SyntaxPass.syntaxCheck(mergedAST, syntacticErrors)

              if (syntacticErrors.hasErrors()) {
                FrontendCompilationResult(SyntaxError, Some(syntacticErrors), None)

              } else {

                /* Object to log semantic errors */
                val semanticErrors = new ErrorLogger(codeLines)

                /* (Phase 5 - Renamer) */
                val renamedAST = Renamer.rename(mergedAST, semanticErrors)

                /* Retrieve table of function signatures */
                val funcTable = renamedAST.info match {
                  case wacc.frontend.SemanticData.TopLevel(ft) => ft
                  case _ => Map.empty[String, wacc.frontend.SemanticData.Func]
                }

                /* (Phase 6 - Type Checker) */
                val typedAST = TypeChecker.check(renamedAST, semanticErrors, funcTable)

                /* Log any semantic errors */
                if (semanticErrors.hasErrors())
                  FrontendCompilationResult(SemanticError, Some(semanticErrors), None)
                else {
                  /* (Phase 6 - Static Analysis) */
                  val typedASTRes = typedAST.get
                  val analysisResult = StaticAnalyser.run(typedASTRes, semanticErrors)
                  if (semanticErrors.hasErrors())
                    FrontendCompilationResult(SemanticError, Some(semanticErrors), None)
                  else {
                    /* Frontend optimisation passes */
                    val (selfTailRecCalls, generalisedTailRecCalls) = CheckTailRec.checkTailRec(typedASTRes)
                    val optimState = FrontendOptimState(
                      selfTailRecCalls, generalisedTailRecCalls, 
                      analysisResult.safePairAccesses, 
                      analysisResult.safeDivisions,
                      analysisResult.safeArrayAccesses,
                      analysisResult.safeChrCalls,
                      analysisResult.safeOverflowOps
                    )
                    FrontendCompilationResult(CompilerSuccess, None, typedAST, Some(optimState))
                  }
                }
              }
          }
      }
    }
  }
}