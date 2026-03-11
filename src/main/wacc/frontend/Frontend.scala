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
  * @param generalTailRecCalls `Call` nodes that are generally tail-recursive.
  * @param safePairAccesses    `PairElem` nodes whose base pointer is provably non-null.
  * @param safeDivisions       `BinaryExpr` (Div/Mod) nodes whose divisor is provably non-zero.
  * @param safeArrayAccesses   `ArrayElem` nodes whose index is provably within bounds.
  * @param safeChrCalls        `UnaryExpr` (chr) nodes whose argument is provably in ASCII range.
  * @param safeOverflowExprs   `Expr` nodes that provably cannot overflow 32-bit signed range.
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
  * 
  * `WaccFrontend.run` drives every compilation phase in order.  Each phase is
  * expressed as a private method that returns `Either[FrontendCompilationResult, T]`:
  *  - `Left(result)` short circuits the pipeline and returns `result` immediately
  *  - `Right(value)` passes `value` to the next phase
  *
  * Compilation phases:
  *   1. loadSource      — read the file from disk.
  *   2. sanitise        — reject non-ASCII characters.
  *   3. parse           — source text to syntactic AST.
  *   4. resolveImports  — inline imported functions
  *   5. syntaxCheck     — reject functions with non returning paths
  *   6. rename          — assign unique ids to variables and eliminate scopes
  *   7. typeCheck       — verify type correctness
  *   8. staticAnalysis  — detect guaranteed runtime errors, identify safe checks
  *
  * Exit codes: `0` success , `100` syntax/import error, `200` semantic error
  */
object WaccFrontend {

    /** Compiles a WACC source file end-to-end, returning a `FrontendCompilationResult`
      *
      * @param path Path to the `.wacc` source file to compile.
      * @return A `FrontendCompilationResult` whose `exitCode` reflects the
      *         outcome and whose `typedAST`, `optimState` are populated on success.
      */
    def run(path: Path): FrontendCompilationResult = {
        val pipeline = for {
            /* read source from disk */
            (codeLines, sourceCode) <- loadSource(path)

            /* Shared logger for all syntactic errors*/
            syntacticErrors = new ErrorLogger(codeLines)

            /* reject non-ASCII characters */
            _  <- sanitise(sourceCode, syntacticErrors)

            /* parse source text into a syntactic AST */
            syntacticAST <- parse(sourceCode, syntacticErrors)

            /* resolve imports and merge imported funcs into the program */
            mergedAST <- resolveImports(syntacticAST, path, syntacticErrors)

            /* reject functions with non-returning paths */
            _ <- syntaxCheck(mergedAST, syntacticErrors)

            /* Logger for semantic errors */
            semanticErrors = new ErrorLogger(codeLines)

            /* renamer */
            renamedAST <- rename(mergedAST, semanticErrors)

            /* type-checker */
            typedAST <- typeCheck(renamedAST, semanticErrors)

            /* local static analysis */
            analysisResult <- staticAnalysis(typedAST, semanticErrors)

            /* gather tail call and safe check metadata */
            optimState = buildOptimState(typedAST, analysisResult)
        } yield FrontendCompilationResult(CompilerSuccess, None, Some(typedAST), Some(optimState))

        pipeline.merge
    }

    /** File loader: reads the source file, returning the lines and the full string
      *
      * @param path Path to the source file.
      */
    private def loadSource(
        path: Path
    ): Either[FrontendCompilationResult, (Vector[String], String)] = {
        val source = Source.fromFile(path.toFile)
        val codeLines: Vector[String] =
            try source.getLines().toVector
            finally source.close()
        Right((codeLines, codeLines.mkString("\n")))
    }

    /** Sanitiser: rejects source text containing non-ASCII characters.
      *
      * Returns `Left` immediately with a `SyntaxError` result if the sanitiser
      * finds any disallowed characters; otherwise returns `Right`
      *
      * @param sourceCode    The full source text.
      * @param errorLogger   Logger to record the sanitiser's error message.
      */
    private def sanitise(
        sourceCode: String,
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Unit] = {
        Sanitiser.sanitise(sourceCode, errorLogger)
        failOnErrors(errorLogger, SyntaxError)
    }

    /** Parser: parses the source text into a syntactic AST
      *
      * Returns `Left` with a `SyntaxError` result on a parse error;
      * otherwise returns `Right(syntacticAST)`
      *
      * @param sourceCode    The full source text.
      * @param errorLogger   Logger to record any parse error message.
      */
    private def parse(
        sourceCode: String,
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Program[SyntacticData]] =
        parser.parse(sourceCode) match {
            case Right(ast) => Right(ast)
            case Left(parseError) =>
                errorLogger.logSyntaxError(parseError)
                Left(syntaxFailure(errorLogger))
        }

    /** Import resolver: resolves all imports and merges their `Func` nodes
      *
      * Imported functions are prepended to the program's function list so they
      * are visible to each other and to the main body.  Returns `Left` with a
      * `SyntaxError` result if any import cannot be resolved.
      *
      * @param ast           The syntactic AST before import resolution.
      * @param sourcePath    Path to the main source file; its parent is the search root.
      * @param errorLogger   Logger to record any import errors.
      */
    private def resolveImports(
        ast: Program[SyntacticData],
        sourcePath: Path,
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Program[SyntacticData]] = {
        val sourceDir = sourcePath.toAbsolutePath.getParent
        ImportResolver.resolveAll(ast.imports, sourceDir) match {
            case Left(importErrors) =>
                importErrors.foreach(err => errorLogger.logImportError(err.pos, err.errMsg))
                Left(syntaxFailure(errorLogger))
            case Right(resolvedModules) =>
                val importedFuncs = resolvedModules.flatMap(_.funcs)
                Right(ast.copy(
                    imports = Nil,  /* all imports have been resolved */
                    funcs = importedFuncs ++ ast.funcs
                ))
        }
    }

    /** Syntax checker: checks that every function has no non returning control flow paths
      *
      * Returns `Left` with a `SyntaxError` result if any function can reach
      * the end without returning; otherwise returns `Right`
      *
      * @param ast         The merged syntactic AST.
      * @param errorLogger Logger to record any non-returning-path errors.
      */
    private def syntaxCheck(
        ast: Program[SyntacticData],
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Unit] = {
        SyntaxPass.syntaxCheck(ast, errorLogger)
        failOnErrors(errorLogger, SyntaxError)
    }

    /** Renamer: renames all binders to unique IDs, producing a renamed AST.
      *
      * Returns `Left` with a `SemanticError` result if the renamer finds any
      * scoping errors, otherwise returns `Right(renamedAST)`.
      *
      * @param ast         The merged syntactic AST.
      * @param errorLogger Logger to record renamer errors.
      */
    private def rename(
        ast: Program[SyntacticData],
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Program[SemanticData]] = {
        val renamedAST = Renamer.rename(ast, errorLogger)
        failOnErrors(errorLogger, SemanticError).map(_ => renamedAST)
    }

    /** Type checker: typechecks the renamed AST
      * 
      * Returns `Left` with a `SemanticError` result on type
      * errors, otherwise returns `Right(typedAST)`
      *
      * @param renamedAST  The AST produced by phase 6.
      * @param errorLogger Logger to record type errors.
      */
    private def typeCheck(
        renamedAST: Program[SemanticData],
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, Program[SemanticData]] = {
        val funcTable = renamedAST.info match {
            case SemanticData.TopLevel(ft) => ft
            case _ => Map.empty[String, SemanticData.Func]
        }
        val typedAST = TypeChecker.check(renamedAST, errorLogger, funcTable)
        failOnErrors(errorLogger, SemanticError).map(_ => typedAST.get)
    }

    /** LSA: runs the static analyser over the typed AST
      *
      * Detects guaranteed runtime errors and identifies checks that
      * can be safely removed by the backend. 
      * 
      * Returns `Left` with a `SemanticError` result if any
      * error-level diagnostics are emitted, otherwise returns
      * `Right(analysisResult)`.
      *
      * @param typedAST    The AST produced by phase 7.
      * @param errorLogger Logger that receives any error-level diagnostics.
      */
    private def staticAnalysis(
        typedAST: Program[SemanticData],
        errorLogger: ErrorLogger
    ): Either[FrontendCompilationResult, AnalysisResult] = {
        val analysisResult = StaticAnalyser.run(typedAST, errorLogger)
        failOnErrors(errorLogger, SemanticError).map(_ => analysisResult)
    }

    /** Collects tail call and safe-check metadata into a `FrontendOptimState`
      *
      * @param typedAST       The fully-typed AST.
      * @param analysisResult The safe-check sets from phase 8.
      */
    private def buildOptimState(
        typedAST: Program[SemanticData],
        analysisResult: AnalysisResult
    ): FrontendOptimState = {
        val (selfTailRecCalls, generalTailRecCalls) = CheckTailRec.checkTailRec(typedAST)
        FrontendOptimState(
            selfTailRecCalls    = selfTailRecCalls,
            generalTailRecCalls = generalTailRecCalls,
            safePairAccesses    = analysisResult.safePairAccesses,
            safeDivisions       = analysisResult.safeDivisions,
            safeArrayAccesses   = analysisResult.safeArrayAccesses,
            safeChrCalls        = analysisResult.safeChrCalls,
            safeOverflowExprs   = analysisResult.safeOverflowOps
        )
    }

    /** Returns `Left(failure)` if `errorLogger` has recorded any errors,
      * otherwise returns `Right(())` so the pipeline can continue.
      *
      * @param errorLogger The logger to inspect.
      * @param exitCode    The `ExitCode` to embed in the failure result
      */
    private def failOnErrors(
        errorLogger: ErrorLogger,
        exitCode: ExitCode
    ): Either[FrontendCompilationResult, Unit] =
        if (errorLogger.hasErrors())
            Left(FrontendCompilationResult(exitCode, Some(errorLogger), None))
        else
            Right(())

    /** Constructs a `SyntaxError` failure result carrying the given logger. */
    private def syntaxFailure(errorLogger: ErrorLogger): FrontendCompilationResult =
        FrontendCompilationResult(SyntaxError, Some(errorLogger), None)
}