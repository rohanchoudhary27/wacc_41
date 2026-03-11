package wacc.frontend.lsa

import wacc.frontend.*

import scala.collection.mutable.ListBuffer

/**
  * Mutable accumulator for all findings produced during a single static analysis
  * pass over a WACC program.
  *
  * An `AnalyserState` is created once per compilation and embedded (as a
  * `given`) through every `analyse*` / `evaluate*` call in `StaticAnalyser`.
  * 
  * It collects `AnalysisDiagnostic`` instances, and sets of AST nodes whose 
  * associated runtime checks are provably unnecessary.
  *
  * Each `markSafe*` method records an AST node that the backend flattener can
  * process without emitting the corresponding runtime guard.
  */
class AnalyserState {

    /**
      * Dispatches a `SafetyVerdict` to the appropriate callback, or does
      * nothing when the verdict is `SafetyVerdict.Indeterminate`
      *
      * This is the single point of truth for the three way provably error, 
      * provably safe, unknown pattern that occurs throughout `StaticAnalyser`.
      * 
      * Call-sites compute a verdict with SafetyVerdict.fromChecks and supply two lambdas:
      *
      *  - onError` — called when the verdict is `SafetyVerdict.DefiniteError`,
      *               emits a compile-time diagnostic
      *  - `onSafe`  — called when the verdict is `SafetyVerdict.ProvablySafe`,
      *                records the AST node in one of the safe node sets.
      *
      * @param verdict The `SafetyVerdict` to dispatch.
      * @param onError Action to run when `verdict` is `SafetyVerdict.DefiniteError`
      * @param onSafe  Action to run when `verdict` is `SafetyVerdict.ProvablySafe`
      */
    def applyVerdict(verdict: SafetyVerdict)(
        onError: AnalyserState => Unit,
        onSafe:  AnalyserState => Unit
    ): Unit = verdict match {
        case SafetyVerdict.DefiniteError => onError(this)
        case SafetyVerdict.ProvablySafe  => onSafe(this)
        case SafetyVerdict.Indeterminate => () /* insufficient precision */
    }

    /* List of diagnostics produced by the analyser. */
    private val diagnostics: ListBuffer[AnalysisDiagnostic] = ListBuffer.empty

    /* Set of PairElem AST nodes where the base pointer is provably non null. */
    private val safePairAccesses: scala.collection.mutable.Set[PairElem[SemanticData]] =
        scala.collection.mutable.Set.empty

    /* Set of BinaryExpr AST nodes (Div/Mod) where the divisor is provably
       nonzero, so the runtime division by zero check can be skipped. */
    private val safeDivisions: scala.collection.mutable.Set[BinaryExpr[SemanticData]] =
        scala.collection.mutable.Set.empty

    /* Set of ArrayElem AST nodes where the index is provably within bounds */
    private val safeArrayAccesses: scala.collection.mutable.Set[ArrayElem[SemanticData]] =
        scala.collection.mutable.Set.empty

    /* Set of UnaryExpr AST nodes where the chr function provably received safe value */
    private val safeChrCalls: scala.collection.mutable.Set[UnaryExpr[SemanticData]] =
        scala.collection.mutable.Set.empty

    /* Set of Expr AST nodes (BinaryExpr for + - *, UnaryExpr for -)
       where the operation provably cannot overflow 32 bit signed range */
    private val safeOverflowOps: scala.collection.mutable.Set[Expr[SemanticData]] =
        scala.collection.mutable.Set.empty

    /** Record a `PairElem` AST node whose base pointer is provably non null,
      * so the flattener can skip the runtime null check.
      *
      * @param pairElem The `PairElem` AST node with a proven-safe access
      */
    def markSafePairAccess(pairElem: PairElem[SemanticData]): Unit = safePairAccesses += pairElem

    /** Record a `BinaryExpr` (Div/Mod) AST node whose divisor is provably
      * nonzero, so the flattener can skip the runtime zero check.
      *
      * @param divExpr The `BinaryExpr` AST node with a proven-safe divisor
      */
    def markSafeDivision(divExpr: BinaryExpr[SemanticData]): Unit = safeDivisions += divExpr

    /** Record a `ArrayElem` AST node whose index is provably within bounds,
      * so the flattener can skip the runtime bounds check.
      *
      * @param arrayElem The `ArrayElem` AST node with a safe index
      */
    def markSafeArrayAccess(arrayElem: ArrayElem[SemanticData]): Unit = safeArrayAccesses += arrayElem

     /** Record an chr `UnaryExpr` AST node whose value is provably within ASCII bounds,
      * so the flattener can skip the runtime bounds check.
      *
      * @param chrExpr The chr `UnaryExpr` AST node with a safe value
      */
    def markSafeChrCall(chrExpr: UnaryExpr[SemanticData]): Unit = safeChrCalls += chrExpr

    /** Record a `Expr` AST node where we don't need to check for integer overflow.
     * 
     * @param expr The `Expr` AST node that is proven to not overflow or underflow
     */
    def markSafeOverflowOp(expr: Expr[SemanticData]): Unit = safeOverflowOps += expr

    /** Emit a diagnostic finding. */
    def emit(diagnostic: AnalysisDiagnostic): Unit = diagnostics += diagnostic

    /** Retrieve all diagnostics found so far */
    def getAllDiagnostics: List[AnalysisDiagnostic] = diagnostics.toList

    /** Retrieve the set of `PairElem` nodes where null checks can be eliminated */
    def getSafePairAccesses: Set[PairElem[SemanticData]] = safePairAccesses.toSet

    /** Retrieve the set of safe `BinaryExpr` (Div/Mod) nodes */
    def getSafeDivisions: Set[BinaryExpr[SemanticData]] = safeDivisions.toSet

    /** Retrieve the set of `ArrayElem` nodes where bounds checks can be eliminated */
    def getSafeArrayAccesses: Set[ArrayElem[SemanticData]] = safeArrayAccesses.toSet

    /** Retrieve the set of `UnaryExpr` nodes where bounds checks can be eliminated */
    def getSafeChrCalls: Set[UnaryExpr[SemanticData]] = safeChrCalls.toSet  

    /** Retrieve the set of `Expr` nodes where overflow checks can be eliminated */
    def getSafeOverflowOps: Set[Expr[SemanticData]] = safeOverflowOps.toSet

    /** Check whether any error level diagnostics have been emitted */
    def hasErrors: Boolean = diagnostics.exists(_.severity == DiagnosticSeverity.Error)
}
