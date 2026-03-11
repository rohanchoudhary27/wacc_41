package wacc.frontend.lsa

/**
  * The three way outcome of a static safety check.
  *
  * Every compile time safety check in the static analyser reduces to one of
  * three mutually exclusive conclusions:
  *
  *  - DefiniteError — the operation provably fails at runtime (e.g. integer
  *                    overflow, out of bounds index, null dereference).
  *                    The analyser should emit a compile-time diagnostic.
  *
  *  - ProvablySafe — the operation provably succeeds at runtime.
  *                   The analyser should record the AST node so the backend
  *                   can omit the corresponding runtime guard.
  *
  *  - Indeterminate — the abstract values are insufficiently precise to
  *                    determine the outcome at compile time. No diagnostic
  *                    is emitted and no guard is eliminated; the backend will
  *                    emit the normal runtime check.
  *
  * Call sites compute a `SafetyVerdict` from the abstract values of the relevant
  * subexpressions, then pass it to AnalyserState.applyVerdict together with
  * the two outcome callbacks:
  *
  * ```
  *   val verdict = SafetyVerdict.fromChecks(
  *       isError  = AbstractValue.doesProvablyOverflowNeg(operandVal),
  *       isSafe   = AbstractValue.isProvablySafeNeg(operandVal)
  *   )
  *   analyserState.applyVerdict(verdict)(
  *       onError = _.emit(AnalysisDiagnostic.negationOverflow(pos, operandVal)),
  *       onSafe  = _.markSafeOverflowOp(expr)
  *   )
  * ```
  */
sealed trait SafetyVerdict

object SafetyVerdict {

    /** The operation is provably unsafe: a runtime error would always occur. */
    case object DefiniteError extends SafetyVerdict

    /** The operation is provably safe: the runtime guard can be eliminated. */
    case object ProvablySafe extends SafetyVerdict

    /** The abstract values are too imprecise to decide either way. */
    case object Indeterminate extends SafetyVerdict

    /**
      * Constructs a `SafetyVerdict` from the two Boolean predicates that the
      * static analyser already computes.
      *
      * The two predicates are mutually exclusive by construction (a value cannot
      * simultaneously be provably error and provably safe), though it still is
      * "defensive" against `isError`.
      *
      * @param isError `true` when the abstract values prove a runtime error occurs.
      * @param isSafe  `true` when the abstract values prove the operation is safe.
      * @return The corresponding `SafetyVerdict`.
      */
    def fromChecks(isError: Boolean, isSafe: Boolean): SafetyVerdict =
        if (isError) DefiniteError
        else if (isSafe) ProvablySafe
        else Indeterminate
}