package wacc.frontend.lsa

import wacc.frontend.*
import wacc.frontend.lsa.*

/* 
 * Diagnostics are the output of the static analyser. They describe problems
 * detected at compile time. There are three levels:
 *   - Error: A guaranteed runtime failure (eg division by zero). These halt the compilation
 *   - Warning: A potential runtime failure that cannot be proven to always
 *              occur (eg a variable that might be null depending on the branch)
 *   - Info: An improvement opportunity (e.g. a null check that can be
 *           removed because the pointer is provably non-null).
 */

/** The severity level of an analysis diagnostic */
enum DiagnosticSeverity {
    case Error, Warning, Info
}

/**  
  * Diagnostic produced by the static analyser.
  *
  * @param severity Level of the diagnostic
  * @param pos The source position where the issue was detected
  * @param message A description of the issue
  * @param detail Optional additional context
  */
case class AnalysisDiagnostic(
    severity: DiagnosticSeverity,
    pos: SourcePos,
    message: String,
    detail: String = ""
) {
    def format: String = {
        val severityStr = severity match {
            case DiagnosticSeverity.Error   => "Compiler error"
            case DiagnosticSeverity.Warning => "Compiler warning"
            case DiagnosticSeverity.Info    => "Compiler info"
        }
        val detailStr = if (detail.nonEmpty) s"\n$detail" else ""
        s"(Line ${pos(0)}, column ${pos(1)}) [$severityStr]: $message$detailStr"
    }
}

object AnalysisDiagnostic {

    /** Division by a value that is provably zero */
    def divisionByZero(
        pos: SourcePos, divisorName: String, divisorValue: AbstractValue
    ): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            pos,
            s"Division by zero: divisor '$divisorName' is ${divisorValue.describe}",
        )

    /** Dereferencing a value that is provably null */
    def nullDereference(pos: SourcePos, ptrName: String): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            pos,
            s"Null dereference: '$ptrName' is known to be null"
        )

    /** Array access with an index that is provably out of bounds */
    def arrayOutOfBounds(
        pos: SourcePos,
        arrayName: String,
        knownLength: Int,
        indexValue: AbstractValue
    ): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            pos,
            s"Array index out of bounds: '$arrayName' has length $knownLength " +
                s"but index is ${indexValue.describe}"
        )

    /** Accessing a pointer that has been freed */
    def useAfterFree(pos: SourcePos, ptrName: String): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            pos,
            s"Use after free: '$ptrName' has already been freed"
        )

    /** A null check can be safely removed because the pointer is provably non-null */
    def redundantNullCheck(pos: SourcePos, ptrName: String): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Info,
            pos,
            s"Null check on '$ptrName' is redundant: pointer is non-null"
        )

    /** A bounds check can be safely removed because the index is provably in range */
    def redundantBoundsCheck(pos: SourcePos, arrayName: String): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Info,
            pos,
            s"Bounds check on '$arrayName' is redundant: index is provably in range"
        )

    /** A branch of an if-else is unreachable because the condition is a known constant */
    def deadBranch(pos: SourcePos, whichBranch: String, condValue: Boolean): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Info,
            pos,
            s"Dead branch: '$whichBranch' branch is unreachable because condition is always ${condValue}"
        )

    /** Code after a return or exit statement is unreachable */
    def unreachableCode(pos: SourcePos): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Info,
            pos,
            "Unreachable code: statement follows a 'return' or 'exit' and will never execute"
        )

    /** Accessing a pair element with a type different from what was actually stored */
    def unsoundPairCoercion(
        pos: SourcePos,
        accessKind: String,
        actualType: SemanticType,
        expectedType: SemanticType
    ): AnalysisDiagnostic =
        AnalysisDiagnostic(
            DiagnosticSeverity.Warning,
            pos,
            s"Unsound pair coercion: '$accessKind' access expects '${expectedType.outputFormat}' " +
                s"but the pair actually stores '${actualType.outputFormat}'"
        )
}
