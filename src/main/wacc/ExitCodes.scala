package wacc

/**
* Compiler exit codes
*/
object ExitCodes {
  opaque type ExitCode = Int
  val CompilerSuccess: ExitCode = 0
  val SyntaxError: ExitCode = 100
  val SemanticError: ExitCode = 200
  val BackendFailure: ExitCode = 300
  val CompilerFailure: ExitCode = -1

  def code(e: ExitCode): Int = e
}
