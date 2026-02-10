package wacc.frontend

/** Additional syntax checking stage of the frontend compiler pipeline.
  *
  * Checks the parser output for syntax errors not defined in the grammar and
  * not handled by Parsley but outlined by the language specification as syntax
  * errors. This only includes `return` statement validity as integer overflow
  * is handled by Parsley.
  */
object SyntaxPass {

  /** Checks whether a Program[SyntacticData] output by the parser satisfies
    * syntax rules not specified in the BNF grammar, logging syntax errors if it
    * does not. These rules only involve non-returning path checking in a
    * function body.
    *
    * @param prog:
    *   The Program AST node output by the parser
    * @param errorLogger:
    *   An error logger object for logging syntax errors
    *
    * @return
    *   Program[SyntacticData]: The original unmodified AST
    */
  def syntaxCheck(
      prog: Program[SyntacticData],
      errorLogger: ErrorLogger
  ): Program[SyntacticData] = {

    /* Log an error for the first non-returning function
           found and then stop */
    prog.funcs.collectFirst {
      case func if !checkReturn(func.stmt) => func
    } match {
      case Some(func) =>
        val errorMsg =
          s"\nFunction error: Function '${func.ident.name}' is not guaranteed to return"
        errorLogger.logSemanticError(func.ident.info.pos, errorMsg)
        prog
      case None => prog
    }
  }

  /** Checks whether a function's statement has a valid exit path under all
    * circumstances
    *
    * @param funcStmt:
    *   The function statement to check for valid return paths
    *
    * @return
    *   Boolean: true if all paths lead to a valid return, false otherwise
    */
  private def checkReturn(funcStmt: Stmt[SyntacticData]): Boolean =
    funcStmt match {
      case Stmt.Return(_, _)                 => true
      case Stmt.Exit(_, _)                   => true
      case Stmt.If(_, ifBlock, elseBlock, _) =>
        checkReturn(ifBlock) && checkReturn(elseBlock)
      case Stmt.While(_, _, _)  => false
      case Stmt.Block(stmts, _) => checkReturn(stmts.last)
      case _                    => false
    }

}
