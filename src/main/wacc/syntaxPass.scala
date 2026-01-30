package wacc

object SyntaxPass {

    def syntaxCheck(prog: Program[SyntacticData],
                    errorLogger: ErrorLogger): Program[SyntacticData] = {

        prog.funcs.collectFirst { case func if !checkReturn(func.stmt) => func } match {
            case Some(func) =>
                val errorMsg = s"Function error: Function '${func.ident.name}' is not guaranteed to return"
                errorLogger.logError(func.info.pos, errorMsg)
                prog
            case None => prog
        }
    }

    private def checkReturn(funcStmt: Stmt[SyntacticData]) : Boolean = funcStmt match {
        case Stmt.Return(_, _) => true
        case Stmt.Exit(_, _) => true
        case Stmt.If(_, ifBlock, elseBlock, _) => checkReturn(ifBlock) && checkReturn(elseBlock)
        case Stmt.While(_, _, _) => false
        case Stmt.Block(stmts, _) => checkReturn(stmts.last)
        case _ => false
    }
        
}