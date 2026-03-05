package wacc.frontend

import scala.collection.mutable

/**
  * State class for the tail recursive traversal containing the set of
  * `Call` nodes to which tail recursive call nodes are added to
  */
private class TailRecState {

    /* Set of tail recursive call nodes */
    private val tailCalls: mutable.Set[Call[SemanticData]] = mutable.Set.empty

    /** Retrieve immutable set of tail recursive calls */
    def getTailCalls: Set[Call[SemanticData]] = tailCalls.toSet

    /**
      * Add a tail recursive call node to the set of tailrec `Call` nodes
      *
      * @param callNode A tail recursive call node to be added.
      */
    def addTailCall(callNode: Call[SemanticData]): Unit = {
        tailCalls += callNode
    }
}

/**
  * Checks for tail recursive `Call` nodes in the `Func` nodes. Part of the
  * WACC Extensions milestone.
  */
object CheckTailRec {

    /**
      * Checks each `Func` node in the AST and if it is a tail recursive call, adds it to
      * the returned set of `Call` nodes
      *
      * @param prog The input typed AST from the typechecker
      * @return The set of `Call` AST nodes which are tail recursive
      */
    def checkTailRec(prog: Program[SemanticData]): Set[Call[SemanticData]] = {
        given st: TailRecState = TailRecState()
        prog.funcs.foreach(checkTailRecFunc)
        st.getTailCalls
    }

    /**
      * Check a `Func` AST node for tail recursion call nodes.
      * If any are found, they are added to the TailRecState.
      *
      * @param func
      */
    private def checkTailRecFunc(func: Func[SemanticData])(using TailRecState): Unit = {
        given funcIdent: Ident[SemanticData] = func.ident
        checkTailRecStmt(func.stmt)
    }

    /**
      * Check a `Stmt` AST node for tail recursion call nodes.
      * If any are found, they are added to the TailRecState.
      *
      * @param stmt The statement node to check for tail recursion in
      * @param funcIdent The `Ident` of the function the statement is inside
      * @param st The TailRecState object
      */
    private def checkTailRecStmt(stmt: Stmt[SemanticData])
                                (using funcIdent: Ident[SemanticData], st: TailRecState): Unit = {
        stmt match
            case Stmt.If(_, ifBlock, elseBlock, _) =>
                checkTailRecStmt(ifBlock)
                checkTailRecStmt(elseBlock)
            case Stmt.While(_, body, _) => checkTailRecStmt(body)
            case Stmt.Block(stmts, _) => checkTailRecBlock(stmts)
            case _ =>
    }

    /**
      * Check a `Stmt.Block` AST node for tail recursion call nodes.
      * If any are found, they are added to the TailRecState.
      *
      * @param stmt The list of statement nodes to check for tail recursion in
      * @param funcIdent The `Ident` of the function the statement is inside
      * @param st The TailRecState object
      */
    private def checkTailRecBlock(stmts: List[Stmt[SemanticData]])
                                 (using funcIdent: Ident[SemanticData], st: TailRecState) = {

        /* Check each statement in the list for tail recursion */
        stmts.foreach(checkTailRecStmt)

        stmts.takeRight(2) match

            /* Tail recursion in WACC takes the form:
               ```
               int x = call foo (n-1);
               return x
               ```
               where these lines are the last two lines of the function body, and are inside
               of a function `foo`.
            */
            case List(
                Stmt.Decl(_, declIdent, callNode @ Call(callFuncIdent, _, _), _),
                Stmt.Return(Ident(returnName, _), _)
            ) => {
                /* Declaration must be what is returned and the called function must be the
                   current function (self-recursion) */
                if (declIdent.name == returnName && callFuncIdent.name == funcIdent.name) {
                    st.addTailCall(callNode)
                }
            }
        
            /* Accounting for the assignment (instead of declaration) tail recursive case */
            case List(
                Stmt.Asgn(Ident(lvalName, _), callNode @ Call(callFuncIdent, _, _), _), 
                Stmt.Return(Ident(returnName, _), _)
            ) => {
                if (lvalName == returnName && callFuncIdent.name == funcIdent.name) {
                    st.addTailCall(callNode)
                }
            }

            case _ =>
    }
}