package wacc.frontend

import scala.collection.mutable
import wacc.backend.AARch64Limits

/**
  * State class for the tail recursive traversal containing the set of
  * `Call` nodes to which tail recursive call nodes are added to
  */
private class TailRecState {

    /* Sets of tail recursive call nodes */
    private val selfTailCalls: mutable.Set[Call[SemanticData]] = mutable.Set.empty
    private val generalTailCalls: mutable.Set[Call[SemanticData]] = mutable.Set.empty

    /** Retrieve immutable set of self and general tail recursive calls */
    def getTailCalls: (Set[Call[SemanticData]], Set[Call[SemanticData]]) =
        (selfTailCalls.toSet, generalTailCalls.toSet)

    /**
      * Add a self tail recursive call node to the set of self tailrec `Call` nodes
      *
      * @param callNode A self tail recursive call node to be added.
      */
    def addSelfTailCall(callNode: Call[SemanticData]): Unit = {
        selfTailCalls += callNode
    }

    /**
      * Add a general tail recursive call node to the set of general tailrec `Call` nodes
      *
      * @param callNode A general tail recursive call node to be added.
      */
    def addGeneralTailCall(callNode: Call[SemanticData]): Unit = {
        generalTailCalls += callNode
    }
}

private case class FuncContext(funcName: String, paramCount: Int)

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
    def checkTailRec(prog: Program[SemanticData]): (Set[Call[SemanticData]], Set[Call[SemanticData]]) = {
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
        given funcContext: FuncContext = FuncContext(func.ident.name, func.params.length)
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
                                (using funcContext: FuncContext,
                                       st: TailRecState): Unit = {
        stmt match
            case Stmt.If(_, ifBlock, elseBlock, _) =>
                checkTailRecStmt(ifBlock)
                checkTailRecStmt(elseBlock)
            case Stmt.While(_, body, _) => checkTailRecStmt(body)
            case Stmt.Block(stmts, _) => checkTailRecBlock(stmts)
            case _ =>
    }

    /**
      * If a `Call` node is self tail recursive, add it to the self tail rec set and if it
      * is generalised tail recursion, add it to the generalised tail recursion set.
      *
      * @param callNode The `Call` node being tested for tail recursion
      * @param retIdentName The name of the ident that is returned by the return statement
      * @param callIdentName The name of the ident that the `Call` node is set to
      * @param st The TailRecState
      * @param funcIdent The function ident node we are currently in
      * @param parentParamCount The number of parameters the parent/current function takes
      * @param childParamCount The number of parameters the `Call` node takes
      */
    private def registerIfTailRec(callNode: Call[SemanticData],
                                  retIdentName: String,
                                  callIdentName: String,
                                  childParamCount: Int)
                                 (using st: TailRecState,
                                        funcContext: FuncContext) = {

        /* Check we return the declaration/assignment immediately */
        if (retIdentName == callIdentName) {

            /* Self tail recursion */
            if (callNode.ident.name == funcContext.funcName) {
                st.addSelfTailCall(callNode)

              /* Generalised tail recursion. We must have this condition because unlike self
                 tailrec where we reuse the same frame, for general tailrec where the caller
                 can have different frame local vars and arg counts, we instead destroy our frame
                 and let the callee make a new frame so the net frame count is the same. So we can
                 only general tailrec optimise if the callee takes in more arguments such that
                 its slots in it's parent can be replaced with the child's arguments. */
            } else if (childParamCount <= AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS
                       || childParamCount <= funcContext.paramCount) {
                st.addGeneralTailCall(callNode)
            }
        }
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
                                 (using funcContext: FuncContext,
                                        st: TailRecState) = {

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
                Stmt.Decl(_, declIdent, callNode @ Call(_, argList, _), _),
                Stmt.Return(Ident(returnName, _), _)
            ) => registerIfTailRec(callNode, returnName, declIdent.name, argList.length)
        
            /* Accounting for the assignment (instead of declaration) tail recursive case */
            case List(
                Stmt.Asgn(Ident(lvalName, _), callNode @ Call(_, argList, _), _), 
                Stmt.Return(Ident(returnName, _), _)
            ) => registerIfTailRec(callNode, returnName, lvalName, argList.length)

            case _ => () /* No tail recursion, do nothing */
    }
}