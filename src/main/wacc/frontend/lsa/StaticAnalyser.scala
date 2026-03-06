package wacc.frontend.lsa

import wacc.frontend.*
import wacc.frontend.lsa.*

import scala.collection.mutable.ListBuffer

/*
 * Performs local static analysis on the typed AST to catch
 * runtime errors at compile time and identify redundant runtime checks.
 *
 * The analyser runs after the type checker and before the backend. It walks
 * each function body and the main program body, creating an `AnalysisState`
 * through each statement. At each program point the state records what we
 * know about every variable (its `AbstractValue`), and we use this knowledge
 * to detect guaranteed errors and to prove that certain runtime checks are unnecessary
 */

/** Context accumulated during the analysis traversal of the AST.
  *
  * @param diagnostics Accumulated analysis diagnostics
  */
class AnalyserState {

    private val diagnostics: ListBuffer[AnalysisDiagnostic] = ListBuffer.empty

    /** Emit a diagnostic finding. */
    def emit(diagnostic: AnalysisDiagnostic): Unit = diagnostics += diagnostic

    /** Retrieve all diagnostics found so far */
    def getAllDiagnostics: List[AnalysisDiagnostic] = diagnostics.toList

    /** Check whether any error level diagnostics have been emitted */
    def hasErrors: Boolean = diagnostics.exists(_.severity == DiagnosticSeverity.Error)
}

/** Result of running the static analyser on a program.
  *
  * @param diagnostics All diagnostics found
  * @param hasErrors True if any error diagnostics were found
  */
case class AnalysisResult(
    diagnostics: List[AnalysisDiagnostic],
    hasErrors: Boolean
)

/** The static analyser entry point */
object StaticAnalyser {

    /** Analyse an entire program for compile-time detectable errors.
      *
      * @param prog The typed AST from the type checker
      * @return An `AnalysisResult` containing all findings
      */
    def analyse(prog: Program[SemanticData]): AnalysisResult = {
        given analyserState: AnalyserState = AnalyserState()

        /* Analyse each function body with parameters initialised to `Unknown`` */
        prog.funcs.foreach(analyseFunc)

        analyseStmt(prog.stmt)(using AnalysisState.empty, analyserState)

        AnalysisResult(
            analyserState.getAllDiagnostics,
            analyserState.hasErrors
        )
    }

    /** Analyse a function definition.
      *
      * Creates an initial state where each parameter is bound to `Unknown` and
      * analyses the function body.
      *
      * @param func The function AST node to analyse
      * @param analyserState The mutable diagnostics accumulator
      */
    private def analyseFunc(
        func: Func[SemanticData]
    )(using analyserState: AnalyserState): Unit = {

        /* Build initial state with each parameter bound to Unknown */
        val initialState = func.params.foldLeft(AnalysisState.empty) { (state, param) =>
            VariableId.fromIdent(param.ident) match {
                case Some(varId) => state.update(varId, Unknown)
                case None        => state
            }
        }

        analyseStmt(func.stmt)(using initialState, analyserState)
    }

    /** Analyse a statement, updating the state and emitting diagnostics.
      *
      * This is the core transfer function of the analysis. Each statement type
      * transforms the state in a specific way (eg. assignments update the
      * target variable, `read` invalidates it, `free` marks it as freed).
      *
      * @param stmt The statement to analyse
      * @param state The abstract state before this statement
      * @param analyserState The mutable diagnostics accumulator
      * 
      * @return The abstract state after this statement
      */
    private def analyseStmt(
        stmt: Stmt[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState =
        (stmt: @unchecked) match {
            case Stmt.Skip(_) => state
            case Stmt.Decl(_, ident, rval, _) => analyseDecl(ident, rval)
            case Stmt.Asgn(lval, rval, _) => analyseAsgn(lval, rval)
            case Stmt.Read(lval, _) => analyseRead(lval)
            case Stmt.Free(expr, _) => analyseFree(expr)
            case Stmt.Return(expr, _) => analyseTerminating(expr)
            case Stmt.Exit(expr, _) => analyseTerminating(expr)
            case Stmt.Print(expr, _) => analysePrint(expr)
            case Stmt.Println(expr, _) => analysePrint(expr)
            case Stmt.If(cond, ifBlock, elseBlock, _) => analyseIf(cond, ifBlock, elseBlock)
            case Stmt.While(cond, body, _) => analyseWhile(cond, body)
            case Stmt.Block(stmts, _) => analyseBlock(stmts)
        }

    /** Analyse a variable declaration `type ident = rval`
      *
      * Evaluates the rvalue to an abstract value and binds the
      * new variable to it.
      */
    private def analyseDecl(
        ident: Ident[SemanticData],
        rval: RValue[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {
        val abstractVal = evaluateRValue(rval)
        VariableId.fromIdent(ident) match {
            case Some(varId) => state.update(varId, abstractVal)
            case None        => state
        }
    }

    /** Analyse an assignment `lval = rval`
      *
      * Evaluates the rval and updates the target variable (lval).
      * For complex lvalues (array elements, pair elements), we invalidate
      * instead since we don't track heap contents.
      */
    private def analyseAsgn(
        lval: LValue[SemanticData],
        rval: RValue[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {
        val abstractVal = evaluateRValue(rval)
        lval match {
            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident) match {
                    case Some(varId) => state.update(varId, abstractVal)
                    case None        => state
                }
            /* Array element and pair element assignments modify heap
               contents which we don't track, so the state is unchanged */
            case _: ArrayElem[SemanticData] => state
            case _: PairElem[SemanticData]  => state
        }
    }

    /** Analyse a `read` statement.
      *
      * `read` gets its value from stdin which is unknowable at compile time,
      * so we must invalidate whatever we knew about the target variable.
      */
    private def analyseRead(
        lval: LValue[SemanticData]
    )(using state: AnalysisState): AnalysisState =
        lval match {
            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident) match {
                    case Some(varId) => state.invalidate(varId)
                    case None        => state
                }
            /* reading into array/pair elements doesn't affect local variable state */
            case _ => state
        }

    /** Analyse a `free` statement.
      *
      * Marks the freed pointer as `Freed` so that subsequent accesses
      * can be flagged as use after free errors.
      */
    private def analyseFree(
        expr: Expr[SemanticData]
    )(using state: AnalysisState): AnalysisState =
        expr match {
            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident) match {
                    case Some(varId) => state.update(varId, Freed)
                    case None        => state
                }
            case _ => state
        }

    /** Analyse a `return` or `exit` statement.
      *
      * Evaluates the return/exit expression and marks the state as unreachable
      * since no further statements execute.
      */
    private def analyseTerminating(
        expr: Expr[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {
        evaluateExpr(expr)
        state.markUnreachable
    }

    /** Analyse a `print` or `println` statement.
      *
      * Evaluates the expression but does not change the state
      */
    private def analysePrint(
        expr: Expr[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {
        evaluateExpr(expr)
        state
    }

    /** Analyse an if statement.
      *
      * Evaluates the condition, then analyses both branches independently
      * starting from the current state, and joins the two resulting states.
      *
      * If the condition is a known boolean, only the reachable branch is
      * relevant (dead branch elimination).
      */
    private def analyseIf(
        cond: Expr[SemanticData],
        ifBlock: Stmt[SemanticData],
        elseBlock: Stmt[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {
        val condVal = evaluateExpr(cond)

        condVal match {
            /* Condition is provably true and only the then branch executes */
            case BoolConstant(true) =>
                analyseStmt(ifBlock)

            /* Condition is provably false and only the else branch executes */
            case BoolConstant(false) =>
                analyseStmt(elseBlock)

            /* Condition is unknown so analyse both branches and join */
            case _ =>
                val thenState = analyseStmt(ifBlock)
                val elseState = analyseStmt(elseBlock)(using state, analyserState)
                thenState.join(elseState)
        }
    }

    /** Analyse a while statement.
      *
      * Loops are a bit hard for static analysis because the body
      * can execute an arbitrary number of times and we can't determine 
      * it easily. The simplest sound approach is to identify all variables
      * that could be modified inside the loop body and set them to `Unknown` before and
      * after the loop. This loses precision but is always safe.
      */
    private def analyseWhile(
        cond: Expr[SemanticData],
        body: Stmt[SemanticData]
    )(using state: AnalysisState): AnalysisState = {

        /* Collect all variables that could be modified inside the loop body */
        val modifiedVars = collectModifiedVariables(body)

        /* Invalidate all potentially modified variables */
        state.invalidateAll(modifiedVars)
    }

    /** Analyse a block of statements
      *
      * Folds through the statements sequentially.
      * Records which variables existed before the block so we can clean
      * up inner scope variables when leaving.
      */
    private def analyseBlock(
        stmts: List[Stmt[SemanticData]]
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState = {

        /* Remember variables in scope before the block for cleanup */
        val outerScope = state.trackedVariables

        /* Thread state through each statement sequentially */
        val finalState = stmts.foldLeft(state) { (currentState, stmt) =>
            if (currentState.unreachable) currentState
            else analyseStmt(stmt)(using currentState, analyserState)
        }

        /* Clean up variables declared inside this block but preserve
           any state changes to outer scope variables */
        val innerVars = finalState.trackedVariables -- outerScope
        if (innerVars.isEmpty) finalState
        else finalState.restrictTo(outerScope ++ finalState.trackedVariables.intersect(outerScope))
    }

    /** Evaluate an expression to its abstract value.
      *
      * @param expr The expression to evaluate
      * @param state The current abstract state
      * @param analyserState The diagnostics accumulator
      *
      * @return The abstract value of the expression
      */
    private def evaluateExpr(
        expr: Expr[SemanticData]
    )(using state: AnalysisState): AbstractValue =
        expr match {
            case IntLiter(value, _)  => IntConstant(value)
            case BoolLiter(value, _) => BoolConstant(value)
            case CharLiter(value, _) => IntConstant(value.toInt)
            case StrLiter(_, _)      => Unknown /* We don't wanna deal with strings */
            case PairLiter(_)        => wacc.frontend.lsa.Null

            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident) match {
                    case Some(varId) => state.lookup(varId)
                    case None        => Unknown
                }

            case UnaryExpr(op, inner, _)          => ???
            case BinaryExpr(op, left, right, _)   => ???
            case ArrayElem(ident, indices, _)      => ???
        }

    /** Evaluate an RValue to its abstract value.
      *
      * @param rval The RValue to evaluate
      * @param state The current abstract state
      * @param analyserState The diagnostics accumulator
      * 
      * @return The abstract value of the RValue
      */
    private def evaluateRValue(
        rval: RValue[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AbstractValue =
        rval match {
            case expr: Expr[SemanticData] => evaluateExpr(expr)
            case NewPair(_, _, _) => NonNull /* newpair is always nonnull */
            case ArrayLiter(elements, _) =>
                /* Array literals have a known length and are nonnull */
                AbstractValue.fromArrayLiteral(elements.length)
            case Call(_, _, _) => Unknown /* we do local analysis, can't know function returns */
            case PairElem(_, _, _) => Unknown 
        }


    /** Collect all variables that could be modified inside a statement.
      *
      *
      * @param stmt The statement to scan
      * @return The set of variable ids that could be modified
      */
    private def collectModifiedVariables(stmt: Stmt[SemanticData]): Set[VariableId] =
        stmt match {
            case Stmt.Skip(_) => Set.empty
            case Stmt.Decl(_, ident, _, _) => VariableId.fromIdent(ident).toSet
            case Stmt.Asgn(lval, _, _) => collectLValueTarget(lval)
            case Stmt.Read(lval, _) => collectLValueTarget(lval)
            case Stmt.Free(expr, _) =>
                expr match {
                    case ident: Ident[SemanticData] =>
                        VariableId.fromIdent(ident).toSet
                    case _ => Set.empty
                }
            case Stmt.Return(_, _) | Stmt.Exit(_, _) => Set.empty
            case Stmt.Print(_, _) | Stmt.Println(_, _) => Set.empty
            case Stmt.If(_, ifBlock, elseBlock, _) =>
                collectModifiedVariables(ifBlock) ++ collectModifiedVariables(elseBlock)
            case Stmt.While(_, body, _) =>
                collectModifiedVariables(body)
            case Stmt.Block(stmts, _) =>
                stmts.flatMap(collectModifiedVariables).toSet
        }

    /** Extract the variable id from an lvalue target, if it is a simple identifier.
      *
      * For complex lvalues (array elements/pair elements), we extract the
      * base identifier since modifying `a[i]` or `fst p` "modifies" the array/pair variable.
      *
      * @param lval The lvalue to extract from
      * @return The set of variable ids that this lvalue could modify
      */
    private def collectLValueTarget(lval: LValue[SemanticData]): Set[VariableId] =
        lval match {
            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident).toSet
            case ArrayElem(ident, _, _) =>
                VariableId.fromIdent(ident).toSet
            case PairElem(_, innerLval, _) =>
                collectLValueTarget(innerLval)
        }
}
