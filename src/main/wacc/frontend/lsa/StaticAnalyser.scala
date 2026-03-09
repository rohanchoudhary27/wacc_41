package wacc.frontend.lsa

import wacc.frontend.*
import wacc.frontend.lsa.*
import wacc.frontend.TypeChecker.~

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

    /** Check whether any error level diagnostics have been emitted */
    def hasErrors: Boolean = diagnostics.exists(_.severity == DiagnosticSeverity.Error)
}

/** Result of running the static analyser on a program.
  *
  * @param diagnostics All diagnostics found
  * @param hasErrors True if any error diagnostics were found
  * @param safePairAccesses `PairElem` AST nodes where the base pointer is provably non-null.
  *                          The flattener can skip emitting `NullCheck` instructions for these
  *                          nodes. Follows the same pattern as `tailRecCalls` in `FrontendOptimState`.
  * @param safeDivisions `BinaryExpr` AST nodes (Div/Mod) where the divisor is provably non-zero.
  *                      The flattener can emit unchecked division/modulo for these nodes.
  * @param safeArrayAccesses `ArrayElem` AST nodes where the index is provably within bounds.
  */
case class AnalysisResult(
    diagnostics: List[AnalysisDiagnostic],
    hasErrors: Boolean,
    safePairAccesses: Set[PairElem[SemanticData]] = Set.empty,
    safeDivisions: Set[BinaryExpr[SemanticData]] = Set.empty,
    safeArrayAccesses: Set[ArrayElem[SemanticData]] = Set.empty
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
            analyserState.hasErrors,
            analyserState.getSafePairAccesses,
            analyserState.getSafeDivisions,
            analyserState.getSafeArrayAccesses
        )
    }

    /** Run the static analysis and report any error level diagnostics to
      * the given `ErrorLogger`.
      *
      * This is the main entry point called of the static analyser. It runs
      * the local static analysis and routes any error diagnostics into the existing
      * error reporting infrastructure. Info and warning diagnostics are
      * printed to stderr but do not cause compilation to fail.
      *
      * @param prog The typed AST from the type checker
      * @param errorLogger The error logger to report errors to
      *
      * @return The `AnalysisResult`
      */
    def run(prog: Program[SemanticData], errorLogger: ErrorLogger): AnalysisResult = {
        val result = analyse(prog)

        /* Put error level diagnostics into the ErrorLogger so the
           compiler pipeline treats them as semantic errors */
        result.diagnostics.foreach { diag =>
            diag.severity match {
                case DiagnosticSeverity.Error =>
                    errorLogger.logSemanticError(diag.pos, diag.message, diag.detail)
                case DiagnosticSeverity.Warning =>
                    Console.err.println(diag.format)
                case DiagnosticSeverity.Info =>
                    Console.out.println(diag.format)
            }
        }

        result
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
            case p: PairElem[SemanticData]  => 
                checkPairElemSafety(p)
                state
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
    )(using state: AnalysisState, analyserState: AnalyserState): AnalysisState =
        val ptrVal = evaluateExpr(expr)

        val ptrName = expr match {
            case ident: Ident[SemanticData] => ident.name
            case _ => "expression"
        }

        /* Check if the freed pointer is null or previously freed, and emit the error */
        emitPointerErrorIfStale(ptrName, ptrVal, expr.info)

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
                /* the else block is dead bec only the then block will execute */
                analyserState.emit(
                    AnalysisDiagnostic.deadBranch(elseBlock.info.pos, "else", true)
                )
                analyseStmt(ifBlock)

            /* Condition is provably false and only the else branch executes */
            case BoolConstant(false) =>
                /* the then block is dead bec only the else block will execute */
                analyserState.emit(
                    AnalysisDiagnostic.deadBranch(ifBlock.info.pos, "then", false)
                )
                analyseStmt(elseBlock)

            /* Condition is unknown so analyse both branches and join */
            case _ =>
                /*  Apply condition refinement to narrow the state in each branch */
                val (thenRefinement, elseRefinement) = extractRefinements(cond)
                val thenEntryState = applyRefinement(state, thenRefinement)
                val elseEntryState = applyRefinement(state, elseRefinement)

                val thenState = analyseStmt(ifBlock)(using thenEntryState, analyserState)
                val elseState = analyseStmt(elseBlock)(using elseEntryState, analyserState)
                thenState.join(elseState)
        }
    }

    /*
     * When a condition in an if statement gives us information about a
     * variable (eg `x == 0`, `p != null`, `!b`), we can narrow the
     * abstract state in each branch to be a bit more precise.
     *
     * A `Refinement` is a (VariableId, AbstractValue) update to
     * apply to the state before entering a branch. `extractRefinements`
     * pattern matches on the condition expression to produce refinements
     * for both the then branch and the else branch */
    private type Refinement = Option[(VariableId, AbstractValue)]

    /** Extract refinements for both branches from a condition expression.
      * 
      * Supported patterns for condition refinement include:
      *  - `x == 0`: the `then` branch is `x = IntConstant(0)`, the `else`
      *              branch has no refinement because `x` can be many other values
      *  - `x == null`: the `then` branch is `x = Null`, the `else` branch
      *                  is `x = NonNull`
      *  - `x != null`: the `then` branch is `x = NonNull`, the `else` branch
      *                  is `x = Null`
      *  - `x == true`: the `then` branch is `x = BoolConstant(true)`, the `else`
      *                 branch is `x = BoolConstant(false)`
      *  - `!cond`: swaps the refinement from `cond`
      *  - `b`:  the `then` branch is `b = true`, the `else` branch
      *          is `b = false` 
      *
      * @param cond The condition expression from the if statement
      * 
      * @return A pair of (then-refinement, else-refinement)
      */
    private def extractRefinements(
        cond: Expr[SemanticData]
    )(using state: AnalysisState): (Refinement, Refinement) = cond match {

        /* x == <value> or <value> == x */
        case BinaryExpr(BinaryOperator.EQ, left, right, _) =>
            extractEqualityRefinements(left, right)

        /* x != <value> or <value> != x */
        case BinaryExpr(BinaryOperator.NEQ, left, right, _) =>
            val (thenRef, elseRef) = extractEqualityRefinements(left, right)
            (elseRef, thenRef)

        /* !cond */
        case UnaryExpr(UnaryOperator.Not, inner, _) =>
            val (thenRef, elseRef) = extractRefinements(inner)
            (elseRef, thenRef)

        /* if b then ... else ... */
        case ident: Ident[SemanticData] =>
            val varId = VariableId.fromIdent(ident)
            (
                varId.map(_ -> BoolConstant(true)),
                varId.map(_ -> BoolConstant(false))
            )

        case _ => (None, None)
    }

    /** Extract refinements from an equality (==)
      *
      * Handles the patterns x == literal and literal == x.
      *
      * For the then branch, the variable is narrowed to the literal's value.
      * For the else branch, we can apply further narrowing too (check above scaladocs)
      *
      * @return (then refinement, else refinement) for the `==` case
      */
    private def extractEqualityRefinements(
        left: Expr[SemanticData],
        right: Expr[SemanticData]
    )(using state: AnalysisState): (Refinement, Refinement) = {

        /* try to match (variable, known value) in either order */
        val varAndValue: Option[(VariableId, AbstractValue)] = (left, right) match {
            case (ident: Ident[SemanticData], other) =>
                for {
                    varId <- VariableId.fromIdent(ident)
                    value = evaluateExprPure(other)
                } yield (varId, value)

            case (other, ident: Ident[SemanticData]) =>
                for {
                    varId <- VariableId.fromIdent(ident)
                    value = evaluateExprPure(other)
                } yield (varId, value)

            case _ => None
        }

        varAndValue match {
            case Some((varId, knownValue)) =>
                val thenRef = Some(varId -> knownValue)
                val elseRef = negateRefinement(varId, knownValue)
                (thenRef, elseRef)

            case None => (None, None)
        }
    }

    /** Compute the else branch refinement from an equality check
      *
      * The else branch is the "negated" version of the then branch, where the opposite 
      * boolean truth value applies
      */
    private def negateRefinement(varId: VariableId, value: AbstractValue): Refinement =
        value match {
            case wacc.frontend.lsa.Null => Some(varId -> NonNull)
            case BoolConstant(v) => Some(varId -> BoolConstant(!v))
            case _ => None
        }

    /** Apply a refinement to the state, narrowing a variable's abstract value
      *
      * If the refinement is `None`, the state is returned unchanged
      */
    private def applyRefinement(state: AnalysisState, refinement: Refinement): AnalysisState =
        refinement match {
            case Some((varId, value)) => state.update(varId, value)
            case None                 => state
        }

    
    /** Evaluate an expression to its abstract value without emitting diagnostics
      *
      * I.e. it doesn't have any side effects 
      * */
    private def evaluateExprPure(
        expr: Expr[SemanticData]
    )(using state: AnalysisState): AbstractValue = expr match {
        case IntLiter(value, _)  => IntConstant(value)
        case BoolLiter(value, _) => BoolConstant(value)
        case CharLiter(value, _) => IntConstant(value.toInt)
        case PairLiter(_)        => wacc.frontend.lsa.Null
        case ident: Ident[SemanticData] =>
            VariableId.fromIdent(ident).map(state.lookup).getOrElse(Unknown)
        case _ => Unknown
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
        val postLoopState = state.invalidateAll(modifiedVars)

        /* After the loop, we know the condition is false (otherwise the loop
           would have continued). Extract the else refinement from the condition
           and apply it to the post loop state  */
        val (_, elseRefinement) = extractRefinements(cond)(using postLoopState)
        applyRefinement(postLoopState, elseRefinement)
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

        var unreachableWarned = false

        /* Thread state through each statement sequentially */
        val finalState = stmts.foldLeft(state) { (currentState, stmt) =>
            if (currentState.unreachable) {
                if (!unreachableWarned) {
                    analyserState.emit(AnalysisDiagnostic.unreachableCode(stmt.info.pos))
                    unreachableWarned = true
                }
                currentState
            }
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
    )(using state: AnalysisState, analyser: AnalyserState): AbstractValue =
        expr match {
            case IntLiter(value, _)  => IntConstant(value)
            case BoolLiter(value, _) => BoolConstant(value)
            case CharLiter(value, _) => IntConstant(value.toInt)
            case StrLiter(_, _) => Unknown /* We don't wanna deal with strings */
            case PairLiter(_) => wacc.frontend.lsa.Null

            case ident: Ident[SemanticData] =>
                VariableId.fromIdent(ident).map(state.lookup).getOrElse(Unknown)

            case UnaryExpr(op, inner, _) => evaluateUnaryExpr(op, inner)
            case b: BinaryExpr[SemanticData] => evaluateBinaryExpr(b)
            case a: ArrayElem[SemanticData] => evaluateArrayElem(a)
        }
    
    /** Evaluate a unary expression to its abstract value.
      *
      * Each unary operator maps to an abstract operation defined in
      * `AbstractValue`. The inner expression is evaluated first, then
      * the abstract operation is applied.
      *
      * @param op The unary operator
      * @param inner The operand expression
      * 
      * @return The abstract value of the unary expression
      */
    private def evaluateUnaryExpr(
        op: UnaryOperator,
        inner: Expr[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AbstractValue = {
        val operandVal = evaluateExpr(inner)

        op match {
            case UnaryOperator.Neg => AbstractValue.neg(operandVal)
            case UnaryOperator.Not => AbstractValue.not(operandVal)
            case UnaryOperator.Len => AbstractValue.len(operandVal)
            case UnaryOperator.Ord => AbstractValue.ord(operandVal)
            case UnaryOperator.Chr => AbstractValue.chr(operandVal)
        }
    }
    
    /** Evaluate a binary expression to its abstract value.
      *
      * Evaluates both operands, then applies the corresponding
      * operation. Division and modulo are special: they return `None` when
      * the divisor is provably zero, which triggers a diagnostic.
      *
      * @param b The `BinaryExpr` AST node
      * 
      * @return The abstract value of the binary expression
      */
    private def evaluateBinaryExpr(
        b: BinaryExpr[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AbstractValue = {
        val BinaryExpr(op, left, right, info) = b
        val leftVal = evaluateExpr(left)
        val rightVal = evaluateExpr(right)

        op match {
            /* Arithmetic operators */
            case BinaryOperator.Add => AbstractValue.add(leftVal, rightVal)
            case BinaryOperator.Sub => AbstractValue.sub(leftVal, rightVal)
            case BinaryOperator.Mul => AbstractValue.mul(leftVal, rightVal)

            /* Division and modulo: check for division by zero */
            case BinaryOperator.Div =>
                evaluateDivisionOp(b, leftVal, rightVal, AbstractValue.div)

            case BinaryOperator.Mod =>
                evaluateDivisionOp(b, leftVal, rightVal, AbstractValue.mod)

            /* Comparison operators */
            case BinaryOperator.GT  => AbstractValue.gt(leftVal, rightVal)
            case BinaryOperator.GTE => AbstractValue.gte(leftVal, rightVal)
            case BinaryOperator.LT  => AbstractValue.lt(leftVal, rightVal)
            case BinaryOperator.LTE => AbstractValue.lte(leftVal, rightVal)
            case BinaryOperator.EQ  => AbstractValue.eq(leftVal, rightVal)
            case BinaryOperator.NEQ => AbstractValue.neq(leftVal, rightVal)

            /* Logical operators */
            case BinaryOperator.And => AbstractValue.and(leftVal, rightVal)
            case BinaryOperator.Or  => AbstractValue.or(leftVal, rightVal)
        }
    }

    /** Evaluate a division or modulo operation, emitting a diagnostic
      * if the divisor is provably zero.
      *
      * Division and modulo return `Option[AbstractValue]` from `AbstractValue`,
      * `None` means provably zero divisor.
      *
      * @param b The `BinaryExpr` AST node 
      * @param leftVal The abstract value of the dividend
      * @param rightVal The abstract value of the divisor
      * @param rightExpr The divisor expression (for extracting a name for diagnostics)
      * @param info The source position of the binary expression
      * @param divOp The abstract operation (either `AbstractValue.div` or `.mod`)
      * 
      * @return The abstract value of the result, or `Unknown` if an error was emitted
      */
    private def evaluateDivisionOp(
        b: BinaryExpr[SemanticData],
        leftVal: AbstractValue,
        rightVal: AbstractValue,
        divOp: (AbstractValue, AbstractValue) => Option[AbstractValue]
    )(using analyserState: AnalyserState): AbstractValue = {
        val BinaryExpr(_, _, rightExpr, info) = b
        divOp(leftVal, rightVal) match {
            case Some(result) => 
                if (AbstractValue.isDefinitelyNonZero(rightVal)) {
                    analyserState.markSafeDivision(b)
                }
                result
            case None =>
                val divisorName = rightExpr match {
                    case ident: Ident[SemanticData] => ident.name
                    case _ => "<expression>"
                }
                analyserState.emit(
                    AnalysisDiagnostic.divisionByZero(info.pos, divisorName, rightVal)
                )
                Unknown
        }
    }

    /** Evaluate an array element access, checking bounds and marking
      * safe accesses for runtime check elimination.
      *
      * For a 1d access a[i], if both the array's length
      * and the index are known, we can check whether the access is provably
      * in bounds, provably out of bounds, or unknown (keep runtime check).
      *
      * For multi diemansional accesses a[i][j], only the first index
      * can be checked against the array's known length because we don't
      * track the lengths of inner arrays (they are heap allocated values
      * we don't follow). The remaining indices are evaluated for their
      * side effects but cannot be bounds checked.
      *
      * @param array The `ArrayElem` AST node (for recording in safe set)
      * @return
      *   Always `Unknown` — we don't track individual array element values
      */
    private def evaluateArrayElem(
        array: ArrayElem[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): AbstractValue = {
        val ArrayElem(ident, indices, info) = array

        val arrayVal = VariableId.fromIdent(ident)
            .map(state.lookup)
            .getOrElse(Unknown)

        val indexVals = indices.map(evaluateExpr)

        /* We can only check the first index against the known array length.
           After the first indexing, we get an inner value whose length is
           unknown (we don't track array contents). */
        (arrayVal, indexVals.headOption) match {
            case (ArrayInfo(Some(length)), Some(indexVal)) =>
                if (AbstractValue.isProvablyOutOfBounds(indexVal, length)) {
                    analyserState.emit(
                        AnalysisDiagnostic.arrayOutOfBounds(
                            info.pos, ident.name, length, indexVal
                        )
                    )
                } else if (AbstractValue.isProvablyInBounds(indexVal, length)) {
                    analyserState.markSafeArrayAccess(array)
                }
            case _ => () /* we keep runtime check for unkown cases */
        }
        Unknown /* we don't track individual array elems in state so return unknown */
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
            case NewPair(e1, e2, _) => 
                AbstractValue.fromNewPair(e1.expressionType, e2.expressionType)
            case ArrayLiter(elements, _) =>
                /* Array literals have a known length and are nonnull */
                AbstractValue.fromArrayLiteral(elements.length)
            case Call(_, _, _) => Unknown /* we do local analysis, can't know function returns */
            case p @ PairElem(isFst, lval, info) => 
                /* Checks if the pair pointer being accessed via fst/snd is provably
                    Null (emits null dereference error) or Freed (emits use after free error) */
                checkPairElemSafety(p)

                /* Check for unsound pair coercion: if we know the actual types
                   stored in the pair slots, verify that the expected type from
                   the type checker's annotation matches the actual stored type */
                val (_, ptrVal) = evaluateLValue(lval)

                ptrVal match {
                    case PairInfo(fstType, sndType) =>
                        val actualType = if (isFst) fstType else sndType
                        val expectedType = info match {
                            case SemanticData.Typed(_, t, _) => t
                            case _  => ?
                        }
                        if (!((actualType ~ expectedType).isDefined)) {
                            val accessKind = if (isFst) "fst" else "snd"
                            analyserState.emit(
                                AnalysisDiagnostic.unsoundPairCoercion(
                                    info.pos, accessKind, actualType, expectedType
                                )
                            )
                        }
                    case _ => () 
                }

                Unknown /* we don't track heap pair contents */
        }

    /** Extract a descriptive name and abstract value from an lvalue.
      *
      * Used by pointer safety checks to produce good diagnostic
      * messages like "variable 'p' is known to be null".
      *
      * @param lval The lvalue to extract information from
      *
      *  @return A tuple of (display name, abstract value)
      */
    private def evaluateLValue(
        lval: LValue[SemanticData]
    )(using state: AnalysisState): (String, AbstractValue) =
        lval match {
            case ident: Ident[SemanticData] =>
                val name = ident.name
                val value = VariableId.fromIdent(ident)
                    .map(state.lookup)
                    .getOrElse(Unknown)
                (name, value)
            case ArrayElem(ident, _, _) =>
                (ident.name, Unknown)
            case PairElem(_, innerLval, _) =>
                evaluateLValue(innerLval)
        }

    /** Check whether an lvalue points to a null or freed pointer and emit
      * diagnostics accordingly.
      *
      * @param lval  The lvalue being dereferenced (the pair being accessed)
      * @param info The semantic data of the access expression (for source position)
      */
    private def checkPointerSafety(
        lval: LValue[SemanticData],
        info: SemanticData
    )(using state: AnalysisState, analyserState: AnalyserState): Unit = {
        val (ptrName, ptrVal) = evaluateLValue(lval)
        emitPointerErrorIfStale(ptrName, ptrVal, info)
    }

    /** Check whether a pair element is safe to use, and if is nonnnull mark it as nonnull
      * in the state for future use in the Backend.
      *
      * @param pairElem The `PairElem` AST node being accessed
      */
    private def checkPairElemSafety(
        pairElem: PairElem[SemanticData]
    )(using state: AnalysisState, analyserState: AnalyserState): Unit = {
        val pairPointer = pairElem.lval
        val (_, pairPointerAbstract) = evaluateLValue(pairPointer)
        checkPointerSafety(pairPointer, pairElem.info)

        if (AbstractValue.isDefinitelyNonNull(pairPointerAbstract)) {
            /* Pointer is provably nonnull, record the PairElem AST node
               so the flattener can skip the runtime null check */
            analyserState.markSafePairAccess(pairElem)
        }
    }

    /** Emits an Error Diagnostic if the pointer is stale, i.e., if it is null or free'd before
     * 
     * @param ptrName The pointer name used to emit a nice error message
     * @param ptrVal The `AbstractValue` of the pointer
     * @param info `SemanticData` used to get the source pos of the error
     */
    private def emitPointerErrorIfStale(
        ptrName: String, ptrVal: AbstractValue, info: SemanticData
    )(using analyserState: AnalyserState): Unit = {
        if (AbstractValue.isDefinitelyNull(ptrVal)) {
            analyserState.emit(AnalysisDiagnostic.nullDereference(info.pos, ptrName))
        } else if (ptrVal == Freed) {
            analyserState.emit(AnalysisDiagnostic.useAfterFree(info.pos, ptrName))
        }
    }

    /** Collect all variables that could be modified inside a statement.
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

extension [A](stmt: Stmt[A]) {
    def info: A = stmt match
        case Stmt.Skip(infoParam) => infoParam
        case Stmt.Decl(_, _, _, infoParam) => infoParam
        case Stmt.Asgn(_, _, infoParam) => infoParam
        case Stmt.Read(_, infoParam) => infoParam
        case Stmt.Free(_, infoParam) => infoParam
        case Stmt.Return(_, infoParam) => infoParam
        case Stmt.Exit(_, infoParam) => infoParam
        case Stmt.Print(_, infoParam) => infoParam
        case Stmt.Println(_, infoParam) => infoParam
        case Stmt.If(_, _, _, infoParam) => infoParam
        case Stmt.While(_, _, infoParam) => infoParam
        case Stmt.Block(_, infoParam) => infoParam

}