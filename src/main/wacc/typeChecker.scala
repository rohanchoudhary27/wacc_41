package wacc

import wacc.Constraint.Is
import wacc.Constraint.IsComparable

/**
 * Constraints represent expectations imposed by the context in which an 
 * expression is type checked.
 *
 * They are used in a top-down traversal of the AST, where the parent node 
 * imposes a Constraint and the child expression is checked against it.
 */
enum Constraint {
    /* The expression must have a type that matches `t`. Matching is defined 
       by the `~` relation on SemanticType (not strict equality).  */
    case Is(t: SemanticType)      

    /* The expression must have a type that is one of the given set */
    case OneOf(ts: Set[SemanticType])
}

object Constraint {
    /* Means that the expression may have any type. This is typically used
       when the context does not impose a specific expectation and the type 
       should be inferred bottom up (type inference). */
    val Infer = Is(?) 

    /* Constraint for types that may be compared using operators <, <=, >, >= */
    val IsComparable = OneOf(Set(KnownType.Int, KnownType.Char))

    /* Constraint for types that may be compared using == or != */
    val IsEquatable = Is(?)
}

/**
 * State that remains fixed throughout type checking.
 * Contains the error logger and function signature table.
 */
class TCState(
  val errorLogger: SemanticErrorLogger,
  val funcSignatureTable: Map[String, SemanticData.Func],
  var currFuncName: String = "",
) {
  /** Emit an error message */
  def emit(pos: SourcePos, msg: String): Unit = {
    errorLogger.logError(pos, msg)
  }
}

/**
 * Type checking stage of the frontend compiler pipeline.
 *
 * This stage runs after the renamer and assumes that all identifiers 
 * are uniquely resolved and function signatures are available.
 *
 * The type checker annotates expressions with SemanticType, enforces
 * WACC typing rules, reports semantic errors but continues checking where possible
 */
object TypeChecker {
    /**
      * Type checks the renamed AST and reports any errors with errorLogger.
      *
      * @param prog the renamed program
      * @param errorLogger logger used to accumulate semantic errors
      * @param funcSignatureTable function signature table populated from the renamer
      * @return Some(typedProgram) if no errors were found, otherwise None
      */
    def check(
        prog: Program[SemanticData], errorLogger: SemanticErrorLogger,
        funcSignatureTable: Map[String, SemanticData.Func]
    ): Option[Program[SemanticData]] = {
        given state: TCState = TCState(errorLogger, funcSignatureTable)
        
        // Type-check each function
        val typedFuncs = prog.funcs.map(f => checkFunc(f))
        
        // Type-check the main statement (which will be Stmt.Block)
        val typedStmt = checkStmt(prog.stmt)

        if (errorLogger.hasErrors()) None
        else
            Some(
                Program(
                    typedFuncs,
                    typedStmt,
                    info = SemanticData.TopLevel(funcTable = funcSignatureTable)
                )
            )
    }
  
    /**
     * Type-check a function definition
     */
    private def checkFunc(func: Func[SemanticData])(using tcState: TCState): Func[SemanticData] = {
        tcState.currFuncName = func.ident.name
        val Func(typ, ident, params, stmt, info) = func
        Func(typ, ident, func.params, checkStmt(stmt), info)
    }

    /**
     * Type-check a function parameter
     */
    private def checkParam(func: Param[SemanticData])(using TCState): Param[SemanticData] = ???

    /**
     * Type-check a statement
     */
    private def checkStmt(stmt: Stmt[SemanticData])(using TCState): Stmt[SemanticData] = stmt match {

      case s @ Stmt.Skip(_) => s
      case Stmt.Exit(expr, semInfo) => Stmt.Exit(checkExpr(expr, Is(KnownType.Int)), semInfo)
      case Stmt.Print(expr, semInfo) => Stmt.Print(checkExpr(expr, Constraint.Infer), semInfo)
      case Stmt.Println(expr, semInfo) => Stmt.Println(checkExpr(expr, Constraint.Infer), semInfo)
      case Stmt.If(expr, stmtIf, stmtElse, semInfo) => Stmt.If(checkExpr(expr, Is(KnownType.Bool)),
                                                               checkStmt(stmtIf),
                                                               checkStmt(stmtElse),
                                                               semInfo)
      case Stmt.While(condExpr, stmt, semInfo) => Stmt.While(checkExpr(condExpr, Is(KnownType.Bool)),
                                                             checkStmt(stmt),
                                                             semInfo)
      case Stmt.Block(stmtList, semInfo) => Stmt.Block(checkStmtList(stmtList), semInfo)
      case Stmt.Decl(typ, ident, rvalue, semInfo) => checkDecl(typ, ident, rvalue, semInfo)
      case Stmt.Asgn(lval, rval, semInfo) => checkAsgn(lval, rval, semInfo)
      case Stmt.Read(lval, semInfo) => checkRead(lval, semInfo)
      case Stmt.Return(expr, semInfo) => checkReturn(expr, semInfo)
      case _ => ???
    }

    private def checkAsgn(lval: LValue[SemanticData], 
                          rval: RValue[SemanticData], 
                          semInfo: SemanticData)
                         (using TCState): Stmt.Asgn[SemanticData] = {
        // if lval is an identifier, we can easily get its type from its info
        // if lval is an array elem or pair elem, idk we'll see in a few days

        /* get lval's semantic type */
        val semTypeOfLVal = lval match {
            case Ident(_, SemanticData.Var(_, _, semType)) => semType
            case _                                         => ?
        }

        /* In any error case (e.g. out of scope identifiers), the renamer will put
           SemanticData.Error as its info. calling checkLvalue on such an identifier
           will do nothing, return the identifier back. So any out of scope identifiers
           in assignment statements will have Ident(_, SemanticData.Error(...)) */
        val typedLValue = checkLvalue(lval, Is(semTypeOfLVal))

        /* check rvalue with lvalue's semantic type */
        val typedRValue = checkRvalue(rval, Is(semTypeOfLVal))

        Stmt.Asgn(typedLValue, typedRValue, semInfo)
    }

    /**
     * Type-check a read statement
     */
    private def checkRead(lval: LValue[SemanticData], semInfo: SemanticData)
                         (using TCState): Stmt.Read[SemanticData] = {
        /* Read can only take int or char types */
        val readableTypes: Set[SemanticType] = Set(KnownType.Int, KnownType.Char)
        val typedLValue = checkLvalue(lval, Constraint.OneOf(readableTypes))
        Stmt.Read(typedLValue, semInfo)
    }

    /**
     * Type-check a read statement
     */
    private def checkReturn(expr: Expr[SemanticData], semInfo: SemanticData)
                         (using tcState: TCState): Stmt.Return[SemanticData] = {

        /* Fetch the current function's return type */
        val SemanticData.Func(_, _, funcRetType, _) = 
            tcState.funcSignatureTable(tcState.currFuncName)
        
        val constraint = (semInfo: @unchecked) match {
            case SemanticData.Pos(_) => Is(funcRetType)
            case SemanticData.Error(pos) => Is(?)
        }

        Stmt.Return(checkExpr(expr, constraint), semInfo)
    }

    /**
     * Type-check an declaration statement
     */
    private def checkDecl(
        typ: SyntaxType,
        ident: Ident[SemanticData],
        rvalue: RValue[SemanticData],
        semInfo: SemanticData
    )(using TCState): Stmt.Decl[SemanticData] = {

        val semType = syntaxToSemantics(typ)

        /* the right side of the assignment must be compatible with
           the left side declared type */
        val typedRvalue = checkRvalue(rvalue, Is(semType))

        /* verify the renamer typed it correctly */
        val typedIdent = checkIdent(ident, Is(semType))

        Stmt.Decl(typ, typedIdent, typedRvalue, semInfo)
    }

    private def checkLvalue(lvalue: LValue[SemanticData], c: Constraint)
                           (using TCState): LValue[SemanticData] = lvalue match {
        case i @ Ident(name, info) => checkIdent(i, c)
        case _                     => ???
    }

    /**
     * Type-check a Call
     */
    private def checkCall(ident: Ident[SemanticData],
                          exprList: List[Expr[SemanticData]],
                          semInfo: SemanticData)
                           (using tcState: TCState): Call[SemanticData] = semInfo match {

        case SemanticData.Pos(_) => {
            val signature = tcState.funcSignatureTable(ident.name)
            val typedArgs = exprList
                .zip(signature.params)
                .map((expr, expectedType) => checkExpr(expr, Is(expectedType)))

            Call(ident, typedArgs, semInfo)
        }

        case SemanticData.Error(_) => Call(ident, exprList, semInfo)
    }


    /**
     * Type-check an RValue
     */
    private def checkRvalue(rvalue: RValue[SemanticData], c: Constraint)
                           (using tcState: TCState): RValue[SemanticData] = rvalue match {

        case Call(ident, exprList, semInfo) => checkCall(ident, exprList, semInfo)
        case expr: Expr[SemanticData] => checkExpr(expr, c)
    }

    /**
     * Type-check an Ident
     */
    private def checkIdent(ident: Ident[SemanticData], c: Constraint)
                          (using state: TCState): Ident[SemanticData] = ident match {

        /* Check for type mismatch error */
        case Ident(_, SemanticData.Var(pos, _, actualType)) => 
             actualType.satisfies(c, pos, state.errorLogger)
             ident

        /* Variable not found by renamer (error already logged) */
        case Ident(_, SemanticData.Error(_)) => 
             ident

        case _ => ident
    }

    /**
     * Type-check a list of expressions
     */
    private def checkStmtList(stmtList: List[Stmt[SemanticData]])
                             (using TCState): List[Stmt[SemanticData]] = stmtList.map(s => checkStmt(s))

    /**
     * Helper to type-check literal expressions
     */
    private def checkLiteral[A](
        value: A,
        info: SemanticData.Expr,
        constructor: (A, SemanticData) => Expr[SemanticData],
        c: Constraint
    )(using state: TCState): Expr[SemanticData] = {
        val SemanticData.Expr(pos, actualType) = info
        val ty = actualType.satisfies(c, pos, state.errorLogger).getOrElse(?)
        constructor(value, info.withType(ty))
    }

    /**
     * Type-check an expression
     * 
     * int x = 5
     * 
     */
    private def checkExpr(expr: Expr[SemanticData], c: Constraint)
                         (using state: TCState): Expr[SemanticData] = expr match {

        case IntLiter(value, info: SemanticData.Expr) => 
            checkLiteral(value, info, IntLiter.apply, c)

        case BoolLiter(value, info: SemanticData.Expr) => 
            checkLiteral(value, info, BoolLiter.apply, c)

        case CharLiter(value, info: SemanticData.Expr) => 
            checkLiteral(value, info, CharLiter.apply, c)

        case StrLiter(value, info: SemanticData.Expr) => 
            checkLiteral(value, info, StrLiter.apply, c)

        case PairLiter(info: SemanticData.Expr) => 
            checkLiteral((), info, (_, data) => PairLiter(data), c)

        case i: Ident[SemanticData] => checkIdent(i, c)
        case UnaryExpr(op, expr, SemanticData.Pos(pos)) => {
            val (inputConstraint, resultType) = op match {
                case UnaryOperator.Not => (Constraint.Is(KnownType.Bool), KnownType.Bool)
                case UnaryOperator.Neg => (Constraint.Is(KnownType.Int), KnownType.Int)
                case UnaryOperator.Len => (Constraint.OneOf(Set(KnownType.Array(?),KnownType.String)), KnownType.Int)
                case UnaryOperator.Ord => (Constraint.Is(KnownType.Char), KnownType.Int)
                case UnaryOperator.Chr => (Constraint.Is(KnownType.Int), KnownType.Char)
            }
            val typedExpr = checkExpr(expr, inputConstraint)
            val finalType = resultType.satisfies(c, pos, state.errorLogger).getOrElse(?)
            UnaryExpr(op, typedExpr, SemanticData.Expr(pos, finalType))
        }
        
        case BinaryExpr(op, left, right, SemanticData.Pos(pos)) => {
            val (inputLConstraint, inputRConstraint, resultType) = op match {
                case BinaryOperator.Add | BinaryOperator.Sub | BinaryOperator.Mul | BinaryOperator.Div | BinaryOperator.Mod => 
                    (Constraint.Is(KnownType.Int), Constraint.Is(KnownType.Int), KnownType.Int)
                case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT | BinaryOperator.LTE => 
                    (IsComparable, IsComparable, KnownType.Bool)
                case BinaryOperator.EQ | BinaryOperator.NEQ => 
                    (Constraint.IsEquatable, Constraint.IsEquatable, KnownType.Bool)
                case BinaryOperator.And | BinaryOperator.Or => 
                    (Constraint.Is(KnownType.Bool), Constraint.Is(KnownType.Bool), KnownType.Bool)
            }
            val typedLExpr = checkExpr(left, inputLConstraint)

            def getType(e: Expr[SemanticData]): SemanticType = e.info match {
                case SemanticData.Expr(_, t) => t
                case _ => ?
            }

            val matchR = op match {
                case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT | BinaryOperator.LTE | BinaryOperator.EQ | BinaryOperator.NEQ => 
                    Constraint.Is(getType(typedLExpr))
                case _ => inputRConstraint
            }
            val typedRExpr = checkExpr(right, matchR)
            val finalType = resultType.satisfies(c, pos, state.errorLogger).getOrElse(?)
            BinaryExpr(op, typedLExpr, typedRExpr, SemanticData.Expr(pos, finalType))
        }

        case _ => ???
    }
    
    /**
     * Type compatibility: ty1 ~ ty2 succeeds if the two types are compatible and returns
     * the "narrow" resulting type.
     */
    extension (ty1: SemanticType) {
        def ~(ty2: SemanticType): Option[SemanticType] = (ty1, ty2) match {
            
            /* `?` matches anything, this will come in handy when checking constraints:
                if we represent Null (pair literal) as `Pair(?)``, then for the statement:
                    ```
                    pair(int, bool) p = null
                    ```
                we will impose a constraint of `Is(Pair(Int, Bool))` from the LHS, then
                will check this against the RHS which will be `Pair(?)`. We can define the
                constraint satisfaction such that `Pair(?).satisfies(Pair(Int, Bool))` will
                check `? ~ Pair(Int, Bool)`, which will match and return `Pair(Int, Bool)`
            */
            case (?, ty) => Some(ty)
            case (ty, ?) => Some(ty)
            case (KnownType.Array(ety1), KnownType.Array(ety2)) =>
                (ety1 ~ ety2).map(KnownType.Array(_))

            /* Exact type match */
            case (ty1, ty2) if ty1 == ty2 => Some(ty1)
            case _  => None
        }
    }

    extension (ty: SemanticType) {
        /** 
         * Checks whether type `ty` matches constraint `c`.
         * 
         * This method performs no error logging.
         * 
         * @return `Some(SemanticType)` on success, or `None` on failure.
         */
        private def matches(c: Constraint): Option[SemanticType] = (ty, c) match {
            case (kty, Constraint.Is(refTy)) => kty ~ refTy
            
            case (kty, Constraint.OneOf(ts)) => 
                ts.iterator
                  .map(t => kty.matches(Constraint.Is(t)))
                  .collectFirst {case Some(res) => res }
            
            case (?, _) => Some(?)

            // ...
        }
        
        /**
         * Checks whether a type satisfies a given constraint, reports an error on failure.
         *
         * @return `Some(SemanticType)` if the constraint is satisfied, otherwise logs a type mismatch error and `None`
         */
        def satisfies(c: Constraint, pos: SourcePos, logger: SemanticErrorLogger): Option[SemanticType] = {
            ty.matches(c).orElse {
                val expectedTypeStr = c match {
                    case Constraint.Is(t)     => t.toString
                    case Constraint.OneOf(ts) => ts.mkString(" | ")
                    case other                => other.toString
                }
                logger.logError(pos, s"Type Mismatch: Expected $expectedTypeStr, got $ty")
                None
            }

        }
    }

}
