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

    override def toString(): String = this match
        case Is(t) => t.toString()
        case OneOf(ts) => ts.mkString("one of { ", ", ", " }")
    
}

object Constraint {
    /* Means that the expression may have any type. This is typically used
       when the context does not impose a specific expectation and the type 
       should be inferred bottom up (type inference). */
    val Infer = Is(?) 

    /* Constraint for types that may be compared using operators <, <=, >, >= */
    val IsComparable = OneOf(Set(KnownType.Int, KnownType.Char))

    /* Constraint for pairs. This is essentialy useful for accessing pair elements
       with `fst` or `snd`. If we have `fst p` for any identifier `p`, `p` must be
       of type pair holding any type. The type checker then can impose this constraint
       on the identifier `p` to check if it is a pair holding any type. */
    val IsPair = Is(KnownType.Pair(?, ?))

    /* Constraint for types that may be compared using == or != */
    val IsEquatable = Is(?)

    /* Read can only take int or char types */
    val IsReadable = Constraint.OneOf(Set(KnownType.Int, KnownType.Char))
}

/**
 * State that remains fixed throughout type checking.
 * Contains the error logger and function signature table.
 */
class TCState(
  val errorLogger: SemanticErrorLogger,
  val funcSignatureTable: Map[String, SemanticData.Func]
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
        if (errorLogger.hasErrors()) {
            print(errorLogger.retrieveAllErrors())
            None
        }
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
    private def checkFunc(f: Func[SemanticData])(using TCState): Func[SemanticData] = ???

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
      case _ => ???
    }

    private def checkAsgn(lval: LValue[SemanticData], 
                          rval: RValue[SemanticData], 
                          semInfo: SemanticData)
                         (using TCState): Stmt.Asgn[SemanticData] = {
        // if lval is an identifier, we can easily get its type from its info
        // if lval is an array elem or pair elem, idk we'll see in a few days

        /* get lval's semantic type */
        val semTypeOfLVal: SemanticType = extractLValueType(lval)
        
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
                         (using tcState: TCState): Stmt.Read[SemanticData] = 
        extractLValueType(lval) match {
            case ? => 
                tcState.emit(semInfo.pos, "Ambiguous assignment between unknown types. Both the left-hand side and right-hand side of this statement have unresolved types.")
                Stmt.Read(lval, SemanticData.Error(semInfo.pos))
            case _ =>
                val typedLValue = checkLvalue(lval, Constraint.IsReadable)
                Stmt.Read(typedLValue, semInfo)
        }
    

    /**
     * Type-check an declaration statement
     */
    private def checkDecl(
        typ: SyntaxType,
        ident: Ident[SemanticData],
        rvalue: RValue[SemanticData],
        semInfo: SemanticData
    )(using st: TCState): Stmt.Decl[SemanticData] = {

        val semType = syntaxToSemantics(typ)
        
        /* verify the renamer typed it correctly */
        val typedIdent = checkIdent(ident, Is(semType))

        /* the right side of the assignment must be compatible with
           the left side declared type */
        val typedRValue = checkRvalue(rvalue, Is(semType))
        Stmt.Decl(typ, typedIdent, typedRValue, semInfo)
    }

    // for read statements, constraint is oneof(int, char)
    private def checkLvalue(lvalue: LValue[SemanticData], c: Constraint)
                           (using state: TCState): LValue[SemanticData] = lvalue match {
        case i @ Ident(name, info)           => checkIdent(i, c)
        case p @ PairElem(isFst, lval, info)     => 
            val typedLValue = checkLvalue(lval, Constraint.IsPair) 
            val pairElementType = extractLValueType(p) // this returns ?
            // CONSIDERATION:
            //      * read    <lval>: if lval type is ? -> error
            //      * asgn    <lval>
            //      * fst/snd <lval> if lval type is ? -> error
            pairElementType.satisfies(c, info.pos, state.errorLogger)
            PairElem(isFst, typedLValue, info)
        case _                               => ???
    }

    /**
      * Extract the type of an lvalue for assignment statements. 
      * 
      * This is used to infer the type of the lvalue used in assignments, and
      * then to impose a constraint with that type to the rvalue later.
      * 
      * @param lval the lvalue in assignment
      * @return `SemanticType` the type of lvalue
      */ 
    private def extractLValueType(lval: LValue[SemanticData]): SemanticType = lval match {
        case Ident(_, SemanticData.Var(_, _, semType)) => semType
        case PairElem(isFst, pairLValue , _) => {
            val pairType: SemanticType = extractLValueType(pairLValue)
            pairType match {
                case KnownType.Pair(fst, snd) => if isFst then fst else snd
                case _                        => ?
            }
        }
        case _ => ?
    }   

    /**
     * Type-check an RValue
     */
    private def checkRvalue(rvalue: RValue[SemanticData], c: Constraint)
                           (using tcState: TCState): RValue[SemanticData] = rvalue match {

        // other cases added in future...
        case expr: Expr[SemanticData] => checkExpr(expr, c)
        case NewPair(e1, e2, info)    => c match {
            case Constraint.Is(KnownType.Pair(fstType, sndType)) => {
                val checkedExpr1 = checkExpr(e1, Constraint.Is(fstType))
                val checkedExpr2 = checkExpr(e2, Constraint.Is(sndType))
                NewPair(checkedExpr1, checkedExpr2, info)
            }
            case other => { 
                tcState.emit(info.pos, s"Type mismatch: Expected Pair but got ${other.toString()}") 
                NewPair(e1, e2, SemanticData.Error(info.pos)) 
            }
        }
        case PairElem(isFst, pairLValue, info) => {
            val typedLVal = checkLvalue(pairLValue, Constraint.IsPair)
            extractLValueType(pairLValue) match {
                case KnownType.Pair(fst, snd) => 
                    if isFst then fst.satisfies(c, info.pos, tcState.errorLogger)
                        .fold
                            {PairElem(isFst, typedLVal, SemanticData.Error(info.pos))} 
                            { _ => PairElem(isFst, typedLVal, info) }
                    else snd.satisfies(c, info.pos, tcState.errorLogger)
                        .fold 
                            { PairElem(isFst, typedLVal, SemanticData.Error(info.pos))}
                            {_ => PairElem(isFst, typedLVal, info) }
                case PairAny => 
                    c match {
                        case Is(?) => 
                            tcState.emit(info.pos, "Ambiguous assignment between unknown types. Both the left-hand side and right-hand side of this statement have unresolved types.")
                            PairElem(isFst, typedLVal, SemanticData.Error(info.pos))
                        case _     => PairElem(isFst, typedLVal, info)
                    }
                case _ => PairElem(isFst, typedLVal, SemanticData.Error(info.pos))
            }
        }
        case _ => ???
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
            case (?, ty) => Some(ty)
            case (ty, ?) => Some(ty)
            case (KnownType.Array(ety1), KnownType.Array(ety2)) =>
                (ety1 ~ ety2).map(KnownType.Array(_))

            case (PairAny | Null, KnownType.Pair(fst, snd)) => Some(KnownType.Pair(fst, snd))

            case (KnownType.Pair(fst, snd), PairAny | Null) => Some(KnownType.Pair(fst, snd))

            case (PairAny, Null) | (Null, PairAny) => Some(Null)

            case (KnownType.Pair(ty11, ty12), KnownType.Pair(ty21, ty22)) =>
                for {
                    t1 <- ty11 ~ ty21
                    t2 <- ty12 ~ ty22
                } yield KnownType.Pair(t1, t2)

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
