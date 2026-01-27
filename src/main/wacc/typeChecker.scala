package wacc

import wacc.Constraint.Is

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
 * Type checking stage of the frontend compiler pipeline.
 *
 * This stage runs after the renamer and assumes that all identifiers 
 * are uniquely resolved and function signatures are available.
 *
 * The type checker annotates expressions with SemanticType, enforces
 * WACC typing rules, reports semantic errors but continues checking where possible
 */
class TypeChecker {
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
        // Type-check each function
        val typedFuncs = prog.funcs.map(f => checkFunc(f, errorLogger))
        
        // Type-check the main statement (which will be Stmt.Block)
        val typedStmt = checkStmt(prog.stmt, errorLogger)

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
    private def checkFunc(
        f: Func[SemanticData], log: SemanticErrorLogger
    ): Func[SemanticData] = ???

    /**
     * Type-check a statement
     */
    private def checkStmt(
        stmt: Stmt[SemanticData], log: SemanticErrorLogger
    ): Stmt[SemanticData] = stmt match {

      case s @ Stmt.Skip(_) => s
      case Stmt.Exit(expr, semInfo) => Stmt.Exit(checkExpr(expr, Is(KnownType.Int), log), semInfo)
      case Stmt.Print(expr, semInfo) => Stmt.Print(checkExpr(expr, Constraint.Infer, log), semInfo)
      case Stmt.Println(expr, semInfo) => Stmt.Println(checkExpr(expr, Constraint.Infer, log), semInfo)
      case Stmt.Block(stmtList, semInfo) => Stmt.Block(checkStmtList(stmtList, log), semInfo)
      case Stmt.Decl(typ, ident, rvalue, semInfo) => checkDecl(typ, ident, rvalue, semInfo, log)
      case Stmt.Asgn(lval, rval, semInfo) => checkAsgn(lval, rval, semInfo, log)
      case _ => ???
    }

    private def checkAsgn(lval: LValue[SemanticData], 
                          rval: RValue[SemanticData], 
                          semInfo: SemanticData, 
                          log: SemanticErrorLogger
    ): Stmt.Asgn[SemanticData] = {
        // if lval is an identifier, we can easily get its type from its info
        // if lval is an array elem or pair elem, idk we'll see in a few days

        /* get lval's semantic type */
        val semTypeOfLVal = lval match {
            case Ident(_, SemanticData.Var(pos, renameId, semType)) => semType
            case _                                                  => ?
        }

        /* In any error case (e.g. out of scope identifiers), the renamer will put
           SemanticData.Error as its info. calling checkLvalue on such an identifier
           will do nothing, return the identifier back. So any out of scope identifiers
           in assignment statements will have Ident(_, SemanticData.Error(...)) */
        val typedLValue = checkLvalue(lval, Is(semTypeOfLVal), log)

        /* check rvalue with lvalue's semantic type */
        val typedRValue = checkRvalue(rval, Is(semTypeOfLVal), log)

        Stmt.Asgn(typedLValue, typedRValue, semInfo)
    }

    /**
     * Type-check an declaration statement
     */
    private def checkDecl(
        typ: SyntaxType,
        ident: Ident[SemanticData],
        rvalue: RValue[SemanticData],
        semInfo: SemanticData,
        log: SemanticErrorLogger,
    ): Stmt.Decl[SemanticData] = {

        val semType = syntaxToSemantics(typ)

        /* the right side of the assignment must be compatible with
           the left side declared type */
        val typedRvalue = checkRvalue(rvalue, Is(semType), log)

        /* verify the renamer typed it correctly */
        val typedIdent = checkIdent(ident, Is(semType), log)

        Stmt.Decl(typ, typedIdent, typedRvalue, semInfo)
    }

    private def checkLvalue(
        lvalue: LValue[SemanticData], c: Constraint, log: SemanticErrorLogger
    ): LValue[SemanticData] = lvalue match {
        case i @ Ident(name, info) => checkIdent(i, c, log)
        case _                     => ???
    }

    /**
     * Type-check an RValue
     */
    private def checkRvalue(
        rvalue: RValue[SemanticData], c: Constraint, log: SemanticErrorLogger
    ): RValue[SemanticData] = rvalue match {

       // other cases added in future...
       case expr: Expr[SemanticData] => checkExpr(expr, c, log)
    }

    /**
     * Type-check an Ident
     */
    private def checkIdent(
        ident: Ident[SemanticData], c: Constraint, log: SemanticErrorLogger
    ): Ident[SemanticData] = ident match {

        /* Check for type mismatch error */
        case Ident(_, SemanticData.Var(pos, _, actualType)) => 
             actualType.satisfies(c, pos, log)
             ident

        /* Variable not found by renamer (error already logged) */
        case Ident(_, SemanticData.Error(_)) => 
             ident

        case _ => ident
    }

    /**
     * Type-check a list of expressions
     */
    private def checkStmtList(stmtList: List[Stmt[SemanticData]],
                              log: SemanticErrorLogger
                              ): List[Stmt[SemanticData]] = stmtList.map(s => checkStmt(s, log))

    /**
     * Type-check an expression
     */
    private def checkExpr(
        expr: Expr[SemanticData], c: Constraint, log: SemanticErrorLogger
    ): Expr[SemanticData] = expr match {

        case IntLiter(value, SemanticData.Expr(pos, actualType)) => {
            val ty = actualType.satisfies(c, pos, log).getOrElse(?)
            IntLiter(value, SemanticData.Expr(pos, ty))
        }

        case BoolLiter(value, SemanticData.Expr(pos, actualType)) => {
            val ty = actualType.satisfies(c, pos, log).getOrElse(?)
            BoolLiter(value, SemanticData.Expr(pos, ty))
        }

        case CharLiter(value, SemanticData.Expr(pos, actualType)) => {
            val ty = actualType.satisfies(c, pos, log).getOrElse(?)
            CharLiter(value, SemanticData.Expr(pos, ty))
        }

        case StrLiter(value, SemanticData.Expr(pos, actualType)) => {
            val ty = actualType.satisfies(c, pos, log).getOrElse(?)
            StrLiter(value, SemanticData.Expr(pos, ty))
        }

        case PairLiter(SemanticData.Expr(pos, actualType)) => 
            PairLiter(SemanticData.Expr(pos, actualType.satisfies(c, pos, log).getOrElse(?)))

        case i: Ident[SemanticData] => checkIdent(i, c, log)

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
