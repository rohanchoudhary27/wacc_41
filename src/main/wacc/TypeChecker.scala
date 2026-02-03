package wacc

import wacc.Constraint.Is
import wacc.Constraint.IsComparable

/** Constraints represent expectations imposed by the context in which an
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
    case Is(t)     => t.toString()
    case OneOf(ts) => ts.mkString(" | ")

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

  /* Free can only take any array or pair type */
  val IsFreeable =
    Constraint.OneOf(Set(KnownType.Array(?), KnownType.Pair(?, ?)))
}

/** State that remains fixed throughout type checking. Contains the error logger
  * and function signature table.
  */
class TCState(
    val errorLogger: ErrorLogger,
    val funcSignatureTable: Map[String, SemanticData.Func],
    var currFuncName: String = ""
) {

  /** Emit an error message */
  def emit(pos: SourcePos, msg: String, extraInfo: String = ""): Unit = {
    errorLogger.logError(pos, msg, extraInfo)
  }
}

/** Type checking stage of the frontend compiler pipeline.
  *
  * This stage runs after the renamer and assumes that all identifiers are
  * uniquely resolved and function signatures are available.
  *
  * The type checker annotates expressions with SemanticType, enforces WACC
  * typing rules, reports semantic errors but continues checking where possible
  */
object TypeChecker {

  /** Type checks the renamed AST and reports any errors with errorLogger.
    *
    * @param prog
    *   the renamed program
    * @param errorLogger
    *   logger used to accumulate semantic errors
    * @param funcSignatureTable
    *   function signature table populated from the renamer
    * @return
    *   Some(typedProgram) if no errors were found, otherwise None
    */
  def check(
      prog: Program[SemanticData],
      errorLogger: ErrorLogger,
      funcSignatureTable: Map[String, SemanticData.Func]
  ): Option[Program[SemanticData]] = {

    given state: TCState = TCState(errorLogger, funcSignatureTable)

    /* Type-check each function */
    val typedFuncs = prog.funcs.map(f => checkFunc(f))

    /* Type-check the main statement */
    val typedStmt = checkStmt(prog.stmt)

    /* Log errors and return None */
    if (errorLogger.hasErrors()) {
      None
    }

    /* return a type checked semantic AST */
    else
      Some(
        Program(
          typedFuncs,
          typedStmt,
          info = SemanticData.TopLevel(funcTable = funcSignatureTable)
        )
      )
  }

  /** Type-check a function definition
    *
    * @param func:
    *   The `Func` node to be type checked
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   The type-checked `Func` node
    */
  private def checkFunc(
      func: Func[SemanticData]
  )(using tcState: TCState): Func[SemanticData] = {

    /* Record the current function being type checked so that we know what
           function we're in when type checking Return and Call statements */
    tcState.currFuncName = func.ident.name

    val Func(typ, ident, params, stmt, info) = func
    Func(typ, ident, func.params, checkStmt(stmt), info)
  }

  /** Type-check a statement definition
    *
    * @param stmt:
    *   The `Stmt` node to be type checked
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   The type-checked `Stmt` node
    */
  private def checkStmt(
      stmt: Stmt[SemanticData]
  )(using TCState): Stmt[SemanticData] =
    (stmt: @unchecked) match {
      case s @ Stmt.Skip(_)         => s
      case Stmt.Exit(expr, semInfo) =>
        Stmt.Exit(checkExpr(expr, Is(KnownType.Int)), semInfo)
      case Stmt.Print(expr, semInfo) =>
        Stmt.Print(checkExpr(expr, Constraint.Infer), semInfo)
      case Stmt.Println(expr, semInfo) =>
        Stmt.Println(checkExpr(expr, Constraint.Infer), semInfo)
      case Stmt.If(expr, stmtIf, stmtElse, semInfo) =>
        Stmt.If(
          checkExpr(expr, Is(KnownType.Bool)),
          checkStmt(stmtIf),
          checkStmt(stmtElse),
          semInfo
        )
      case Stmt.While(condExpr, stmt, semInfo) =>
        Stmt.While(
          checkExpr(condExpr, Is(KnownType.Bool)),
          checkStmt(stmt),
          semInfo
        )
      case Stmt.Block(stmtList, semInfo) =>
        Stmt.Block(checkStmtList(stmtList), semInfo)
      case Stmt.Decl(typ, ident, rvalue, semInfo) =>
        checkDecl(typ, ident, rvalue, semInfo)
      case Stmt.Asgn(lval, rval, semInfo) => checkAsgn(lval, rval, semInfo)
      case Stmt.Read(lval, semInfo)       => checkRead(lval, semInfo)
      case Stmt.Return(expr, semInfo)     => checkReturn(expr, semInfo)
      case Stmt.Free(expr, semInfo)       => checkFree(expr, semInfo)
    }

  /** Type check an assignment statement.
    *
    * @param lval
    *   left-hand side of the assignment statement
    * @param rval
    *   right-hand side of the assignment statement
    * @param semInfo
    *   semantic data associated with the statement (`SemanticData.Pos`)
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   `Stmt.Asgn[SemanticData]` type-checked Asgn node
    */
  private def checkAsgn(
      lval: LValue[SemanticData],
      rval: RValue[SemanticData],
      semInfo: SemanticData
  )(using TCState): Stmt.Asgn[SemanticData] = {

    /* get left-hand side semantic type */
    val semTypeOfLVal: SemanticType = extractLValueType(lval)

    /* check lvalue against its own type */
    val typedLValue = checkLvalue(lval, Is(semTypeOfLVal))

    /* check rvalue with lvalue's semantic type */
    val typedRValue = checkRvalue(rval, Is(semTypeOfLVal))

    Stmt.Asgn(typedLValue, typedRValue, semInfo)
  }

  /** Type-check a read statement
    *
    * @param lval
    *   value to be read
    * @param semInfo
    *   semantic data associated with the statement (`SemanticData.Pos`)
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   `Stmt.Read[SemanticData]` type-checked Read node
    */
  private def checkRead(lval: LValue[SemanticData], semInfo: SemanticData)(using
      tcState: TCState
  ): Stmt.Read[SemanticData] = {

    extractLValueType(lval) match {

      /* the type of lval must be known when used in a read statement */
      case ? =>
        val errorMsg = "Type error: ambiguous assignment between unknown types."
        val errorDesc = "\nDestination type unresolved so cannot be read into"
        tcState.emit(lval.info.pos, errorMsg, errorDesc)
        Stmt.Read(lval, SemanticData.Error(lval.info.pos))
      case _ =>
        val typedLValue = checkLvalue(lval, Constraint.IsReadable)
        Stmt.Read(typedLValue, semInfo)
    }
  }

  /** Type-check a return statement
    *
    * @param expr:
    *   The expression being returned
    * @param semInfo:
    *   Semantic data associated with the statement (`SemanticData.Pos`)
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   `Stmt.Read[SemanticData]` type-checked Read node
    */
  private def checkReturn(expr: Expr[SemanticData], semInfo: SemanticData)(using
      tcState: TCState
  ): Stmt.Return[SemanticData] = {

    /* Fetch the current function's return type */
    val funcRetType = tcState.funcSignatureTable.get(tcState.currFuncName) match
        case Some(SemanticData.Func(_, _, retType, _)) => retType
        case None => ?

    /* Return expression must match the function return type (unless
           we're allowing a mismatch because of a Renamer found error) */
    val constraint = (semInfo: @unchecked) match {
      case SemanticData.Pos(_)     => Is(funcRetType)
      case SemanticData.Error(pos) => Is(?)
    }

    Stmt.Return(checkExpr(expr, constraint), semInfo)
  }

  /** Type check a free statement
    *
    * @param expr:
    *   The expression being returned
    * @param semInfo:
    *   Semantic data associated with statement * @param tcState: The
    *   type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return
    *   `Stmt.Free[SemanticData]` type-checked Free node
    */
  private def checkFree(expr: Expr[SemanticData], semInfo: SemanticData)(using
      tcState: TCState
  ): Stmt.Free[SemanticData] = {
    val typedExpr = checkExpr(expr, Constraint.IsFreeable)
    Stmt.Free(typedExpr, semInfo)
  }

  /** Type-check a declaration statement
    *
    * @param typ:
    *   Syntax type of the declaration
    * @param ident:
    *   Identifier to be declared
    * @param rvalue:
    *   Right-hand side of the declaration statement
    * @param semInfo:
    *   Semantic data associated with the statement (`SemanticData.Pos`)
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return:
    *   `Stmt.Decl[SemanticData]` type-checked Decl node
    */
  private def checkDecl(
      typ: SyntaxType,
      ident: Ident[SemanticData],
      rvalue: RValue[SemanticData],
      semInfo: SemanticData
  )(using tcState: TCState): Stmt.Decl[SemanticData] = {

    /* both the identifier and rvalue must have this type */
    val semType = syntaxToSemantics(typ)

    /* verify the renamer typed it correctly */
    val typedIdent = checkIdent(ident, Is(semType))

    /* the right side of the assignment must be compatible with
           the left side declared type */
    val typedRValue = checkRvalue(rvalue, Is(semType))

    Stmt.Decl(typ, typedIdent, typedRValue, semInfo)
  }

  /** Type-check an L-value
    *
    * @param lvalue:
    *   The lvalue to be type-checked
    * @param c:
    *   The constraint imposed to the lvalue
    *
    * @return
    *   `LValue[SemanticData]` type-checked `LValue` node
    */
  private def checkLvalue(lvalue: LValue[SemanticData], c: Constraint)(using
      tcState: TCState
  ): LValue[SemanticData] =

    (lvalue: @unchecked) match {

      case i @ Ident(name, info)           => checkIdent(i, c)
      case p @ PairElem(isFst, lval, info) =>

        /* make sure that the thing we are referencing has a pair type */
        val typedLValue = checkLvalue(lval, Constraint.IsPair)

        val pairElementType = extractLValueType(p)

        /* this lvalue's type must satisfy the constraint */
        val elemType =
          pairElementType
            .satisfies(c, info.pos, tcState.errorLogger)
            .getOrElse(?)

        PairElem(isFst, typedLValue, SemanticData.Typed(info.pos, elemType))

      case a @ ArrayElem(_, _, _) => checkArrayAccess(a, c)
    }

  /** Extract the type of an lvalue
    *
    * @param lval:
    *   The lvalue to get the type of
    * @return:
    *   The `SemanticType` of lvalue
    */
  private def extractLValueType(lval: LValue[SemanticData]): SemanticType =
    lval match {
      /* renamer already encodes identifier types in its info field */
      case Ident(_, SemanticData.Var(_, _, semType)) => semType

      /* for fst/snd dereferencing, we have to make sure that we're referencing a pair
           or we return `?` */
      case PairElem(isFst, pairLValue, _) => {
        /* LValue inside PairElem can be a PairElem too, and that can go on. For example,
               `fst fst fst p`.*/
        val pairType: SemanticType = extractLValueType(pairLValue)
        pairType match {
          /* If the lvalue inside is not an erased pair, return the given type */
          case KnownType.Pair(fst, snd) => if isFst then fst else snd
          /* If the inner lvalue is an erased pair type or something else, we
                   can't know its field types, so return `?`. */
          case _ => ?
        }
      }

      /* for array element access, unwrap one Array layer per index */
      case ArrayElem(ident, indices, _) =>
        var arrType: SemanticType = extractLValueType(ident)
        for (_ <- indices) {
          arrType = arrType match {
            case KnownType.Array(elemType) => elemType
            case _ => ?
          }
        }
        arrType

      case _ => ?
    }

  /** Type-check a call statement
    *
    * @param ident:
    *   Identifier to be declared
    * @param exprList:
    *   The list of arguments in the call
    * @param c:
    *   The constrain to type-check the call against (for a declaration)
    * @param semInfo:
    *   Semantic data associated with the statement (`SemanticData.Pos`)
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return:
    *   `Stmt.Call[SemanticData]` type-checked Call node
    */
  private def checkCall(
      ident: Ident[SemanticData],
      exprList: List[Expr[SemanticData]],
      c: Constraint,
      semInfo: SemanticData
  )(using tcState: TCState): Call[SemanticData] =

    (semInfo: @unchecked) match {

      /* Standard case for a Call() statement */
      case SemanticData.Pos(pos) => {

        /* Fetch the function signature */
        val signature = tcState.funcSignatureTable(ident.name)

        /* Check each provided argument matches the type of the
                function param */
        val typedArgs = exprList
          .zip(signature.params)
          .map((expr, expectedType) => checkExpr(expr, Is(expectedType)))

        /* Check the function being called satisfies the `Decl()` enforced
                type constraint */
        val finalType =
          signature.returnType
            .satisfies(c, pos, tcState.errorLogger)
            .getOrElse(?)

        Call(ident, typedArgs, SemanticData.Typed(pos, finalType))
      }

      /* Renamer found an argument count mismatch or undefined function use
            error so skip type-check */
      case SemanticData.Error(_) => Call(ident, exprList, semInfo)
    }

  /** Get the type of an expression given it is annotated with
    * SemanticData.Typed. Helper function for `checkRvalue`.
    *
    * @param expr:
    *   The expression to extract a type out of
    *
    * @return:
    *   The semantic type of the expression
    */
  private def getExpressionType(expr: Expr[SemanticData]): SemanticType =
    expr.info match {
      case SemanticData.Var(_, _, t) => t
      case SemanticData.Typed(_, t) => t
      case _                        => ?
    }

  /** Type-check an RValue
    *
    * @param rvalue:
    *   The rvalue to type check
    * @param c:
    *   The constraint imposed to this rvalue
    *
    * @return:
    *   `RValue[SemanticData]` type-checked `RValue` node
    */
  private def checkRvalue(rvalue: RValue[SemanticData], c: Constraint)(using
      tcState: TCState
  ): RValue[SemanticData] =

    (rvalue: @unchecked) match {

      case Call(ident, exprList, semInfo) =>
        checkCall(ident, exprList, c, semInfo)
      case expr: Expr[SemanticData] => checkExpr(expr, c)

      /* for newpair(e1, e2), both e1 and e2 must match the pair types imposed in the
               constraint, otherwise we get an error. */
      case NewPair(e1, e2, info) =>
        c match {

          case Constraint.Is(pairType @ KnownType.Pair(fstType, sndType)) => {
            val checkedExpr1 = checkExpr(e1, Constraint.Is(fstType))
            val checkedExpr2 = checkExpr(e2, Constraint.Is(sndType))
            NewPair(
              checkedExpr1,
              checkedExpr2,
              SemanticData.Typed(info.pos, pairType)
            )
          }

          case Constraint.Is(PairAny) | Constraint.Is(?) => {
            val checkedExpr1 = checkExpr(e1, Constraint.Infer)
            val checkedExpr2 = checkExpr(e2, Constraint.Infer)

            NewPair(
              checkedExpr1,
              checkedExpr2,
              SemanticData.Typed(
                info.pos,
                KnownType.Pair(
                  getExpressionType(checkedExpr1),
                  getExpressionType(checkedExpr2)
                )
              )
            )
          }

          case other => {
            tcState.emit(
              info.pos,
              s"Type error: Expected '${other.toString()}' but got '${PairAny.toString()}'"
            )

            /* error indicated by SemanticData.Error */
            NewPair(e1, e2, SemanticData.Error(info.pos))
          }
        }

      /* for [e1, e2, ...], check all elements have compatible types */
      case ArrayLiter(elems, info) =>
        c match {
          case Constraint.Is(arrType @ KnownType.Array(elemType)) =>
            /* first pass: infer types of all elements to check compatibility */
            val typedElems = elems.map(e => checkExpr(e, Constraint.Infer))
            val elemTypes = typedElems.map(getExpressionType)
            
            /* check if any elements are incompatible with expected type */
            val incompatibleTypes = elemTypes.filterNot(t => (t ~ elemType).isDefined)
            
            if (incompatibleTypes.nonEmpty) {
              /* report single comprehensive error showing all types found */
              val typesList = elemTypes.distinct.mkString(", ")
              tcState.emit(
                info.pos,
                "Type error: array literal mismatch",
                s"\nliteral contains mix of $typesList\nexpected $arrType"
              )
              ArrayLiter(typedElems, SemanticData.Error(info.pos))
            } else {
              /* all elements compatible - recheck with constraint to apply weakening */
              val retypedElems = elems.map(e => checkExpr(e, Is(elemType)))
              ArrayLiter(retypedElems, SemanticData.Typed(info.pos, arrType))
            }

          /* type weakening: char[] can be used where string is expected */
          case Constraint.Is(KnownType.String) =>
            val typedElems = elems.map(e => checkExpr(e, Is(KnownType.Char)))
            ArrayLiter(typedElems, SemanticData.Typed(info.pos, KnownType.String))

          case Constraint.Is(?) =>
            /* find the Lowest Common Ancestor of all element types */
            if (elems.isEmpty) {
              ArrayLiter(Nil, SemanticData.Typed(info.pos, KnownType.Array(?)))
            } else {
              /* first pass: infer types of all elements */
              val typedElems = elems.map(e => checkExpr(e, Constraint.Infer))
              val elemTypes = typedElems.map(getExpressionType)
              
              /* find LCA by folding types with ~ operator */
              val lcaOpt: Option[SemanticType] = elemTypes.tail.foldLeft(
                Option(elemTypes.head)
              ) { (acc, elemType) =>
                acc.flatMap(_ ~ elemType)
              }
              
              (lcaOpt: @unchecked) match {
                case Some(lcaType) =>
                  /* second pass: re-check all elements against LCA to apply weakening */
                  val retypedElems = elems.map(e => checkExpr(e, Is(lcaType)))
                  ArrayLiter(retypedElems, SemanticData.Typed(info.pos, KnownType.Array(lcaType)))
                  
                case None =>
                  val typesList = elemTypes.mkString(", ")
                  tcState.emit(
                    info.pos,
                    "Type error: array literal has incompatible element types",
                    s"\nElement types: [$typesList]\nNo common ancestor type exists"
                  )
                  ArrayLiter(typedElems, SemanticData.Error(info.pos))
              }
            }

          case other =>
            tcState.emit(info.pos, s"Type error: Expected Array but got ${other.toString()}")
            ArrayLiter(elems, SemanticData.Error(info.pos))
        }

      /* for fst/snd dereferencing, we have to make sure that we dereference a
               pair (erased or non-erased).

               For erased case, check that we at least know something about the type from the
               constraint, if the constraint also doesn't have a known type, there's a type
               ambiguity error. Otherwise, return the PairElem node.
            
               For non-erased case, check that either the type first or second element of pair,
               depending on `fst/snd`, matches the constraint. Any errors will get reported to
               the logger and the resultant node will have `SemanticData.Error` annotation. */
      case p: PairElem[SemanticData] => checkPairAccess(p, c)
    }

  /** Type-check pair access.
    *
    * Checks that the lvalue we are accessing has the type pair.
    *
    * If the type of lvalue is a known (non-erased) pair, type-checks the first
    * or the second element of the lvalue, depending on `isFst` field of the
    * given `pairElem`, against the given constraint `c`.
    *
    * If the type of lvalue is not known (erased), it either returns the given
    * `pairElem` but with a typed lvalue, or if the constraint is also unknown,
    * emits an error to avoid ambiguous assignment between unknown types.
    *
    * In any case of error, it annotates the return value with
    * `SemanticData.Error`.
    *
    * @param pairElem
    *   Access statement (e.g `fst p`)
    * @param c
    *   The constraint imposed to the accessed element of the pair
    * @param tcState
    *   Implicit typechecker state
    * @return
    *   Type-checked pair elem
    */
  private def checkPairAccess(
      pairElem: PairElem[SemanticData],
      c: Constraint
  )(using tcState: TCState): RValue[SemanticData] =

    val PairElem(isFst, lvalue, info) = pairElem
    val typedLVal = checkLvalue(lvalue, Constraint.IsPair)

    extractLValueType(lvalue) match {

      /* non-erased pair case */
      case KnownType.Pair(fst, snd) =>
        if isFst then
          fst
            .satisfies(c, info.pos, tcState.errorLogger)
            .fold { PairElem(isFst, typedLVal, SemanticData.Error(info.pos)) } {
              t => PairElem(isFst, typedLVal, SemanticData.Typed(info.pos, t))
            }
        else
          snd
            .satisfies(c, info.pos, tcState.errorLogger)
            .fold { PairElem(isFst, typedLVal, SemanticData.Error(info.pos)) } {
              t => PairElem(isFst, typedLVal, SemanticData.Typed(info.pos, t))
            }

      /* erased pair case */
      case PairAny =>
        c match {
          case Is(?) =>
            val errorMsg =
              "Type error: ambiguous assignment between unknown types."
            val errorDesc =
              "\nBoth the left-hand side and right-hand side of this statement have" +
                " unresolved types."
            tcState.emit(info.pos, errorMsg, errorDesc)
            PairElem(isFst, typedLVal, SemanticData.Error(info.pos))
          case _ => PairElem(isFst, typedLVal, info)
        }

      case _ => PairElem(isFst, typedLVal, SemanticData.Error(info.pos))
    }

  /** Type-check array element access.
    *
    * Checks that all indices are Int and that the element type satisfies
    * the constraint.
    *
    * @param arrayElem
    *   Array element access (e.g `arr[0]`)
    * @param c
    *   The constraint imposed on the element type
    * @param tcState
    *   Implicit typechecker state
    * @return
    *   Type-checked array elem
    */
  private def checkArrayAccess(
      arrayElem: ArrayElem[SemanticData],
      c: Constraint
  )(using tcState: TCState): ArrayElem[SemanticData] =

    val ArrayElem(ident, indices, info) = arrayElem

    /* check array identifier has array type */
    val typedIdent = checkIdent(ident, Constraint.Is(KnownType.Array(?)))

    /* check all indices are Int */
    val typedIndices = indices.map(i => checkExpr(i, Is(KnownType.Int)))

    /* extract element type and check constraint */
    val elemType = extractLValueType(arrayElem)
    
    /* if elemType is ?, we tried to index too deeply */
    if (elemType == ?) then
      val identType = extractLValueType(ident)
      tcState.emit(
        info.pos,
        "Type error: array accessed with too many indices",
        s"\nExpression '${ident.name}' has type $identType but accessed with ${indices.length} indices"
      )
      ArrayElem(typedIdent, typedIndices, SemanticData.Error(info.pos))
    else
      val finalType = elemType.satisfies(c, info.pos, tcState.errorLogger).getOrElse(?)
      ArrayElem(typedIdent, typedIndices, SemanticData.Typed(info.pos, finalType))

  /** Type-check an Ident
    *
    * @param ident:
    *   Identifier to type check
    * @param c:
    *   The constraint imposed on the identifier
    *
    * @return:
    *   `Ident[SemanticData]` type-checked `Ident` node
    */
  private def checkIdent(ident: Ident[SemanticData], c: Constraint)(using
      tcState: TCState
  ): Ident[SemanticData] = ident match {

    /* Check for type mismatch error */
    case Ident(_, SemanticData.Var(pos, _, actualType)) =>
      actualType.satisfies(c, pos, tcState.errorLogger)
      ident

    /* Variable not found by renamer (error already logged) */
    case Ident(_, SemanticData.Error(_)) =>
      ident

    case _ => ident
  }

  /** Type-check a list of expressions
    *
    * @param stmtList:
    *   List of statements to type check
    *
    * @return:
    *   `List[Stmt[SemanticData]]` list of type-checked statements
    */
  private def checkStmtList(
      stmtList: List[Stmt[SemanticData]]
  )(using TCState): List[Stmt[SemanticData]] = {
    stmtList.map(s => checkStmt(s))
  }

  /** Type-check a literal
    *
    * @param value:
    *   The literal value
    * @param info:
    *   The SemanticData annotation of the literal
    * @param constructor:
    *   The apply method of the literal
    * @param c:
    *   The constraint to type-check the call against
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return:
    *   The type-checked literal node
    */
  private def checkLiteral[A](
      value: A,
      info: SemanticData.Typed,
      constructor: (A, SemanticData) => Expr[SemanticData],
      c: Constraint
  )(using tcState: TCState): Expr[SemanticData] = {
    val SemanticData.Typed(pos, actualType) = info
    val ty = actualType.satisfies(c, pos, tcState.errorLogger).getOrElse(?)
    constructor(value, info.withType(ty))
  }

  /** Type-check an expression
    *
    * @param expr
    *   Expression to type check
    * @param c:
    *   The constraint imposed on the expression
    * @param tcState:
    *   The type-checker state with an error logger, current function name, and
    *   function table
    *
    * @return:
    *   `Expr[SemanticData]` type-checked `Expr` node
    */
  private def checkExpr(expr: Expr[SemanticData], c: Constraint)(using
      tcState: TCState
  ): Expr[SemanticData] = (expr: @unchecked) match {

    case IntLiter(value, info: SemanticData.Typed) =>
      checkLiteral(value, info, IntLiter.apply, c)

    case BoolLiter(value, info: SemanticData.Typed) =>
      checkLiteral(value, info, BoolLiter.apply, c)

    case CharLiter(value, info: SemanticData.Typed) =>
      checkLiteral(value, info, CharLiter.apply, c)

    case StrLiter(value, info: SemanticData.Typed) =>
      checkLiteral(value, info, StrLiter.apply, c)

    case PairLiter(info: SemanticData.Typed) =>
      checkLiteral((), info, (_, data) => PairLiter(data), c)

    case i: Ident[SemanticData] => checkIdent(i, c)

    case uExpr: UnaryExpr[SemanticData] => checkUnaryExpr(uExpr, c)

    case bExpr: BinaryExpr[SemanticData] => checkBinaryExpr(bExpr, c)

    case a: ArrayElem[SemanticData] => checkArrayAccess(a, c)
  }

  /** Type-check a unary expression
    *
    * The type table for unary expressions is given as
    * | Operator | Argument Type | Return Type |
    * |:---------|:--------------|:------------|
    * | !        | bool          | bool        |
    * | -        | int           | int         |
    * | len      | τ[]           | int         |
    * | ord      | char          | int         |
    * | chr      | int           | char        |
    *
    * Based on the operator type, it emposes an input constraint on the input
    * expression and checks that the constraint matches the result type.
    *
    * @param unExpr
    *   the unary expression to type-check
    * @param c
    *   constraint imposed on the unary expression
    * @param tcState
    *   type checker state
    *
    * @return:
    *   type-annotated unary expression
    */
  private def checkUnaryExpr(unExpr: UnaryExpr[SemanticData], c: Constraint)(
      using tcState: TCState
  ): UnaryExpr[SemanticData] = {
    val UnaryExpr(op, expr, SemanticData.Pos(pos)) = (unExpr: @unchecked)
    val (inputConstraint, resultType) = op match {
      case UnaryOperator.Not => (Constraint.Is(KnownType.Bool), KnownType.Bool)
      case UnaryOperator.Neg => (Constraint.Is(KnownType.Int), KnownType.Int)
      case UnaryOperator.Len => (Constraint.Is(KnownType.Array(?)), KnownType.Int)
      case UnaryOperator.Ord => (Constraint.Is(KnownType.Char), KnownType.Int)
      case UnaryOperator.Chr => (Constraint.Is(KnownType.Int), KnownType.Char)
    }

    val typedExpr = checkExpr(expr, inputConstraint)
    val finalType =
      resultType.satisfies(c, pos, tcState.errorLogger).getOrElse(?)
    UnaryExpr(op, typedExpr, SemanticData.Typed(pos, finalType))
  }

  /** Type-check a binary expression
    *
    * The type table for binary expressions is given as
    * | Operators     | Left-Hand Type | Right-Hand Type | Return Type |      |      |
    * |:--------------|:---------------|:----------------|:------------|:-----|:-----|
    * | *  /  %  +  - | int            | int             | int         |      |      |
    * | >  >=  <  <=  | int / char     | int / char      | bool        |      |      |
    * | ==  !=        | τ              | τ               | bool        |      |      |
    * | &&            |                |                 | bool        | bool | bool |
    *
    * Based on the operator type, it emposes input constraints on the input
    * expressions and checks that the constraint matches the result type.
    *
    * For comparison operators (==, >=, <=, !=, >, <), the types of left and
    * right expressions should match.
    *
    * @param binExpr
    *   the binary expression to type-check
    * @param c
    *   constraint imposed on the binary expression
    * @param tcState
    *   type checker state
    *
    * @return:
    *   type-annotated binary expression
    */
  private def checkBinaryExpr(binExpr: BinaryExpr[SemanticData], c: Constraint)(
      using tcState: TCState
  ): BinaryExpr[SemanticData] = {
    /* Unary expression constraints and types */
    val BinaryExpr(op, left, right, SemanticData.Pos(pos)) =
      (binExpr: @unchecked)
    val (inputLConstraint, inputRConstraint, resultType) = op match {
      case BinaryOperator.Add | BinaryOperator.Sub | BinaryOperator.Mul |
          BinaryOperator.Div | BinaryOperator.Mod =>
        (
          Constraint.Is(KnownType.Int),
          Constraint.Is(KnownType.Int),
          KnownType.Int
        )
      case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT |
          BinaryOperator.LTE =>
        (IsComparable, IsComparable, KnownType.Bool)
      case BinaryOperator.EQ | BinaryOperator.NEQ =>
        (Constraint.IsEquatable, Constraint.IsEquatable, KnownType.Bool)
      case BinaryOperator.And | BinaryOperator.Or =>
        (
          Constraint.Is(KnownType.Bool),
          Constraint.Is(KnownType.Bool),
          KnownType.Bool
        )
    }

    val typedLExpr = checkExpr(left, inputLConstraint)

    /* Constraint for the right expression: it is either the constraint defined
           above if the operator is not a comparison operator, or it is the same constraint
           as the left expression. */
    val constraintR = op match {
      case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT |
          BinaryOperator.LTE | BinaryOperator.EQ | BinaryOperator.NEQ =>

        /* Semantic type of the right expression */
        val rightEnforcedType = typedLExpr.info match {
          case SemanticData.Typed(_, t)  => t
          case SemanticData.Var(_, _, t) => t
          case _                         => ?
        }
        Constraint.Is(rightEnforcedType)

      case _ => inputRConstraint
    }

    val typedRExpr = checkExpr(right, constraintR)
    val finalType =
      resultType.satisfies(c, pos, tcState.errorLogger).getOrElse(?)
    BinaryExpr(op, typedLExpr, typedRExpr, SemanticData.Typed(pos, finalType))
  }

  extension (ty1: SemanticType) {

    /** Type compatibility: ty1 ~ ty2 succeeds if the two types are compatible
      * and returns the "stricter" resulting type.
      */
    def ~(ty2: SemanticType): Option[SemanticType] = (ty1, ty2) match {
      case (?, ty) => Some(ty)
      case (ty, ?) => Some(ty)

      case (KnownType.Array(KnownType.Char), KnownType.String) => Some(KnownType.String)

      case (KnownType.Array(ety1), KnownType.Array(ety2)) =>
        (ety1 ~ ety2) match {
          case Some(resultType) if resultType == ety1 && resultType == ety2 =>
            /* Both types are the same */
            Some(KnownType.Array(resultType))
          case Some(resultType) if ety1 == ? || ety2 == ? =>
            /* One side is wildcard, allow matching */
            Some(KnownType.Array(resultType))
          case _ =>
            /* Types differ and no wildcard - reject due to array invariance */
            None
        }

      /* erased pair or null initialiser should match a non-erased pair type*/
      case (PairAny | Null, KnownType.Pair(fst, snd)) =>
        Some(KnownType.Pair(fst, snd))
      case (KnownType.Pair(fst, snd), PairAny | Null) =>
        Some(KnownType.Pair(fst, snd))

      /* erased pair and null should match each other*/
      case (PairAny, Null) | (Null, PairAny) => Some(Null)

      /* two pairs are compatible if their respective types are compatible */
      case (KnownType.Pair(ty11, ty12), KnownType.Pair(ty21, ty22)) =>
        for {
          t1 <- ty11 ~ ty21
          t2 <- ty12 ~ ty22
        } yield KnownType.Pair(t1, t2)

      /* exact type match */
      case (ty1, ty2) if ty1 == ty2 => Some(ty1)

      case _ => None
    }

    /** Checks whether type `ty` matches constraint `c`. This method performs no
      * error logging.
      *
      * @param c:
      *   The constraint to check the type against
      *
      * @return
      *   `Some(SemanticType)` on success, or `None` on failure.
      */
    private def matches(c: Constraint): Option[SemanticType] = (ty1, c) match {
      case (kty, Constraint.Is(refTy)) => kty ~ refTy

      case (kty, Constraint.OneOf(ts)) =>
        ts.iterator
          .map(t => kty.matches(Constraint.Is(t)))
          .collectFirst { case Some(res) => res }
    }

    /** Checks whether a type satisfies a given constraint, reports an error on
      * failure.
      *
      * @param c:
      *   The constraint to check the type against
      * @param pos:
      *   The position where the type is used for error logging
      * @param logger:
      *   The error logger for logging a type mismatch error
      *
      * @return
      *   `Some(SemanticType)` if the constraint is satisfied, otherwise logs a
      *   type mismatch error and `None`
      */
    def satisfies(
        c: Constraint,
        pos: SourcePos,
        logger: ErrorLogger
    ): Option[SemanticType] = {
      ty1.matches(c).orElse {
        logger.logError(
          pos,
          s"Type error: Expected '$c' but got '$ty1'"
        )
        None
      }

    }
  }

}
