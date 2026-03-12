package wacc.frontend

import scala.math.{max}
import scala.collection.mutable
import SyntacticData.*

/** State used throughout the renaming process. Contains mutable structures for
  * tracking variable IDs and function signatures, and the error logger.
  */
class RenamerState(
    val variableIdMap: mutable.Map[String, Int],
    val funcSignatureTable: mutable.Map[String, SemanticData.Func],
    val structSignatureTable: mutable.Map[String, KnownType.Record],
    val errorLogger: ErrorLogger
) {

  /** Get the next unique ID for a variable name and increment the counter */
  def fresh(name: String): Int = {
    val id = variableIdMap.getOrElse(name, 0)
    variableIdMap(name) = id + 1
    id
  }

  /** Emit an error message */
  def emit(pos: SourcePos, msg: String, extraInfo: String = "") =
    errorLogger.logSemanticError(pos, msg, extraInfo)

  /** Get the function signature table as an immutable map */
  def allFuncs: Map[String, SemanticData.Func] = funcSignatureTable.toMap
}

/** Renamer stage of the frontend compiler pipeline.
  *
  * Renames variables names to be unique so the type checker doesn't have to
  * deal with scope. Reports semantic errors for redeclaration (functions and
  * variables) and variable out of scope errors. Converts the syntactic type
  * system to the semantic one for the type checker.
  */
object Renamer {

  /* The normalised edit-distance threshold beyond which an in-scope
     variable is close enough to an out-of-scope error identifier to
     be suggested */
  val guessThreshold = 0.7

  /** Renames the input Program AST and logs any semantic errors found.
    *
    * @param prog:
    *   The `Program` abstract syntax tree to rename
    * @param errorLogger:
    *   A `ErrorLogger` object to which semantic errors are logged
    * @return:
    *   The renamed AST
    */
  def rename(
      prog: Program[SyntacticData],
      errorLogger: ErrorLogger
  ): Program[SemanticData] = {

    /* Initialise state */
    given state: RenamerState = RenamerState(
      variableIdMap = mutable.Map.empty,
      funcSignatureTable = mutable.Map.empty,
      structSignatureTable = mutable.Map.empty,
      errorLogger = errorLogger
    )

    /* The current scope table */
    given scope: RenamerScope = RenamerScope.empty

    /* Set for finding duplicate struct names */
    val seenStructNames: mutable.Set[String] = mutable.Set()

    /* Process struct definitions */
    prog.structs.foreach { s =>
      val structName = s.ident.name

      if (seenStructNames.contains(structName)) {
        val errorMsg =
          s"Scope error: '${structName}' is already defined as a struct"
        state.emit(s.ident.info.pos, errorMsg)
      } else {
        seenStructNames += structName
      }

      val fieldNames = mutable.Set[String]()
      val fields = s.fields.map { f =>
        val fieldName = f.ident.name
        if (fieldNames.contains(fieldName)) {
          val errorMsg =
            s"Scope error: Field '${fieldName}' is already defined in struct '${structName}'"
          state.emit(f.ident.info.pos, errorMsg)
        }
        fieldNames += fieldName
        (fieldName, syntaxToSemantics(f.typ))
      }

      val recordType: KnownType.Record = KnownType.Record(structName, fields)
      state.structSignatureTable += (structName -> recordType)
    }

    /* Set for finding duplicate function names */
    val seenFuncNames: mutable.Set[String] = mutable.Set()

    /* Add the functions to the function signature map */
    prog.funcs.foreach { f =>
      val funcName = f.ident.name

      /* Trying to define a function with the same name */
      if (seenFuncNames.contains(funcName)) {
        val errorMsg =
          s"Scope error: '${funcName}' is already defined as a function"
        state.emit(f.ident.info.pos, errorMsg)
      } else {
        seenFuncNames += funcName
      }

      /* Convert to semantic types */
      val retType = syntaxToSemantics(f.typ)
      val paramTypes = f.params.map { p => syntaxToSemantics(p.typ) }

      /* Record function signature for the typechecker */
      val funcSignature: SemanticData.Func = SemanticData.Func(
        pos = f.info.pos,
        name = funcName,
        returnType = retType,
        params = paramTypes
      )
      state.funcSignatureTable += (funcName -> funcSignature)
    }

    /* Rename the functions and program statement */
    val newStructs = prog.structs.map(renameStructDef)
    val newFuncs = prog.funcs.map(f => renameFunc(f)(using scope.newScope))
    val newStmt = renameStmt(prog.stmt)

    Program(
      imports = Nil,
      structs = newStructs,
      funcs = newFuncs,
      stmt = newStmt,
      info = SemanticData.TopLevel(state.allFuncs, state.structSignatureTable.toMap)
    )
  }

  /** Renames a `Func` AST node
    *
    * @param func:
    *   The `Func` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Func` node
    */
  private def renameFunc(
      func: Func[SyntacticData]
  )(using scope: RenamerScope, state: RenamerState): Func[SemanticData] = {
    val Func(typ, ident, paramList, stmt, synPos) = func

    /* Tag the Ident() (function name identifier) node with SemanticData.Func */
    val funcSignature = state.funcSignatureTable(ident.name)
    val funcNameIdent = Ident[SemanticData](ident.name, funcSignature)

    val renamedParams = paramList.map(renameParam)
    val renamedBody = renameStmt(stmt, inFunc = true)

    Func(
      typ,
      funcNameIdent,
      renamedParams,
      renamedBody,
      synPos.toSemanticPos
    )
  }

  private def renameStructDef(
      structDef: StructDef[SyntacticData]
  ): StructDef[SemanticData] = {
    val StructDef(ident, fields, synPos) = structDef
    val renamedIdent = Ident[SemanticData](ident.name, synPos.toSemanticPos)
    val renamedFields = fields.map { f =>
      Field(f.typ, Ident[SemanticData](f.ident.name, f.ident.info.toSemanticPos), f.info.toSemanticPos)
    }
    StructDef(renamedIdent, renamedFields, synPos.toSemanticPos)
  }

  /** Renames a `Param` AST node
    *
    * @param param:
    *   The `Param` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Param` node
    */
  private def renameParam(
      param: Param[SyntacticData]
  )(using scope: RenamerScope, state: RenamerState): Param[SemanticData] = {

    val Param(typ, ident, synPos) = param

    /* Define the SemanticData to annotate a Param() node with either
           a SemanticData.Error annotation in the redeclaration case or
           SemanticData.Pos */
    val paramSemData =
      if (scope.contains(ident.name)) {
        val errorMsg =
          s"Scope error: Parameter '${ident.name}' is already defined for this function"
        state.emit(ident.info.pos, errorMsg)
        SemanticData.Error(ident.info.pos)
      } else {
        synPos.toSemanticPos
      }

    /* Add the parameters (renamed) to the function's scope */
    val renameId = state.fresh(ident.name)
    val identSemData: SemanticData.Var =
      SemanticData.Var(ident.info.pos, renameId, syntaxToSemantics(typ))
    scope.add(ident.name, identSemData)

    Param(typ, Ident(ident.name, identSemData), paramSemData)
  }

  /** Renames a `Stmt` AST node
    *
    * @param stmt:
    *   The `Stmt` abstract syntax tree node to rename
    * @param inFunc:
    *   True if the statement is inside a function, false otherwise
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Stmt` node
    */
  private def renameStmt(
      stmt: Stmt[SyntacticData],
      inFunc: Boolean = false
  )(using scope: RenamerScope, state: RenamerState): Stmt[SemanticData] = {
    (stmt: @unchecked) match {
      case Stmt.Skip(synPos)       => Stmt.Skip(synPos.toSemanticPos)
      case Stmt.Exit(expr, synPos) =>
        Stmt.Exit(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Print(expr, synPos) =>
        Stmt.Print(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Println(expr, synPos) =>
        Stmt.Println(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Return(expr, synPos)    => renameReturn(expr, synPos, inFunc)
      case Stmt.Block(stmtList, synPos) =>
        renameBlock(stmtList, synPos, inFunc)

      case Stmt.If(expr, stmtIf, stmtElse, synPos) =>
        Stmt.If(
          renameExpr(expr),
          renameStmt(stmtIf, inFunc),
          renameStmt(stmtElse, inFunc),
          synPos.toSemanticPos
        )

      case Stmt.While(condExpr, stmt, synPos) =>
        Stmt.While(
          renameExpr(condExpr),
          renameStmt(stmt, inFunc),
          synPos.toSemanticPos
        )

      case Stmt.Decl(syntacticType, ident, rval, synPos) =>
        renameDecl(ident, rval, synPos, syntacticType)

      case Stmt.Asgn(lval, rval, synPos) =>
        Stmt.Asgn(
          renameLValue(lval),
          renameRValue(rval),
          synPos.toSemanticPos
        )

      case Stmt.Read(lval, synPos) =>
        Stmt.Read(renameLValue(lval), synPos.toSemanticPos)
      case Stmt.Free(expr, synPos) =>
        Stmt.Free(renameExpr(expr), synPos.toSemanticPos)
    }
  }

  /** Renames a `Decl` AST node (statement)
    *
    * @param ident:
    *   The identifier of the declaration
    * @param rval:
    *   The RValue being assigned to the new identifier
    * @param synPos:
    *   The syntactic position of the declaration
    * @param syntacticType:
    *   The syntactic type of the declaration
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Decl` node
    */
  private def renameDecl(
      ident: Ident[SyntacticData],
      rval: RValue[SyntacticData],
      synPos: SyntacticData,
      syntacticType: SyntaxType
  )(using
      scope: RenamerScope,
      state: RenamerState
  ): Stmt.Decl[SemanticData] = {

    val renamedRVal = renameRValue(rval)

    /* Check for a variable re-declaration error */
    if (scope.contains(ident.name)) {
      val originalVar = scope.lookup(ident.name).get
      val oldPos = originalVar.pos
      val errorMsg =
        s"Scope error: Variable '${ident.name}' is already declared in this scope"
      val originalDeclMsg =
        s"\nOriginal declaration was at Line ${oldPos(0)}, Column ${oldPos(1)}"
      state.emit(ident.info.pos, errorMsg, originalDeclMsg)
    }

    /* Add a renamed variable to the scope table */
    val renameId = state.fresh(ident.name)
    val varSemData: SemanticData.Var = SemanticData.Var(
      synPos.pos,
      renameId,
      syntaxToSemantics(syntacticType)
    )
    scope.add(ident.name, varSemData)

    Stmt.Decl(
      syntacticType,
      renameIdent(ident),
      renamedRVal,
      synPos.toSemanticPos
    )

  }

  /** Renames a `Block` AST node (statement)
    *
    * @param stmtList:
    *   The list of statements in the block
    * @param synPos:
    *   The syntactic start position of the block
    * @param inFunc:
    *   True if the block is in a function, false otherwise
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Block` node
    */
  private def renameBlock(
      stmtList: List[Stmt[SyntacticData]],
      synPos: SyntacticData,
      inFunc: Boolean
  )(using
      scope: RenamerScope,
      state: RenamerState
  ): Stmt.Block[SemanticData] = {

    /* Create new scope and use it for the block */
    Stmt.Block(
      renameStmtList(stmtList, inFunc)(using scope.newScope),
      synPos.toSemanticPos
    )
  }

  /** Renames a `Return` AST node (statement)
    *
    * @param expr:
    *   The expression being returned
    * @param synPos:
    *   The syntactic start position of the block
    * @param inFunc:
    *   True if the block is in a function, false otherwise
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Return` node
    */
  private def renameReturn(
      expr: Expr[SyntacticData],
      synPos: SyntacticData,
      inFunc: Boolean
  )(using
      scope: RenamerScope,
      state: RenamerState
  ): Stmt.Return[SemanticData] = {

    /* Get the SemanticData to tag the return statement with as either
           an error annotation or the position */
    val semData =
      if (!inFunc) {
        val errorMsg =
          "Semantic error: Return statements can only be used inside functions"
        state.emit(synPos.pos, errorMsg)
        SemanticData.Error(synPos.pos)
      } else {
        synPos.toSemanticPos
      }

    Stmt.Return(renameExpr(expr), semData)
  }

  /** Renames a `Call` AST node (statement)
    *
    * @param ident:
    *   The function identifier being called
    * @param exprList:
    *   The list of expressions forming the arguments to the function
    * @param synPos:
    *   The syntactic start position of the block
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Return` node
    */
  private def renameCall(
      ident: Ident[SyntacticData],
      exprList: List[Expr[SyntacticData]],
      synPos: SyntacticData
  )(using scope: RenamerScope, state: RenamerState): RValue[SemanticData] = {

    /* Find the function's identifer and SemanticData tag via a
           lookup to the function signature table */
    val (funcIdent, semInfo) =
      state.funcSignatureTable.get(ident.name) match

        /* Function signature found */
        case Some(signature) => {

          /* Number of provided and given args for mismatch checks */
          val numFuncParams = signature.params.length
          val numCallArgs = exprList.length

          /* The function's identifier from the table */
          val funcIdent =
            Ident[SemanticData](signature.name, signature)

          /* Get the SemanticData annotation for the Call() statement
                       as either an error (for argument number mismatch) or
                       a position */
          val semInfo =
            if (numFuncParams != numCallArgs) {
              val errorMsg =
                s"Argument error: Function '${ident.name}' takes ${numFuncParams}" +
                  s" arguments but ${numCallArgs} were given"
              state.emit(ident.info.pos, errorMsg)
              SemanticData.Error(ident.info.pos)
            } else {
              synPos.toSemanticPos
            }

          (funcIdent, semInfo)
        }

        /* Function being called is not defined */
        case None => {
          val errorMsg =
            s"Scope error: Function '${ident.name}' is not defined"
          state.emit(ident.info.pos, errorMsg)
          (
            Ident[SemanticData](
              ident.name,
              ident.info.toSemanticPos
            ),
            SemanticData.Error(ident.info.pos)
          )
        }

    Call(funcIdent, exprList.map(renameExpr), semInfo)
  }

  private def renameNewRecord(
      name: Ident[SyntacticData],
      args: Args[SyntacticData],
      synPos: SyntacticData
  )(using scope: RenamerScope, state: RenamerState): RValue[SemanticData] = {
    val structName = name.name
    val semInfo = state.structSignatureTable.get(structName) match {
      case Some(recordType) =>
        val numFields = recordType.fields.length
        val numArgs = args.length
        if (numFields != numArgs) {
          val errorMsg =
            s"Argument error: Struct '${structName}' has ${numFields}" +
              s" fields but ${numArgs} arguments were given"
          state.emit(name.info.pos, errorMsg)
          SemanticData.Error(name.info.pos)
        } else {
          synPos.toSemanticPos
        }
      case None =>
        val errorMsg =
          s"Scope error: Struct '${structName}' is not defined"
        state.emit(name.info.pos, errorMsg)
        SemanticData.Error(name.info.pos)
    }
    NewRecord(
      Ident[SemanticData](structName, name.info.toSemanticPos),
      args.map(renameExpr),
      semInfo
    )
  }

  /** Renames an `RValue` AST node
    *
    * @param rvalue:
    *   The `RValue` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `RValue` node
    */
  private def renameRValue(
      rvalue: RValue[SyntacticData]
  )(using scope: RenamerScope, state: RenamerState): RValue[SemanticData] =
    (rvalue: @unchecked) match {

      case ArrayLiter(elems, info) =>
        ArrayLiter(elems.map(renameExpr), info.toSemanticPos)
      case Call(ident, exprList, synPos) =>
        renameCall(ident, exprList, synPos)
      case expr: Expr[SyntacticData]   => renameExpr(expr)
      case PairElem(isFst, lval, info) =>
        PairElem(isFst, renameLValue(lval), info.toSemanticPos)
      case NewPair(e1, e2, info) =>
        NewPair(renameExpr(e1), renameExpr(e2), info.toSemanticPos)
      case NewRecord(name, args, info) =>
        renameNewRecord(name, args, info)
    }

  /** Renames an `LValue` AST node
    *
    * @param rvalue:
    *   The `LValue` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `LValue` node
    */
  private def renameLValue(
      lvalue: LValue[SyntacticData]
  )(using RenamerScope, RenamerState): LValue[SemanticData] =
    (lvalue: @unchecked) match {

      case i: Ident[SyntacticData]     => renameIdent(i)
      case PairElem(isFst, lval, info) =>
        PairElem(isFst, renameLValue(lval), info.toSemanticPos)
      case ArrayElem(ident, indices, info) =>
        ArrayElem(
          renameIdent(ident),
          indices.map(renameExpr),
          info.toSemanticPos
        )
      case FieldAccess(expr, field, info) =>
        FieldAccess(
          renameExpr(expr),
          Ident[SemanticData](field.name, field.info.toSemanticPos),
          info.toSemanticPos
        )
    }

  /** Computes the variable in scope with name most similar to `userVar`
    *
    * @param userVar:
    *   The user written variable name being checked against for close matches
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    *
    * @return:
    *   Some(str) if a close-enough in-scope identifier match was found, None if
    *   no close-enough match was found
    */
  private def bestGuessVariable(
      userVar: String,
      scope: RenamerScope
  ): Option[String] = {

    val userVarSize = userVar.length
    var bestGuess: String = ""
    var bestGuessScore: Double = 0.0

    /* Find the in-scope variable with the highest "score" (best guess) */
    for (inScopeVar <- scope.getInScope()) {

      /* Compute similarity score */
      val similarityScore = computeSimilarity(userVar, inScopeVar)

      /* Normalise the score between 0 and 1 */
      val normalisedSimilarity =
        1 - (similarityScore / (max(userVarSize, inScopeVar.length)))

      /* Update best guess */
      if (bestGuessScore < normalisedSimilarity) {
        bestGuessScore = normalisedSimilarity
        bestGuess = inScopeVar
      }
    }

    if (bestGuessScore >= guessThreshold && bestGuess.nonEmpty)
      Some(bestGuess)
    else None
  }

  /** Computes the edit distance between two strings (with classic DP)
    *
    * @param str1:
    *   The first string in the edit distance calculation
    * @param str2:
    *   The second string in the edit distance calculation
    *
    * @return:
    *   The edit-distance between the two strings
    */
  private def computeSimilarity(str1: String, str2: String): Double = {

    val len1 = str1.length
    val len2 = str2.length

    /* Initialise DP state */
    val distMat = Array.ofDim[Int](len1 + 1, len2 + 1)
    for (i <- 0 to len1) {
      distMat(i)(0) = i
    }
    for (j <- 0 to len2) {
      distMat(0)(j) = j
    }

    /* Populate the DP edit-distance table */
    for (i <- 1 to len1) {
      for (j <- 1 to len2) {
        val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
        distMat(i)(j) = Math.min(
          Math.min(distMat(i - 1)(j) + 1, distMat(i)(j - 1) + 1),
          distMat(i - 1)(j - 1) + cost
        )
      }
    }

    distMat(len1)(len2)
  }

  /** Renames an `Ident` AST node. Pre-Condition: `Ident` is tagged with
    * SemanticData.Var as it does not make sense to rename a function identifier
    * in WACC.
    *
    * @param rvalue:
    *   The `Ident` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Ident` node
    */
  private def renameIdent(
      ident: Ident[SyntacticData]
  )(using scope: RenamerScope, state: RenamerState): Ident[SemanticData] = {

    /* Get the semantic data to tag the identifier with using the
           table of in-scope variables */
    val semData = scope.lookup(ident.name) match {

      /* Variable in table so use its identifier (with the new position
               else the declaration site position is used */
      case Some(varSemData) => varSemData.copy(pos = ident.info.pos)

      /* Identifier not found so emit an error message */
      case None => {
        val errorMsg =
          s"Scope error: Variable '${ident.name}' is not defined"

        /* Attempt to suggest a similar looking in-scope variable */
        val suggestedVar = bestGuessVariable(ident.name, scope) match
          case None                  => ""
          case Some(closestVariable) =>
            s"\nDid you mean '${closestVariable}'?"

        state.emit(ident.info.pos, errorMsg, suggestedVar)

        SemanticData.Error(ident.info.pos)
      }
    }

    Ident(ident.name, semData)
  }

  /** Renames a list of `Stmt` nodes
    *
    * @param stmtList:
    *   The list of statement nodes to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed list of `Stmt` nodes
    */
  private def renameStmtList(
      stmtList: List[Stmt[SyntacticData]],
      inFunc: Boolean
  )(using
      scope: RenamerScope,
      state: RenamerState
  ): List[Stmt[SemanticData]] = {
    stmtList.map(s => renameStmt(s, inFunc))
  }

  /** Renames a literal AST node
    *
    * @param value:
    *   The value of the literal node
    * @param info:
    *   The syntactic data annotating the literal node
    * @param litType:
    *   The semantic type of the literal node
    * @param constructor:
    *   The literal node's apply method
    *
    * @return:
    *   The renamed literal AST node
    */
  private def renameLiteral[A](
      value: A,
      info: SyntacticData,
      litType: SemanticType,
      constructor: (A, SemanticData) => Expr[SemanticData]
  ): Expr[SemanticData] = {
    constructor(value, SemanticData.Typed(info.pos, litType))
  }

  /** Renames an `Expr` AST node.
    *
    * @param expr:
    *   The `Expr` abstract syntax tree node to rename
    * @param scope:
    *   A `RenamerScope` tracking in scope variables
    * @param state:
    *   A `RenamerState` containing an error logger, and error/function state
    *   maps
    *
    * @return:
    *   The renamed `Expr` node
    */
  private def renameExpr(
      expr: Expr[SyntacticData]
  )(using RenamerScope, RenamerState): Expr[SemanticData] =
    (expr: @unchecked) match {

      /* Atomic expressions */
      case IntLiter(value, info) =>
        renameLiteral(value, info, KnownType.Int, IntLiter.apply)
      case BoolLiter(value, info) =>
        renameLiteral(value, info, KnownType.Bool, BoolLiter.apply)
      case CharLiter(value, info) =>
        renameLiteral(value, info, KnownType.Char, CharLiter.apply)
      case StrLiter(value, info) =>
        renameLiteral(value, info, KnownType.String, StrLiter.apply)
      case PairLiter(info) => PairLiter(SemanticData.Typed(info.pos, Null))
      case i @ Ident(name, info) => renameIdent(i)

      /* Unary expressions */
      case UnaryExpr(op, expr, synPos) =>
        UnaryExpr(op, renameExpr(expr), synPos.toSemanticPos)

      /* Binary expressions */
      case BinaryExpr(op, left, right, synPos) =>
        BinaryExpr(
          op,
          renameExpr(left),
          renameExpr(right),
          synPos.toSemanticPos
        )

      /* Array element access */
      case ArrayElem(ident, indices, info) =>
        ArrayElem(
          renameIdent(ident),
          indices.map(renameExpr),
          info.toSemanticPos
        )

      /* Field access */
      case FieldAccess(expr, field, info) =>
        FieldAccess(
          renameExpr(expr),
          Ident[SemanticData](field.name, field.info.toSemanticPos),
          info.toSemanticPos
        )
    }
}
