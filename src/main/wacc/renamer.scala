package wacc

import scala.math.{max}
import scala.collection.mutable
import SyntacticData.*

/**
* State that remains fixed throughout the renaming process.
* Contains mutable structures for tracking variable IDs and function signatures,
* and the error logger.
*/
class RenamerState(
  val variableIdMap: mutable.Map[String, Int],
  val funcSignatureTable: mutable.Map[String, SemanticData.Func],
  val errorLogger: SemanticErrorLogger
) {
  /** Get the next unique ID for a variable name and increment the counter */
  def fresh(name: String): Int = {
    val id = variableIdMap.getOrElse(name, 0)
    variableIdMap(name) = id + 1
    id
  }
  
  /** Emit an error message */
  def emit(pos: SourcePos, msg: String, extraInfo: String = "") =
    errorLogger.logError(pos, msg, extraInfo)
  
  /** Get the function signature table as an immutable map */
  def allFuncs: Map[String, SemanticData.Func] = funcSignatureTable.toMap
}

/**
* Renamer stage of the frontend compiler pipeline.
*
* Renames variables names to be unique so the type checker doesn't have to deal with
* scope. Reports semantic errors for redeclaration (functions and variables) and
* variable out of scope errors. Converts the syntactic type system to the semantic one
* for the type checker.
*/
object Renamer {

  /* The normalised edit-distance threshold beyond which an in-scope
     variable is close enough to an out-of-scope error identifier to
     be suggested */
  val guessThreshold = 0.7

  /**
  * Renames the input Program AST and logs any semantic errors found.
  *
  * @param prog: The `Program` abstract syntax tree to rename
  * @param errorLogger: A `SemanticErrorLogger` object to which semantic errors are logged
  *
  * @return: The renamed AST
  */
  def rename(prog: Program[SyntacticData], errorLogger: SemanticErrorLogger): Program[SemanticData] = {
    /* Initialise state */
    given state: RenamerState = RenamerState(
      mutable.Map.empty,
      mutable.Map.empty,
      errorLogger
    )
    given scope: RenamerScope = RenamerScope.empty

    /* Add the functions to the function signature map */
    prog.funcs.foreach { f =>
      
      /* Convert to semantic types */
      val retType = syntaxToSemantics(f.typ)
      val paramTypes = f.params.map { p => syntaxToSemantics(p.typ) }

      val funcName = f.ident.name
      val funcSignature: SemanticData.Func = SemanticData.Func(
          pos = f.info.pos,
          name = funcName,
          returnType = retType,
          params = paramTypes
      )

      /* Record function for the typechecker */
      state.funcSignatureTable += (funcName -> funcSignature)
    }

    /* Rename the functions and program statement */
    val newFuncs = prog.funcs.map(f => renameFunc(f))
    val newStmt = renameStmt(prog.stmt)
    
    Program(newFuncs, newStmt, SemanticData.TopLevel(state.allFuncs))
  }

  private def renameFunc(func: Func[SyntacticData])(using RenamerScope, RenamerState): Func[SemanticData] = ???

  private def renameStmt(stmt: Stmt[SyntacticData])(using scope: RenamerScope, state: RenamerState): Stmt[SemanticData] = {
    stmt match {
      case Stmt.Skip(synPos) => Stmt.Skip(synPos.toSemanticPos)
      case Stmt.Exit(expr, synPos) => Stmt.Exit(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Print(expr, synPos) => Stmt.Print(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Println(expr, synPos) => Stmt.Println(renameExpr(expr), synPos.toSemanticPos)
      case Stmt.Block(stmtList, synPos) => renameBlock(stmtList, synPos)

      case Stmt.If(expr, stmtIf, stmtElse, synPos) => Stmt.If(renameExpr(expr),
                                                              renameStmt(stmtIf),
                                                              renameStmt(stmtElse),
                                                              synPos.toSemanticPos)

      case Stmt.While(condExpr, stmt, synPos) => Stmt.While(renameExpr(condExpr),
                                                            renameStmt(stmt),
                                                            synPos.toSemanticPos)

      case Stmt.Decl(syntacticType, ident, rval, synPos) => {
        val renamedRVal = renameRValue(rval)

        if (scope.contains(ident.name)) {
          val originalVar = scope.lookup(ident.name).get
          val oldPos = originalVar.pos
          val errorMsg = s"Scope error: Variable '${ident.name}' is already declared in this scope"
          val originalDeclMsg = s"\nOriginal declaration was at Line ${oldPos(0)}, Column ${oldPos(1)}"
          state.emit(synPos.pos, errorMsg, originalDeclMsg)
        }

        val renameId = state.fresh(ident.name)
        val varSemData: SemanticData.Var = SemanticData.Var(synPos.pos, renameId, syntaxToSemantics(syntacticType))
        scope.add(ident.name, varSemData)
        Stmt.Decl(syntacticType, renameIdent(ident), renamedRVal, synPos.toSemanticPos)
      }
      case Stmt.Asgn(lval, rval, synPos) => {
        val renamedLVal = renameLValue(lval)
        val renamedRVal = renameRValue(rval)
        Stmt.Asgn(renamedLVal, renamedRVal, synPos.toSemanticPos)
      }
      case Stmt.Read(lval, synPos) => {
        val renamedLVal = renameLValue(lval)
        Stmt.Read(renamedLVal, synPos.toSemanticPos)
      }
      case _ => ???
    }
  }

  private def renameBlock(stmtList: List[Stmt[SyntacticData]],
                          synPos: SyntacticData)
                         (using scope: RenamerScope, state: RenamerState): Stmt.Block[SemanticData] = {
    /* Create new scope and use it for the block */
    Stmt.Block(renameStmtList(stmtList)(using scope.newScope), synPos.toSemanticPos)
  }

  private def renameRValue(rvalue: RValue[SyntacticData])
                          (using RenamerScope, RenamerState): RValue[SemanticData] = rvalue match {
    // right now, rvalue is only ever an expr but later other cases will go here
    case expr: Expr[SyntacticData]   => renameExpr(expr)
    case PairElem(isFst, lval, info) => PairElem(isFst, renameLValue(lval), info.toSemanticPos) // FIXME: is it SD.Pos?
    case NewPair(e1, e2, info)       => NewPair(renameExpr(e1), renameExpr(e2), info.toSemanticPos) // FIXME: is it SD.Pos?
    case _                           => ???
  }

  private def renameLValue(lvalue: LValue[SyntacticData])
                          (using RenamerScope, RenamerState): LValue[SemanticData] = lvalue match {
    case i: Ident[SyntacticData] => renameIdent(i)
    case PairElem(isFst, lval, info) => PairElem(isFst, renameLValue(lval), info.toSemanticPos)
    case _                       => ???
  }
  
  /* Computes the variable in scope with name most similar to `userVar` */
  private def bestGuessVariable(userVar: String, scope: RenamerScope): Option[String] = {

    val userVarSize = userVar.length
    var bestGuess: String = ""
    var bestGuessScore: Double = 0.0

    for (inScopeVar <- scope.getInScope()) {
      val similarityScore = computeSimilarity(userVar, inScopeVar)
      val normalisedSimilarity = 1 - (similarityScore / (max(userVarSize, inScopeVar.length)))
      if (bestGuessScore < normalisedSimilarity) {
        bestGuessScore = normalisedSimilarity
        bestGuess = inScopeVar
      }
    }

    if (bestGuessScore >= guessThreshold && bestGuess.nonEmpty) Some(bestGuess) else None
  }

  /* Compute the edit distance between two strings */
  private def computeSimilarity(str1: String, str2: String): Double = {

    val len1 = str1.length
    val len2 = str2.length

    val distMat = Array.ofDim[Int](len1 + 1, len2 + 1)

    for (i <- 0 to len1) {
      distMat(i)(0) = i
    }

    for (j <- 0 to len2) {
      distMat(0)(j) = j
    }

    for (i <- 1 to len1) {
      for (j <- 1 to len2) {
        val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1
        distMat(i)(j) = Math.min(
          Math.min(distMat(i - 1)(j) + 1,
                  distMat(i)(j - 1) + 1),
          distMat(i - 1)(j - 1) + cost
        )
      }
    }

    distMat(len1)(len2)
  }

  private def renameIdent(ident: Ident[SyntacticData])
                         (using scope: RenamerScope, state: RenamerState): Ident[SemanticData] = {
    val semData = scope.lookup(ident.name) match {
      case Some(varSemData) => varSemData.copy(pos = ident.info.pos)
      case None => {
        val errorMsg = s"Scope error: Variable '${ident.name}' is not defined"

        val suggestedVar = bestGuessVariable(ident.name, scope) match
          case None => ""
          case Some(closestVariable) => s"\nDid you mean '${closestVariable}'?"
        
        state.emit(ident.info.pos, errorMsg, suggestedVar)
        SemanticData.Error(ident.info.pos)
      }
    }
    Ident(ident.name, semData)
  }

  private def renameStmtList(exprList: List[Stmt[SyntacticData]])
                            (using RenamerScope, RenamerState): List[Stmt[SemanticData]] = {
    exprList.map(s => renameStmt(s))
  }

  private def renameLiteral[A](
      value: A,
      info: SyntacticData,
      litType: SemanticType,
      constructor: (A, SemanticData) => Expr[SemanticData]
  ): Expr[SemanticData] = {
    constructor(value, SemanticData.Expr(info.pos, litType))
  }

  private def renameExpr(expr: Expr[SyntacticData])
                        (using RenamerScope, RenamerState): Expr[SemanticData] = expr match {
    // <atom>                      
    case IntLiter(value, info) => renameLiteral(value, info, KnownType.Int, IntLiter.apply)
    case BoolLiter(value, info) => renameLiteral(value, info, KnownType.Bool, BoolLiter.apply)
    case CharLiter(value, info) => renameLiteral(value, info, KnownType.Char, CharLiter.apply)
    case StrLiter(value, info) => renameLiteral(value, info, KnownType.String, StrLiter.apply)
    case PairLiter(info) => PairLiter(SemanticData.Expr(info.pos, Null))
    case i @ Ident(name, info) => renameIdent(i)
    // <unary-oper> <expr>
    case UnaryExpr(op, expr, synPos) => UnaryExpr(op, renameExpr(expr), synPos.toSemanticPos)
    // <expr> <binary-oper> <expr
    case BinaryExpr(op, left, right, synPos) => BinaryExpr(op, renameExpr(left), renameExpr(right), synPos.toSemanticPos)
    case _ => ???
  }
}
