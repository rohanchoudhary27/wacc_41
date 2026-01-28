package wacc

import scala.collection.mutable.Map
import scala.math.{max}
import SyntacticData.*

/**
* Renamer stage of the frontend compiler pipeline.
*
* Renames variables names to be unique so the type checker doesn't have to deal with
* scope. Reports semantic errors for redeclaration (functions and variables) and
* variable out of scope errors. Converts the syntactic type system to the semantic one
* for the type checker.
*
*/
class Renamer {

  /* Map from variable name to the next id to use when renaming
     a variable of the same name (shadowed). */
  private val variableIdMap = Map[String, Int]()

  /* Map from function name to a function signature object. */
  private val funcSignatureTable = Map[String, SemanticData.Func]()

  /* Top-level symbol table to track variable scope */
  var varSymbolTable = new SymbolTable(None)

  /* Retrieves the next unused id to rename a variable. Updates
     the map for the next call. */
  private def getNextId(varStr: String) = {
    val id = variableIdMap.getOrElse(varStr, 0)
    variableIdMap(varStr) = id + 1
    id
  }

  /**
  * Renames the input Program AST and logs any semantic errors found.
  *
  * @param prog: The `Program` abstract syntax tree to rename
  * @param info: A `SemanticErrorLogger` object to which semantic errors are logged
  *
  * @return: The renamed AST
  */
  def rename(prog: Program[SyntacticData], errorLogger: SemanticErrorLogger): Program[SemanticData] = {

    /* Add the functions to the function signature map */
    prog.funcs.foreach {f =>
      
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
      funcSignatureTable += (funcName -> funcSignature)
    }

    /* Rename the functions and program statement */
    val newFuncs = prog.funcs.map(f => renameFunc(f))
    val newStmt = renameStmt(prog.stmt, varSymbolTable, errorLogger)
    
    Program(newFuncs, newStmt, SemanticData.TopLevel(funcSignatureTable.toMap))
  }

  private def renameFunc(func: Func[SyntacticData]): Func[SemanticData] = ???

  private def renameStmt(stmt: Stmt[SyntacticData], symbolTable: SymbolTable, errorLogger: SemanticErrorLogger): Stmt[SemanticData] = {
    stmt match {
      case Stmt.Skip(synPos) => Stmt.Skip(synPos.toSemanticPos)
      case Stmt.Exit(expr, synPos) => Stmt.Exit(renameExpr(expr, symbolTable, errorLogger), synPos.toSemanticPos)
      case Stmt.Print(expr, synPos) => Stmt.Print(renameExpr(expr, symbolTable, errorLogger), synPos.toSemanticPos)
      case Stmt.Println(expr, synPos) => Stmt.Println(renameExpr(expr, symbolTable, errorLogger), synPos.toSemanticPos)
      case Stmt.Block(stmtList, synPos) => renameBlock(stmtList, synPos, symbolTable, errorLogger)

      case Stmt.If(expr, stmtIf, stmtElse, synPos) => Stmt.If(renameExpr(expr, symbolTable, errorLogger),
                                                              renameStmt(stmtIf, symbolTable, errorLogger),
                                                              renameStmt(stmtElse, symbolTable, errorLogger),
                                                              synPos.toSemanticPos)

      case Stmt.While(condExpr, stmt, synPos) => Stmt.While(renameExpr(condExpr, symbolTable, errorLogger),
                                                            renameStmt(stmt, symbolTable, errorLogger),
                                                            synPos.toSemanticPos)

      case Stmt.Decl(syntacticType, ident, rval, synPos) => {

        val renamedRVal = renameRValue(rval, symbolTable, errorLogger)

        val renameId = getNextId(ident.name)
        val varSemData: SemanticData.Var =
          SemanticData.Var(synPos.pos, renameId, syntaxToSemantics(syntacticType))

        if (!symbolTable.set(ident.name, varSemData)) {
          errorLogger.logError(synPos.pos, s"Scope error: Variable ${ident.name} is already declared in this scope")
        }

        Stmt.Decl(syntacticType, renameIdent(ident, symbolTable, errorLogger), renamedRVal, synPos.toSemanticPos)

      }
      case Stmt.Asgn(lval, rval, synPos) => {
        val renamedLVal = renameLValue(lval, symbolTable, errorLogger)
        val renamedRVal = renameRValue(rval, symbolTable, errorLogger)
        Stmt.Asgn(renamedLVal, renamedRVal, synPos.toSemanticPos)
      }
      case Stmt.Read(lval, synPos) => {
        val renamedLVal = renameLValue(lval, symbolTable, errorLogger)
        Stmt.Read(renamedLVal, synPos.toSemanticPos)
      }
      case _ => ???
    }
  }

  private def renameBlock(stmtList: List[Stmt[SyntacticData]],
                          synPos: SyntacticData,
                          symbolTable: SymbolTable,
                          errorLogger: SemanticErrorLogger): Stmt.Block[SemanticData] = {

        val childScope = new SymbolTable(Some(symbolTable))
        Stmt.Block(renameStmtList(stmtList, childScope, errorLogger), synPos.toSemanticPos)
  }

  private def renameRValue(rvalue: RValue[SyntacticData],
                           symbolTable: SymbolTable,
                           errorLogger: SemanticErrorLogger): RValue[SemanticData] = rvalue match
    // right now, rvalue is only ever an expr but later other cases will go here
    case expr: Expr[SyntacticData] => renameExpr(expr, symbolTable, errorLogger)
    case _                         => ???

  private def renameLValue(lvalue: LValue[SyntacticData],
                           symbolTable: SymbolTable,
                           errorLogger: SemanticErrorLogger): LValue[SemanticData] = lvalue match
    case i: Ident[SyntacticData] => renameIdent(i, symbolTable, errorLogger)
    case _                       => ???
  

  /* Computes the variable in scope with name most similar to `userVar` */
  private def bestGuessVariable(userVar: String, symbolTable: SymbolTable): Option[String] = {

    val userVarSize = userVar.length
    var bestGuess: String = ""
    var bestGuessScore: Double = 0.0
    val guessThreshold = 0.5

    for (inScopeVar <- symbolTable.getInScope()) {
      val similarityScore = computeSimilarity(userVar, inScopeVar)
      val normalisedSimilarity = 1 - (similarityScore / (math.max(userVarSize, inScopeVar.length)))
      if (bestGuessScore < normalisedSimilarity) {
        bestGuessScore = normalisedSimilarity
        bestGuess = inScopeVar
      }
    }

    if (bestGuessScore >= guessThreshold && bestGuess.nonEmpty) Some(bestGuess) else None
  }

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

  private def renameIdent(ident: Ident[SyntacticData],
                          symbolTable: SymbolTable,
                          errorLogger: SemanticErrorLogger): Ident[SemanticData] = {

    val semData = symbolTable.get(ident.name) match
      case Some(varSemData) => varSemData
      case None => {
        val errorMsg = s"Scope error: Variable ${ident.name} is not defined"

        val suggestedVar = bestGuessVariable(ident.name, symbolTable) match
          case None => ""
          case Some(closestVariable) => s"\nDid you mean ${closestVariable}?"
        
        errorLogger.logError(ident.info.pos, errorMsg, suggestedVar)
        SemanticData.Error(ident.info.pos)
      }

    Ident(ident.name, semData)
  }

  private def renameStmtList(exprList: List[Stmt[SyntacticData]],
                             symbolTable: SymbolTable,
                             errorLogger: SemanticErrorLogger): List[Stmt[SemanticData]] = {
    exprList.map(s => renameStmt(s, symbolTable, errorLogger))
  }

  private def renameLiteral[A](
      value: A,
      info: SyntacticData,
      litType: SemanticType,
      constructor: (A, SemanticData) => Expr[SemanticData]
  ): Expr[SemanticData] = {
    constructor(value, SemanticData.Expr(info.pos, litType))
  }

  private def renameExpr(expr: Expr[SyntacticData],
                         symbolTable: SymbolTable,
                         errorLogger: SemanticErrorLogger): Expr[SemanticData] = expr match {
    case IntLiter(value, info) => renameLiteral(value, info, KnownType.Int, IntLiter.apply)
    case BoolLiter(value, info) => renameLiteral(value, info, KnownType.Bool, BoolLiter.apply)
    case CharLiter(value, info) => renameLiteral(value, info, KnownType.Char, CharLiter.apply)
    case StrLiter(value, info) => renameLiteral(value, info, KnownType.String, StrLiter.apply)
    case PairLiter(info) => PairLiter(SemanticData.Expr(info.pos, Null))
    case i @ Ident(name, info) => renameIdent(i, symbolTable, errorLogger)
    case UnaryExpr(op, expr, synPos) => UnaryExpr(op, renameExpr(expr, symbolTable, errorLogger), synPos.toSemanticPos)
    case BinaryExpr(op, left, right, synPos) => BinaryExpr(op, renameExpr(left, symbolTable, errorLogger), renameExpr(right,symbolTable, errorLogger), synPos.toSemanticPos)
    case _ => ???
  }
}
