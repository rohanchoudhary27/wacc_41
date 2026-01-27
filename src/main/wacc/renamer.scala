package wacc

import scala.collection.mutable.Map

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
          pos = f.info match { case SyntacticData.Pos(pos) => pos },
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
      case Stmt.Skip(synPos) => Stmt.Skip(synPos.asSemanticData.get)
      case Stmt.Exit(expr, synPos) => Stmt.Exit(renameExpr(expr, symbolTable, errorLogger), synPos.asSemanticData.get)
      case Stmt.Print(expr, synPos) => Stmt.Print(renameExpr(expr, symbolTable, errorLogger), synPos.asSemanticData.get)
      case Stmt.Println(expr, synPos) => Stmt.Println(renameExpr(expr, symbolTable, errorLogger), synPos.asSemanticData.get)
      case Stmt.Block(stmtList, synPos) => renameBlock(stmtList, synPos, symbolTable, errorLogger)
      case Stmt.Decl(syntacticType, ident, rval, synVar) => {

        val renamedRVal = renameRValue(rval, symbolTable, errorLogger)

        val renameId = getNextId(ident.name)
        val varSemData: SemanticData.Var =
          SemanticData.Var(getPos(synVar), renameId, syntaxToSemantics(syntacticType))

        if (!symbolTable.set(ident.name, varSemData)) {
          errorLogger.logError(getPos(synVar), s"Scope error: Variable ${ident.name} is already declared in this scope")
        }

        Stmt.Decl(syntacticType, renameIdent(ident, symbolTable, errorLogger), renamedRVal, varSemData)

      }
      case _ => ???
    }
  }

  private def renameBlock(stmtList: List[Stmt[SyntacticData]],
                          synPos: SyntacticData,
                          symbolTable: SymbolTable,
                          errorLogger: SemanticErrorLogger): Stmt.Block[SemanticData] = {

        val childScope = new SymbolTable(Some(symbolTable))
        Stmt.Block(renameStmtList(stmtList, childScope, errorLogger), synPos.asSemanticData.get)
  }

  private def renameRValue(rvalue: RValue[SyntacticData],
                           symbolTable: SymbolTable,
                           errorLogger: SemanticErrorLogger): RValue[SemanticData] = rvalue match
    // right now, rvalue is only ever an expr but later other cases will go here
    case expr: Expr[SyntacticData] => renameExpr(expr, symbolTable, errorLogger)
  
  private def renameIdent(ident: Ident[SyntacticData],
                          symbolTable: SymbolTable,
                          errorLogger: SemanticErrorLogger): Ident[SemanticData] = {

    val semData = symbolTable.get(ident.name) match
      case Some(varSemData) => varSemData
      case None => {
        errorLogger.logError(getPos(ident.info), s"Scope error: Variable ${ident.name} is not defined")
        SemanticData.Error(getPos(ident.info))
      }

    Ident(ident.name, semData)
  }

  private def renameStmtList(exprList: List[Stmt[SyntacticData]],
                             symbolTable: SymbolTable,
                             errorLogger: SemanticErrorLogger): List[Stmt[SemanticData]] = {
    exprList.map(s => renameStmt(s, symbolTable, errorLogger))
  }

  private def renameExpr(expr: Expr[SyntacticData],
                         symbolTable: SymbolTable,
                         errorLogger: SemanticErrorLogger): Expr[SemanticData] = expr match {
    case IntLiter(value, info) =>
      val exprInfo = SemanticData.Expr(getPos(info), KnownType.Int)
      IntLiter(value, exprInfo)
    case BoolLiter(value, info) => 
      val exprInfo = SemanticData.Expr(getPos(info), KnownType.Bool)
      BoolLiter(value, exprInfo)
    case CharLiter(value, info) => 
      val exprInfo = SemanticData.Expr(getPos(info), KnownType.Char)
      CharLiter(value, exprInfo)
    case StrLiter(value, info) => 
      val exprInfo = SemanticData.Expr(getPos(info), KnownType.String)
      StrLiter(value, exprInfo)
    case PairLiter(info) => PairLiter(SemanticData.Expr(getPos(info), Null))
    case i @ Ident(name, info) => renameIdent(i, symbolTable, errorLogger)
    case _ => ???
  }
}
