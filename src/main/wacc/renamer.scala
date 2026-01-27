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

    /* Top-level symbol table to track variable scope */
    val varSymbolTable = new SymbolTable(None)

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
    val newStmt = renameStmt(prog.stmt, varSymbolTable)
    
    Program(newFuncs, newStmt, SemanticData.TopLevel(funcSignatureTable.toMap))
  }

  /**
  * Converts a syntactic type to a semantic one. The types input here are from the
  * AST user types so are 1-to-1 mappings. Other semantic types are relevant in the
  * typechecker.
  *
  * @param synType: The syntactic type to convert
  *
  * @return: The semantic type equivalent
  */
  private def syntaxToSemantics(synType: SyntaxType): SemanticType = {
    synType match {
      case SyntaxType.Int => KnownType.Int
      case SyntaxType.Bool => KnownType.Bool
      case SyntaxType.Char => KnownType.Char
      case SyntaxType.String => KnownType.String
      case SyntaxType.ErasedPair => PairAny
      case SyntaxType.Pair(fst, snd) =>
        KnownType.Pair(syntaxToSemantics(fst), syntaxToSemantics(snd))
      case SyntaxType.Array(arrType) =>
        KnownType.Array(syntaxToSemantics(arrType))
    }
  }


  private def renameFunc(func: Func[SyntacticData]): Func[SemanticData] = ???

  private def renameStmt(stmt: Stmt[SyntacticData], symbolTable: SymbolTable): Stmt[SemanticData] = {
    stmt match {
      case Stmt.Skip(synPos) => Stmt.Skip(synPos.asSemanticData.get)
      case Stmt.Exit(expr, synPos) => Stmt.Exit(renameExpr(expr), synPos.asSemanticData.get)
      case Stmt.Print(expr, synPos) => Stmt.Print(renameExpr(expr), synPos.asSemanticData.get)
      case Stmt.Println(expr, synPos) => Stmt.Println(renameExpr(expr), synPos.asSemanticData.get)
      case Stmt.Block(stmtList, synPos) => Stmt.Block(renameStmtList(stmtList, symbolTable), synPos.asSemanticData.get)
      case _ => ???
    }
  }

  private def renameStmtList(exprList: List[Stmt[SyntacticData]], symbolTable: SymbolTable): List[Stmt[SemanticData]] = {
    exprList.map(s => renameStmt(s, symbolTable))
  }

  private def renameExpr(expr: Expr[SyntacticData]): Expr[SemanticData] = expr match {
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
    case UnaryExpr(op, expr, synPos) => UnaryExpr(op, renameExpr(expr), synPos.asSemanticData.get)
    case BinaryExpr(op, left, right, synPos) => BinaryExpr(op, renameExpr(left), renamExpr(right), synPos.asSemanticData.get)
    case _ => ???
  }
}
