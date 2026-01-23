package wacc

import scala.collection.mutable.Map

// temp stub - generic type output for parser AST
type SourcePos = (Int, Int)

class Renamer {

  private val variableIdMap = Map[String, Int]()
  private val funcSignatureTable = Map[String, SemanticData.Func]()

  private def getNextId(varStr: String) = {
    val id = variableIdMap.getOrElse(varStr, 0)
    variableIdMap(varStr) = id + 1
    id
  }

  def rename(prog: Program[SourcePos], errorLogger: SemanticErrorLogger): Program[SemanticData] = {

    val varSymbolTable = new SymbolTable(None)

    prog.funcs.foreach {f =>
      
      val retType = syntaxToSemantics(f.typ)
      val paramTypes = f.params.map { p => syntaxToSemantics(p.typ) }

      val funcName = f.ident.ident
        
      val funcSignature: SemanticData.Func = SemanticData.Func(
          pos = f.info,
          name = funcName,
          returnType = retType,
          params = paramTypes
      )

      funcSignatureTable += (funcName -> funcSignature)
    }

    val newFuncs = prog.funcs.map(f => renameFunc(f))
    val newStmt = renameStmt(prog.stmt, varSymbolTable)
    
    Program(newFuncs, newStmt, SemanticData.TopLevel(funcSignatureTable.toMap))
  }

  private def syntaxToSemantics(synType: SyntaxType): SemanticType = {
    synType match {
      case SyntaxType.Int => SemanticType.Int
      case SyntaxType.Bool => SemanticType.Bool
      case SyntaxType.Char => SemanticType.Char
      case SyntaxType.String => SemanticType.String
      case SyntaxType.ErasedPair => SemanticType.PairAny
      case SyntaxType.Pair(fst, snd) =>
        SemanticType.Pair(syntaxToSemantics(fst), syntaxToSemantics(snd))
      case SyntaxType.Array(arrType) =>
        SemanticType.Array(syntaxToSemantics(arrType))
    }
  }

  private def renameFunc(func: Func[SourcePos]): Func[SemanticData] = ???
  private def renameStmt(stmt: Stmt[SourcePos],
                 symbolTable: SymbolTable): Stmt[SemanticData] = ???
}
