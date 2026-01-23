package wacc

// temp stub - generic type output for parser AST
type SourcePos = (Int, Int)

object Renamer {

  private val funcSymbolTable: SymbolTable[SemanticData.Func] = new SymbolTable(None)

  def rename(prog: Program[SourcePos]): Program[SemanticData] = {

    val varSymbolTable: SymbolTable[SemanticData.Var] = new SymbolTable(None)

    prog.funcs.foreach {f =>
      
      val retType = syntaxToSemantics(f.typ)
      val paramTypes = f.params.map { p => syntaxToSemantics(p.typ) }
        
      SemanticData.Func(
          pos = f.info,
          name = f.ident.ident,
          returnType = retType,
          params = paramTypes
      )

    }

    val newFuncs = prog.funcs.map(f => renameFunc(f))
    val newStmt = renameStmt(prog.stmt, varSymbolTable)
    
    Program(newFuncs, newStmt, SemanticData.TopLevel(funcSymbolTable))
  }

  def syntaxToSemantics(synType: SyntaxType): SemanticType = {
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
  
  def renameFunc(func: Func[SourcePos]): Func[SemanticData] = ???
  def renameStmt(stmt: Stmt[SourcePos],
                 symbolTable: SymbolTable[SemanticData.Var]): Stmt[SemanticData] = ???
}
