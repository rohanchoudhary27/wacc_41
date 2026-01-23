package wacc

// temp stub - generic type output for parser AST
type SourcePos = (Int, Int)

enum SemanticData {

  case Pos(pos: SourcePos)

  case Var(pos: SourcePos, renameId: Int, semType: SemanticType)

  case Func(pos: SourcePos,
            name: String,
            returnType: SemanticType,
            params: List[SemanticType])
}

