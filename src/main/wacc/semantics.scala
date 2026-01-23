package wacc

enum SemanticType {

    /* Semantic types directly mapped from syntactic types */
    case Int, Bool, Char, String
    case Array(t: SemanticType)
    case Pair(fst: SemanticType, snd: SemanticType)

    /* Polymorphic semantic types  */
    case PairAny     // corresponding to `ErasedPair`
    case Null        // corresponding to a pair literal
    case EmptyArray  // corresponding to an empty array literal
}

enum SemanticData {

  case Pos(pos: SourcePos)

  case Var(pos: SourcePos, renameId: Int, semType: SemanticType)

  case Func(pos: SourcePos,
            name: String,
            returnType: SemanticType,
            params: List[SemanticType])
  
  case TopLevel(funcTable: SymbolTable[Func])
}
