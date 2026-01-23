package wacc

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

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

  // Used by the type checker to fill in types of expressions
  case Expr(pos: SourcePos, semType: SemanticType)
  
  case TopLevel(funcTable: Map[String, Func])

  case Error(pos: SourcePos)
}

case class SemanticError(pos: SourcePos, msg: String)

case class SemanticErrorLogger() {
  val errors = ListBuffer[SemanticError]()

  def logError(pos: SourcePos, msg: String) = errors += SemanticError(pos, msg)

  def retrieveAllErrors(): List[SemanticError] = errors.toList 

  def hasErrors(): Boolean = errors.nonEmpty
}

