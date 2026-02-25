package wacc.frontend

import scala.collection.immutable.Map

/** The semantic type system
  */
sealed trait SemanticType {
  def outputFormat: String = this match
    case KnownType.Int      => "int"
    case KnownType.Bool     => "bool"
    case KnownType.Char     => "char"
    case KnownType.String   => "string"
    case KnownType.Array(t) =>
      t match
        case ? => "array"
        case _ => s"${t.outputFormat}[]"
    case KnownType.Pair(a, b) =>
      (a, b) match
        case (?, ?) => "pair"
        case _      => s"pair(${a.outputFormat}, ${b.outputFormat})"
    case PairAny    => "pair"
    case Null       => "null"
    case EmptyArray => "[]"
    case ?          => "unknown"
}

/** Semantic types that map 1-to-1 with syntactic types
  */
enum KnownType extends SemanticType {
  case Int, Bool, Char, String
  case Array(t: SemanticType)
  case Pair(fst: SemanticType, snd: SemanticType)
}

/* Polymorphic semantic types created and used in the typechecker */
case object PairAny extends SemanticType /* corresponding to `ErasedPair` */
case object Null extends SemanticType /* corresponding to a pair literal */
case object EmptyArray
    extends SemanticType /* corresponding to an empty array literal */
case object ?
    extends SemanticType /* corresponding to typing error causing code or type inference */

/** The `info` annotation to the AST nodes for a renamed AST. Also ouptut by the
  * type checker.
  */
enum SemanticData {

  /** No additional semantic data from the renamer added (forwarding
    * Parser-outputted position).
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    */
  case Pos(pos: SourcePos)

  /** Annotating a declaration or identifier (variable use)
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    * @param renameId:
    *   The id of the renamed variable
    * @param semType:
    *   The semantic type of the variable
    */
  case Var(pos: SourcePos, renameId: Int, semType: SemanticType)

  /** A function signature used as part of the top level Program() annotation.
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    * @param name:
    *   The name of the function (not renamed as functions) cannot be overloaded
    *   and are all in the same scope in WACC
    * @param returnType:
    *   The semantic type of the return value
    * @param params:
    *   A list of the semantic types of the function arguments
    */
  case Func(
      pos: SourcePos,
      name: String,
      returnType: SemanticType,
      params: List[SemanticType]
  )

  /** Annotating an expression. Only used by the type checker after discovering
    * the type of an expression. Expressions are annotated with `Pos` initially
    * by the renamer.
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    * @param semType:
    *   The semantic type of the expression
    * @param arrayLength:
    *   Optional array length for array literals
    */
  case Typed(pos: SourcePos, semType: SemanticType, arrayLength: Option[Int] = None)

  /** Annotating the top-level Program() AST node.
    *
    * @param funcTable:
    *   A map of the functions in the program (name to signature) for the
    *   typechecker
    */
  case TopLevel(funcTable: Map[String, Func])

  /** Annotating a node that caused a semantic error in the renamer so the tree
    * can still be built to catch more semantic errors.
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    */
  case Error(pos: SourcePos)
}

object SemanticData {

  /** Extension methods for working with SemanticData positions.
    */
  extension (data: SemanticData) {

    /** Extract the source position from semantic data */
    def pos: SourcePos = data match {
      case SemanticData.Pos(p)           => p
      case SemanticData.Var(p, _, _)     => p
      case SemanticData.Func(p, _, _, _) => p
      case SemanticData.Typed(p, _, _)   => p
      case SemanticData.Error(p)         => p
      case SemanticData.TopLevel(_)      =>
        (0, 0) /* TopLevel has initial position */
    }

    /** Update the semantic type while preserving position and other data */
    def withType(newType: SemanticType): SemanticData = data match {
      case SemanticData.Typed(pos, _, arrLen)   => SemanticData.Typed(pos, newType, arrLen)
      case SemanticData.Var(pos, id, _) => SemanticData.Var(pos, id, newType)

      /* Pos, Func, Error, TopLevel don't have updatable types */
      case other => other
    }
  }
}

/** Converts a syntactic type to a semantic one. The types input here are from
  * the AST user types so are 1-to-1 mappings. Other semantic types are relevant
  * in the typechecker.
  *
  * @param synType:
  *   The syntactic type to convert
  *
  * @return:
  *   The semantic type equivalent
  */
def syntaxToSemantics(synType: SyntaxType): SemanticType = {
  synType match {
    case SyntaxType.Int            => KnownType.Int
    case SyntaxType.Bool           => KnownType.Bool
    case SyntaxType.Char           => KnownType.Char
    case SyntaxType.String         => KnownType.String
    case SyntaxType.ErasedPair     => PairAny
    case SyntaxType.Pair(fst, snd) =>
      KnownType.Pair(syntaxToSemantics(fst), syntaxToSemantics(snd))
    case SyntaxType.Array(arrType) =>
      KnownType.Array(syntaxToSemantics(arrType))
  }
}
