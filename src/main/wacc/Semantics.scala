package wacc

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

/** The semantic type system
  */
sealed trait SemanticType {
  override def toString: String = this match
    case KnownType.Int        => "int"
    case KnownType.Bool       => "bool"
    case KnownType.Char       => "char"
    case KnownType.String     => "string"
    case KnownType.Array(t)   => t match
      case ? => "array"
      case _ => s"$t[]"
    case KnownType.Pair(a, b) =>
      (a, b) match
        case (?, ?) => "pair"
        case _      => s"pair($a, $b)"
    case PairAny => "pair"
    case Null => "null"
    case EmptyArray => "[]"
    case ? => "unknown"
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
    */
  case Typed(pos: SourcePos, semType: SemanticType)

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
      case SemanticData.Typed(p, _)      => p
      case SemanticData.Error(p)         => p
      case SemanticData.TopLevel(_)      =>
        (0, 0) /* TopLevel has initial position */
    }

    /** Update the semantic type while preserving position and other data */
    def withType(newType: SemanticType): SemanticData = data match {
      case SemanticData.Typed(pos, _)   => SemanticData.Typed(pos, newType)
      case SemanticData.Var(pos, id, _) => SemanticData.Var(pos, id, newType)

      /* Pos, Func, Error, TopLevel don't have updatable types */
      case other => other
    }
  }
}

/** An object representing a semantic error.
  *
  * @param pos:
  *   A (row, column) tuple in the program where the AST node is located
  * @param msg:
  *   A string error message
  */
case class SemanticError(pos: SourcePos, msg: String, codeLine: String)

/** A logger class for recording semantic errors.
  */
case class ErrorLogger(val codeLines: Vector[String] = Vector.empty) {

  val errors = ListBuffer[SemanticError]()
  val indentTrackedLines = codeLines.map { line => indentAlign(line) }

  /** Strip leading and trailing whitespace from a string and provide the number
    * of leading whitespace chars (indent) stripped
    *
    * @param str:
    *   The string to be stripped
    *
    * @return
    *   (String, Int): A tuple with the stripped string and number of indent
    *   characters
    */
  private def indentAlign(str: String): (String, Int) = {
    val leftStrippedStr = str.stripLeading()
    (leftStrippedStr.stripTrailing(), str.length - leftStrippedStr.length)
  }

  /** Log a new semantic error.
    *
    * @param pos:
    *   A (row, column) tuple in the program where the AST node is located
    * @param msg:
    *   A mandatory string error message
    * @param extraInfo:
    *   Optional additional text for the error
    */
  def logError(pos: SourcePos, msg: String, extraInfo: String = "") = {
    val codeLine =
      if (indentTrackedLines.nonEmpty)
        val (line, _) = indentTrackedLines(pos(0) - 1)
        line
      else ""

    errors += SemanticError(pos, msg + extraInfo, codeLine)
  }

  /** Return a list of the the semantic errors logged.
    *
    * @return:
    *   An immutable list of `SemanticError` objects
    */
  def retrieveAllErrors(): String = {

    val errorBuilder = new StringBuilder

    /* Add each error to the error builder for a final string output */
    for (err <- errors) {

      /* Error location and main message */
      errorBuilder.append(
        s"[Line ${err.pos(0)}, column ${err.pos(1)}] ${err.msg}\n\n"
      )

      /* Code where the error occured and a pointer to the position
         in the line */
      if (indentTrackedLines.nonEmpty) {
        val (codeLine, indent) = indentTrackedLines(err.pos(0) - 1)
        val errorPointerLine = " " * (err.pos(1) - indent - 1) + "^\n"
        errorBuilder.append(s"${codeLine}\n")
        errorBuilder.append(s"${errorPointerLine}")
      }
    }

    errorBuilder.toString()
  }

  /** Check whether or not any semantic errors have been encountered.
    *
    * @return:
    *   true if any semantic errors have been found, false if not
    */
  def hasErrors(): Boolean = errors.nonEmpty
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
