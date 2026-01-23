package wacc

import scala.collection.immutable.Map
import scala.collection.mutable.ListBuffer

/**
* The semantic type system 
*/
enum SemanticType {

    /* Semantic types directly mapped from syntactic types */
    case Int, Bool, Char, String
    case Array(t: SemanticType)
    case Pair(fst: SemanticType, snd: SemanticType)

    /* Polymorphic semantic types created and used in the typechecker */
    case PairAny     // corresponding to `ErasedPair`
    case Null        // corresponding to a pair literal
    case EmptyArray  // corresponding to an empty array literal
}


/**
* The `info` annotation to the AST nodes for a renamed AST. Also ouptut
* by the type checker.
*/
enum SemanticData {

  /**
  * No additional semantic data from the renamer added (forwarding
  * Parser-outputted position).
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  */
  case Pos(pos: SourcePos)

  /**
  * Annotating a declaration or identifier (variable use)
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  * @param renameId: The id of the renamed variable
  * @param semType: The semantic type of the variable
  */
  case Var(pos: SourcePos, renameId: Int, semType: SemanticType)

  /**
  * A function signature used as part of the top level Program()
  * annotation.
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  * @param name: The name of the function (not renamed as functions)
  * cannot be overloaded and are all in the same scope in WACC
  * @param returnType: The semantic type of the return value
  * @param params: A list of the semantic types of the function arguments
  */
  case Func(pos: SourcePos,
            name: String,
            returnType: SemanticType,
            params: List[SemanticType])

  /**
  * Annotating an expression. Only used by the type checker after discovering
  * the type of an expression. Expressions are annotated with `Pos` initially
  * by the renamer.
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  * @param semType: The semantic type of the expression
  */
  case Expr(pos: SourcePos, semType: SemanticType)
  
  /**
  * Annotating the top-level Program() AST node.
  *
  * @param funcTable: A map of the functions in the program (name to signature)
  * for the typechecker
  */
  case TopLevel(funcTable: Map[String, Func])

  /**
  * Annotating a node that caused a semantic error so the tree can still
  * be built to catch more semantic errors.
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  */
  case Error(pos: SourcePos)
}

/**
* An object representing a semantic error.
*
* @param pos: A (row, column) tuple in the program where the AST
* node is located
* @param msg: A string error message
*/
case class SemanticError(pos: SourcePos, msg: String)

/**
* A logger class for recording semantic errors.
*/
case class SemanticErrorLogger() {

  val errors = ListBuffer[SemanticError]()

  /**
  * Log a new semantic error.
  *
  * @param pos: A (row, column) tuple in the program where the AST
  * node is located
  * @param msg: A string error message
  */
  def logError(pos: SourcePos, msg: String) = errors += SemanticError(pos, msg)

  /**
  * Return a list of the the semantic errors logged.
  *
  * @return: An immutable list of `SemanticError` objects
  */
  def retrieveAllErrors(): List[SemanticError] = errors.toList 

  /**
  * Check whether or not any semantic errors have been encountered.
  *
  * @return: true if any semantic errors have been found, false if not
  */
  def hasErrors(): Boolean = errors.nonEmpty
}

