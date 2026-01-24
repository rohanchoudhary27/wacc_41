package wacc

/**
* A source position in the program, representing (line, column).
*/
type SourcePos = (Int, Int)

/**
* The `info` annotation to AST nodes for a parsed AST output by the parser.
*/
enum SyntacticData {
  /**
  * Position information for a parsed AST node.
  *
  * @param pos: The source position tuple, representing where the AST node is 
  * located in the program.
  */
  case Pos(pos: SourcePos)
}
