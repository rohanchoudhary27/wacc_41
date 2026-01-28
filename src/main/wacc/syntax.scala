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

object SyntacticData {
  /**
  * Extension methods for working with SyntacticData positions.
  */
  extension (data: SyntacticData) {
    /** Extract the source position from syntactic data */
    def pos: SourcePos = data match {
      case SyntacticData.Pos(p) => p
    }
    
    /** Convert to SemanticData.Pos for position-only forwarding */
    def toSemanticPos: SemanticData.Pos = SemanticData.Pos(data.pos)
  }
}