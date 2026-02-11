package wacc.backend

import wacc.frontend.*
import scala.collection.mutable.ListBuffer

/** State used throughout the flattening process. 
  */
class FlattenerState(
) {
    private val directives = ListBuffer[DataDirective]()

    def getAllDirectives = directives.toList
}

/**
  * The backend flattener which inputs a typed AST from the frontend and outputs the high
  * level 3-register IR which is to then be input to the register allocator and code generator 
  */
object Flattener {

    def flatten(typedAST: Program[SemanticData]): RegProgram = {
        val Program(funcs, stmt, _) = typedAST

        given state: FlattenerState = FlattenerState()

        RegProgram(funcs.map(flattenFunc),
                   state.getAllDirectives,
                   flattenStmt(stmt, ListBuffer[Instruction]()))
    }

    /**
      * Flattens a statment into a list of high level IR instructions. Uses the buffer passed
      * in to record new `Instruction` objects and passes this buffer to other flatten function
      * calls. At the end, it returns instrBuffer.toList
      *
      * @param typedStmt the statement AST node to flatten
      * @param instrBuffer a buffer to emit `Instruction` objects to
      * @return the list of high level IR instructions
      */
    private def flattenStmt(typedStmt: Stmt[SemanticData],
                            instrBuffer: ListBuffer[Instruction]): List[Instruction] = ???

    /**
      * Flattens a function AST node. Internally, it creates its own `ListBuffer[Instruction]`
      * which it then passes to any other flatten function calls.
      *
      * @param typedFunc the function node to flatten
      * @return a `FunctionDef` high level IR object
      */
    private def flattenFunc(typedFunc: Func[SemanticData]): FunctionDef = ???

}