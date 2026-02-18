package wacc.backend

import wacc.frontend.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import wacc.ExitCodes.*

/**
  * A state class managing the virtual register allocation and instructions that make up
  * a function. The main body of a WACC program is treated as a 'main' function.
  */
class FunctionState(
) {

    /* A renamed variable from the AST. While the AST has this information already in
       SemanticData.Var, it's clearer to repackage it like this without the extra info */
    case class QualifiedName(varName: String, varId: Int)

    /* Map of variables to their virtual registers */
    private val virtualRegMap = Map[QualifiedName, VirtualReg]()
    private var nextVirtualId = 0

    /* The list of instructions for the function */
    private val instructions = ListBuffer[Instruction]()

    /**
      * Look up the virtual register of a given variable. A precondition is that the variable
      * must already have an allocated VirtualReg.
      *
      * @param varName the string name of the variable
      * @param renameId the renamed ID from the frontend of the variable
      * @return the VirtualReg assigned to the variable from a prevoius `defineReg` call
      */
    def lookupReg(varName: String, renameId: Int): VirtualReg = {
        virtualRegMap.get(QualifiedName(varName, renameId)) match
            case None => {
                Console.err.println("Internal compiler failure: attempted to lookup a virtual" +
                                    "register before defining one")
                sys.exit(code(CompilerFailure))
            }
            case Some(virtualReg) => virtualReg
    }

    /**
      * Define a new virtual register mapping.
      *
      * @param varName the string name of the variable
      * @param renameId the renamed ID from the frontend of the variable
      * @param typ the `KnownType` of the variable
      * @return a new virtual register
      */
    def defineReg(varName: String, renameId: Int, typ: KnownType): VirtualReg = {
        val nextReg = VirtualReg(nextVirtualId, Size.fromType(typ))
        virtualRegMap +=
            QualifiedName(varName, renameId) -> nextReg
        nextVirtualId += 1
        nextReg
    }

    /**
      * Allocate a register as part of computing an expression (not to a variable)
      *
      * @param typ
      * @return
      */
    def anonymousReg(typ: KnownType): VirtualReg = {
        val nextReg = VirtualReg(nextVirtualId, Size.fromType(typ))
        nextVirtualId += 1
        nextReg
    }

    /**
      * Emits an instruction to the instruction buffer
      *
      * @param instr
      */
    def emitInstruction(instr: Instruction): Unit = instructions += instr

    /**
      * Returns an immutable list of the function's instructions
      *
      * @return
      */
    def getAllInstructions = instructions.toList
}

/**
  * State class for managing directives.
  */
class GlobalFlattenContext(
) {
    private val directives = ListBuffer[DataDirective]()

    /* Next available label ID for e.g. "${prefix}_0" */
    private var nextLabelId = 0

    /**
      * Return a `Label` with the next label ID. Essentially reserves a label because labels for
      * branching may not be defined when they need to be used (e.g. branching to an 'else' clause
      * in a conditional when failing the 'if' condition). Then later, this label must be emitted
      * as part of a `LabelDef` insruction to the list of instructions.
      *
      * @param prefix the label prefix string
      * @return a `Label` object
      */
    def getNextLabel(prefix: String): Label = {
        val label = Label(s"${prefix}_${nextLabelId}")
        nextLabelId += 1
        label
    }

    /* Mapping strings to their memory address locations (labels) */
    private val strMap = Map[String, Label]()
    private var strCount = 0

    /**
      * Get an immutable list of all the directives.
      *
      * @return an immutable list of `DataDirective` objects
      */
    def getAllDirectives = directives.toList

    /**
      * Add a string to the data directives if not already added.
      *
      * @param str the string to add to the data directives
      * @return the label of the string (either from a map if already added or a
      * newly created label)
      */
    def addString(str: String): Label = {
        strMap.get(str) match
            case None => {
                val strLabel = Label(s"string_${strCount}")
                directives += DataDirective(strLabel, str)
                strMap += str -> strLabel
                strCount += 1
                strLabel
            }
            case Some(label) => label
    }
}

/**
  * The backend flattener which inputs a typed AST from the frontend and outputs the high
  * level 3-register IR which is to then be input to the register allocator and code generator 
  */
object Flattener {

    /**
      * The main function of the Flattener. Inputs a typed AST (Program[SemanticData]) from the
      * frontend typechecker and outputs a `RegProgram` which is a flattened version of the AST
      * into a high level 3-register assembley IR.
      *
      * @param typedAST the type checker AST output from the frontend
      * @return a `RegProgram` 3-register assembley IR
      */
    def flatten(typedAST: Program[SemanticData]): RegProgram = {
        val Program(funcs, stmt, _) = typedAST

        /* Initialise contexts */
        given funcState: FunctionState = FunctionState()
        given directiveState: GlobalFlattenContext = GlobalFlattenContext()

        /* Flatten the main statement which populates the funcState
           instructions buffer */
        flattenStmt(stmt)

        RegProgram(funcs.map(flattenFunc),
                   directiveState.getAllDirectives,
                   funcState.getAllInstructions)
    }

    /**
      * Converts an operand to a virtual register by making an anonymous register
      * if the operand is an immeidate
      *
      * @param op The operand to convert to a register
      * @param funcState The FunctionState context
      * @return The virtual register for the operand
      */
    private def toReg(op: Operand)(using funcState: FunctionState): VirtualReg = op match {
        case reg: VirtualReg => reg
        case imm: Immediate => 
            val newReg = funcState.anonymousReg(KnownType.Int)
            funcState.emitInstruction(Mov(newReg, imm))
            newReg
        case label: Label => ???
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
    private def flattenStmt(typedStmt: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext): Unit = typedStmt match {

        case Stmt.Block(stmts, _) => stmts.foreach(flattenStmt)
        case Stmt.Exit(expr, _) => flattenExit(expr)
        case Stmt.Print(expr, _) => flattenPrint(expr)
        case Stmt.Println(expr, _) => flattenPrintln(expr)
        case _ => {
            println(typedStmt)
            ???
        }
    }
        
    /**
      * Flatten an Exit AST node, emitting a Call node to the exit clib function
      *
      * @param expr The expr that evaluates to the exit code
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      */
    private def flattenExit(expr: Expr[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext) = {
        val exitCode = flattenExpr(expr)
        val callInstr = wacc.backend.Call(Label("exit"), List(exitCode), None)
        funcState.emitInstruction(callInstr)
    }

    /**
      * Flatten a Print AST node, emitting a Call node to the right print widget
      *
      * @param expr The expression to be printed
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      */
    private def flattenPrint(expr: Expr[SemanticData], newline: Boolean = false)
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext) = {

        val printVal = flattenExpr(expr)

        val SemanticData.Typed(_, semType) = expr.info

        val printWidget: Widget = semType match
            case wacc.frontend.KnownType.Int =>  PrintIntWidget
            case KnownType.Bool => PrintBoolWidget
            case wacc.frontend.KnownType.Char => PrintCharWidget
            case wacc.frontend.KnownType.String => PrintStringWidget
        

        funcState.emitInstruction(wacc.backend.Print(printVal, printWidget, newline))
    }

    /**
      * Flatten a Println AST node, emitting a Call node to the right print widget
      * with a new line
      *
      * @param expr The expression to be printed
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      */
    private def flattenPrintln(expr: Expr[SemanticData])
                              (using funcState: FunctionState,
                                     directiveState: GlobalFlattenContext) = {
        flattenPrint(expr, newline = true)
    }

    /**
      * Flatten an Expr AST node to an Operand
      *
      * @param typedExpr The expr to flatten
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      * @return The flattened Operand
      */
    private def flattenExpr(typedExpr: Expr[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext): Operand = typedExpr match
        case IntLiter(value, _) => Immediate(value)
        case BoolLiter(value, _) => Immediate(if value then 1 else 0)
        case CharLiter(value, _) => Immediate(value)
        case StrLiter(value, _) => directiveState.addString(value)
        case BinaryExpr(op, left, right, _) => flattenBinaryExpr(op, left, right)
        case UnaryExpr(op, expr, _) => flattenUnaryExpr(op, expr)
        case _ => ???
    
    /**
      * Flatten a binary expression
      *
      * @param op The binary operator
      * @param left The left operand expression
      * @param right The right operand expression
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      * @return The virtual register containing the result
      */
    private def flattenBinaryExpr(op: BinaryOperator,
                                  left: Expr[SemanticData],
                                  right: Expr[SemanticData])
                                  (using funcState: FunctionState,
                                         directiveState: GlobalFlattenContext): VirtualReg = {
        val leftOp = flattenExpr(left)
        val rightOp = flattenExpr(right)

        op match {
            case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT | 
                 BinaryOperator.LTE | BinaryOperator.EQ | BinaryOperator.NEQ =>
                flattenComparison(op, leftOp, rightOp)
            case _ => ???
        }
    }

    /**
      * Flatten a comparison operation
      *
      * @param op The comparison operator
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param funcState The current scope function state
      * @return The virtual register containing the comparison result (0 or 1)
      */
    private def flattenComparison(op: BinaryOperator,
                                  leftOp: Operand,
                                  rightOp: Operand)
                                  (using funcState: FunctionState): VirtualReg = {
        val leftReg = toReg(leftOp)
        val rightReg = toReg(rightOp)

        /* Emit the compare instruction */
        funcState.emitInstruction(Cmp(leftReg, rightReg))

        /* Map frontend operator to backend condition */
        val condition = op match {
            case BinaryOperator.GT  => Condition.GT
            case BinaryOperator.GTE => Condition.GE
            case BinaryOperator.LT  => Condition.LT
            case BinaryOperator.LTE => Condition.LE
            case BinaryOperator.EQ  => Condition.EQ
            case BinaryOperator.NEQ => Condition.NE
        }

        /* Create result register and emit SetCond instruction */
        val resultReg = funcState.anonymousReg(KnownType.Bool)
        funcState.emitInstruction(SetCond(resultReg, condition))

        resultReg
    }

    /**
      * Flatten a unary expression
      *
      * @param op The unary operator
      * @param expr The operand expression
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      * @return The virtual register containing the result
      */
    private def flattenUnaryExpr(op: UnaryOperator,
                                 expr: Expr[SemanticData])
                                 (using funcState: FunctionState,
                                        directiveState: GlobalFlattenContext): VirtualReg = {
        val operand = flattenExpr(expr)

        op match {
            case UnaryOperator.Not => flattenNot(operand)
            case _ => ???
        }
    }

    /**
      * Flatten a boolean NOT operation
      *
      * @param operand The operand to negate
      * @param funcState The current scope function state
      * @return The virtual register containing the negated result
      */
    private def flattenNot(operand: Operand)
                          (using funcState: FunctionState): VirtualReg = {
        val srcReg = toReg(operand)
        val resultReg = funcState.anonymousReg(KnownType.Bool)

        funcState.emitInstruction(Not(resultReg, srcReg))
        
        resultReg
    }

    /**
      * Helper function to flatten binary boolean operations
      *
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param makeInstr Function to create the instruction given dest, src1, src2
      * @param funcState The current scope function state
      * @return The virtual register containing the result
      */
    private def flattenBinaryBoolOp(leftOp: Operand,
                                     rightOp: Operand,
                                     makeInstr: (VirtualReg, VirtualReg, VirtualReg) => Instruction)
                                    (using funcState: FunctionState): VirtualReg = {
        val leftReg = toReg(leftOp)
        val rightReg = toReg(rightOp)
        val resultReg = funcState.anonymousReg(KnownType.Bool)

        funcState.emitInstruction(makeInstr(resultReg, leftReg, rightReg))
        
        resultReg
    }

    /**
      * Flatten a boolean AND operation
      *
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param funcState The current scope function state
      * @return The virtual register containing the AND result
      */
    private def flattenAnd(leftOp: Operand, rightOp: Operand)
                          (using funcState: FunctionState): VirtualReg =
        flattenBinaryBoolOp(leftOp, rightOp, And.apply)

    /**
      * Flatten a boolean OR operation
      *
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param funcState The current scope function state
      * @return The virtual register containing the OR result
      */
    private def flattenOr(leftOp: Operand, rightOp: Operand)
                         (using funcState: FunctionState): VirtualReg =
        flattenBinaryBoolOp(leftOp, rightOp, Or.apply)
                                  
    /**
      * Flattens a function AST node. Internally, it creates its own `ListBuffer[Instruction]`
      * which it then passes to any other flatten function calls.
      *
      * @param typedFunc the function node to flatten
      * @return a `FunctionDef` high level IR object
      */
    private def flattenFunc(typedFunc: Func[SemanticData])
                           (using directiveState: GlobalFlattenContext): FunctionDef = {

        /* Create a fresh function state */
        given funcContext: FunctionState = FunctionState()

        ???
    }

}