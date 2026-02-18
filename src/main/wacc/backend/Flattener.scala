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
        case Stmt.Decl(_, ident, rval, _) => flattenDecl(ident, rval)
        case Stmt.Asgn(lval, rval, _) => flattenAsgn(lval, rval)
        case _ => {
            println(typedStmt)
            ???
        }
    }

    /**
      * Flattens a variable declaration into high level IR.
      * 
      * A declaration introduces a new variable into the current scope. We must
      * allocate a fresh virtual register for it and then emit a `Mov`` instruction
      * to initialize it with the evaluated right hand side.
      *
      * @param ident The AST identifier node being declared
      * @param rval The AST right hand side value being assigned
      */
    private def flattenDecl(
        ident: Ident[SemanticData], 
        rval: RValue[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): Unit = { 
        val SemanticData.Var(_, varId, typ) = ident.info: @unchecked 

        /* unlike assignments, declarations create a new variable binding, so we must call 
            defineReg to allocate a fresh `VirtualReg` and store it in the environment so 
            future lookups (eg `lvalue` or `expr` lookups) can find it */
        /* funcState's `defineReg` expects a `KnownType` but the type extracted from the
            `ident` is a `SemanticType`. But we know from type checker that it's always going
             to be a `KnownType`, so just convert it to a `KnownType`. */
        val kty: KnownType = (typ: @unchecked) match {
            case t: KnownType => t
            // case _:  /* error!!!!!! should not happen in a declaration */
        }
        
        val destReg: VirtualReg = funcState.defineReg(ident.name, varId, kty)

        val srcOperand: Operand = flattenRValue(rval)

        funcState.emitInstruction(Mov(destReg, srcOperand))
    }

    /**
      * Flattens a variable assignment into high level IR.
      * 
      * An assignment updates a location that already exists. We look up the
      * existing destination (can be register or memory) and emit a Mov to update it
      *
      * @param lval The AST left hand side location being updated
      * @param rval The AST right hand side value being assigned
      */
    private def flattenAsgn(
        lval: LValue[SemanticData],
        rval: RValue[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext) = {
        /* assignments update existing state, and flattenLValue handles looking up
            the previously allocated VirtualReg for identifiers or memory for heap objects */
        val destReg: VirtualReg = flattenLValue(lval)
        val srcOperand: Operand = flattenRValue(rval)

        funcState.emitInstruction(Mov(destReg, srcOperand))
    }

    /**
      * Flattens an LValue AST node into a VirtualReg representing its destination
      *
      * @param lval The AST left-hand side node.
      * 
      * @return The VirtualReg associated with the evaluated LValue
      */
    private def flattenLValue(lval: LValue[SemanticData])
                             (using funcState: FunctionState): VirtualReg = lval match {
        case ident: Ident[SemanticData] =>
            val SemanticData.Var(_, varId, _) = ident.info: @unchecked 
            funcState.lookupReg(ident.name, varId)
            
        case _ => 
            // we handle arrays and pairs later...
            ??? 
    }

    /**
      * Flattens an RValue AST node into an Operand
      *
      * @param rval The AST right hand side node
      * 
      * @return The evaluated Operand (VirtualReg or Immediate) containing the value
      */
    private def flattenRValue(
        rval: RValue[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): Operand = rval match {
        case expr: Expr[SemanticData] => flattenExpr(expr)
        case _ => 
            // array literals, newpairs, calls, and pairelems later on...
            ??? 
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
        case _ => ???
                                  
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