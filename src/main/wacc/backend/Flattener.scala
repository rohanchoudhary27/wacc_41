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
      * Define a new virtual register mapping with given size.
      *
      * @param varName the string name of the variable
      * @param renameId the renamed ID from the frontend of the variable
      * @param size the `Size` of the variable
      * @return a new virtual register
      */
    def defineReg(varName: String, renameId: Int, size: Size): VirtualReg = {
        val nextReg = VirtualReg(nextVirtualId, size)
        virtualRegMap +=
            QualifiedName(varName, renameId) -> nextReg
        nextVirtualId += 1
        nextReg
    }

    /**
      * Allocate a register as part of computing an expression (not to a variable) with
      * the given type.
      *
      * @param typ the `KnownType` of the variable
      * @return
      */
    def anonymousReg(typ: KnownType): VirtualReg = {
        val nextReg = VirtualReg(nextVirtualId, Size.fromType(typ))
        nextVirtualId += 1
        nextReg
    }

    /**
      * Allocate a register as part of computing an expression (not to a variable) with
      * the given size.
      *
      * @param size the `Size` of the variable
      * @return
      */
    def anonymousReg(size: Size): VirtualReg = {
        val nextReg = VirtualReg(nextVirtualId, size)
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

                /* Escape chars must be written with backslashes in the assembly */
                val escapedStr = str
                    .replace("\"", "\\\"")
                    .replace("\'", "\\\'")
                    .replace("\n", "\\n")

                directives += DataDirective(strLabel, escapedStr)
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

    private val PAIR_HEAP_SIZE_B = 16

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
    private def toReg(op: Operand)(using funcState: FunctionState): VirtualReg = 
        toReg(op, KnownType.Int)
    
    private def toReg(op: Operand, targetType: KnownType)
                     (using funcState: FunctionState): VirtualReg = op match {
        case reg: VirtualReg => reg
        case imm: Immediate => 
            val newReg = funcState.anonymousReg(targetType)
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
        case Stmt.Skip(_) => () /* do nothing on skip */
        case Stmt.Exit(expr, _) => flattenExit(expr)
        case Stmt.Read(lval, _) => flattenRead(lval)
        case Stmt.Print(expr, _) => flattenPrint(expr)
        case Stmt.Println(expr, _) => flattenPrintln(expr)
        case Stmt.Return(expr, _) => flattenReturn(expr)
        case Stmt.Decl(_, ident, rval, _) => flattenDecl(ident, rval)
        case Stmt.Asgn(lval, rval, _) => flattenAsgn(lval, rval)
        case Stmt.If(cond, ifBlock, elseBlock, _) => flattenIf(cond, ifBlock, elseBlock)
        case Stmt.While(cond, body, _) => flattenWhile(cond, body)
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

        /* what if destReg's size and srcOperand's size are different? */

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
        val srcOperand: Operand = flattenRValue(rval)
        
        /* We need to generate different high level IR based on the lvalue. If it's a pair access
            we have to use PairStore instead of a simple Mov, so we pattern match lval */
        lval match {
            case _: Ident[SemanticData] =>
                val destReg: VirtualReg = flattenLValue(lval)
                funcState.emitInstruction(Mov(destReg, srcOperand))
            case p: PairElem[SemanticData] => 
                val (pairPtr, pos) = resolvePairElem(p) /* get the base pointer and index */
                /* null check when accessing the pair p */
                funcState.emitInstruction(NullCheck(pairPtr))
                funcState.emitInstruction(PairStore(pairPtr, pos, srcOperand))
            case _ => ???
        }
    }

    /**
      * Flattens an LValue AST node into a VirtualReg representing its destination
      *
      * @param lval The AST left-hand side node.
      * 
      * @return The VirtualReg associated with the evaluated LValue
      */
    private def flattenLValue(
        lval: LValue[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): VirtualReg = lval match {
        case ident: Ident[SemanticData] =>
            val SemanticData.Var(_, varId, _) = ident.info: @unchecked 
            funcState.lookupReg(ident.name, varId)
        
        case p: PairElem[SemanticData] =>
            /* To write to an inner pair (lvalue means that), we must first find out where 
                it lives in memory. If we have something like `fst fst p`, to 
                get the address of this whole expression, we need to get the address 
                of the inner `fst p`, and so on.  */
            flattenPairElem(p)

        case _ => 
            // we handle arrays later... (good luck saim)
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
        case wacc.frontend.Call(ident, argList, info) => flattenCall(ident, argList)
        case NewPair(e1, e2, _) => 
            /* flatten both expressions, they'll be put inside PairStore */
            val src1 = flattenExpr(e1)
            val src2 = flattenExpr(e2)
            /* Pairs are always allocated in 16 bytes on the heap, so get a new 
                anonymous register with size `Size.X` and heap allocate with 16 bytes. The
                freshly allocated anonymous register will be the base pointer of the pair, i.e.,
                the pointer that corresponds to the first element. this is simialr to C arrays
                where int *p = {1, 2, 3} the pointer p refers to the first element. */
            val pairPtr = funcState.anonymousReg(Size.X) 
            funcState.emitInstruction(HeapAlloc(pairPtr, Immediate(PAIR_HEAP_SIZE_B)))

            funcState.emitInstruction(PairStore(pairPtr, PairPos.Fst, src1))
            funcState.emitInstruction(PairStore(pairPtr, PairPos.Snd, src2))
            
            pairPtr
        case p: PairElem[SemanticData] => flattenPairElem(p)
        case _ => ??? // array liters and calls
    }

    /**
     * Flattens a `PairElem` AST node by emitting instructions to resolve its base pointer,
     * perform a runtime null check, and load its contents from the heap.
     * 
     * @param p The `PairElem` AST node to flatten
     * @return A `VirtualReg` containing the loaded value or resolved memory address.
     */
    private def flattenPairElem(
        p: PairElem[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): VirtualReg = {
        val PairElem(_, _, SemanticData.Typed(_, t)) = p: @unchecked
        val (pairPtr, pos) = resolvePairElem(p)

        /* emit the null check when referencing a pair */
        funcState.emitInstruction(NullCheck(pairPtr))

        /* Extract the element size from its type and allocate an anonymous register
            based on that type. */
        val pairElemSize = Size.fromSemType(t)

        val destReg = funcState.anonymousReg(pairElemSize)

        /* Load the `pos`-index of the pair from pairPtr into destReg */
        funcState.emitInstruction(PairLoad(pairPtr, pos, destReg))

        destReg
    }
    

    /**
     * Resolve the virtual register that contains the pair and the position.
     * 
     * @param pairElem The `PairElem` to resolve
     * @return `(VirtualReg, PairPos)` the virtual base register of the pair and fst/snd
     */ 
    private def resolvePairElem(
        pairElem: PairElem[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): (VirtualReg, PairPos) = {
        val PairElem(isFst, lval, _) = pairElem
         /* flatten the lvalue to get the pair register */
        val pairPtr = flattenLValue(lval)

        val pos = if isFst then PairPos.Fst else PairPos.Snd
        (pairPtr, pos)
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
      * Flatten a Read AST node, emitting a Read instruction with the appropriate widget
      *
      * @param lval The left-value to read into
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      */
    private def flattenRead(lval: LValue[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext): Unit = {
        val destReg = flattenLValue(lval)
        
        /* Extract type from lvalue */
        val readType = lval match {
            case ident: Ident[SemanticData] =>
                val SemanticData.Var(_, _, semType) = ident.info: @unchecked
                semType
            case _ => ??? // arrays and pairs to be done later
        }
        
        /* Convert SemanticType to KnownType */
        val knownType = readType match {
            case kt: KnownType => kt
            case _ => ??? // shouldn't happen after type checking
        }
        
        /* Select widget based on type - read only supports int and char */
        val readWidget = knownType match {
            case KnownType.Int => ReadIntegerWidget
            case KnownType.Char => ReadCharWidget
            case _ => ???
        }
        
        funcState.emitInstruction(Read(destReg, readWidget))
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
        val semType = expr.expressionType

        val printWidget: Widget = semType match
            case wacc.frontend.KnownType.Int =>  PrintIntWidget
            case KnownType.Bool => PrintBoolWidget
            case wacc.frontend.KnownType.Char => PrintCharWidget
            case wacc.frontend.KnownType.String => PrintStringWidget
            case wacc.frontend.KnownType.Pair(_, _) => PrintPointerWidget
            case wacc.frontend.KnownType.Array(_) => PrintPointerWidget
            case wacc.frontend.Null => PrintPointerWidget
        

        funcState.emitInstruction(wacc.backend.Print(printVal, printWidget, newline))
    }

    private def flattenIf(cond: Expr[SemanticData],
                          ifBlock: Stmt[SemanticData],
                          elseBlock: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext) = {

        val flatCondition = flattenExpr(cond)

        /* Define labels */
        val elseLabel = directiveState.getNextLabel(".L_else")
        val endLabel = directiveState.getNextLabel(".L_end")

        /* If the condition is false, jump to the else case*/
        funcState.emitInstruction(Cmp(toReg(flatCondition), Immediate(0)))
        funcState.emitInstruction(BranchCond(Condition.EQ, elseLabel))

        /* Fall through to the true case and then jump to the end */
        flattenStmt(ifBlock)
        funcState.emitInstruction(BranchUncond(endLabel))

        /* Define the else section */
        funcState.emitInstruction(LabelDef(elseLabel))
        flattenStmt(elseBlock)

        /* End of if statement label */
        funcState.emitInstruction(LabelDef(endLabel))
    }

    private def flattenWhile(cond: Expr[SemanticData],
                             body: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext) = {
        
        /* Define labels */
        val bodyLabel = directiveState.getNextLabel(".L_body")
        val condLabel = directiveState.getNextLabel(".L_cond")

        /* Jump to the condition */
        funcState.emitInstruction(BranchUncond(condLabel))

        /* Loop body */
        funcState.emitInstruction(LabelDef(bodyLabel))
        flattenStmt(body)

        /* Evaluate the condition and jump back if it's true */
        funcState.emitInstruction(LabelDef(condLabel))
        val flatCondition = flattenExpr(cond)
        funcState.emitInstruction(Cmp(toReg(flatCondition), Immediate(1)))
        funcState.emitInstruction(BranchCond(Condition.EQ, bodyLabel))
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
        case PairLiter(_) => Immediate(0) /* reference compiler uses 0 to represent `null` */
        case Ident(name, SemanticData.Var(_, renameId, _)) => funcState.lookupReg(name, renameId)
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

        val exprType = left.expressionType match {
            case t: KnownType => t
            case _ => KnownType.Int
        }

        op match {
            case BinaryOperator.GT | BinaryOperator.GTE | BinaryOperator.LT | 
                 BinaryOperator.LTE | BinaryOperator.EQ | BinaryOperator.NEQ =>
                flattenComparison(op, leftOp, rightOp, exprType)
            case BinaryOperator.And => 
                flattenBinaryOp(leftOp, rightOp, And.apply, KnownType.Bool)
            case BinaryOperator.Or => 
                flattenBinaryOp(leftOp, rightOp, Or.apply, KnownType.Bool)
            case BinaryOperator.Add => flattenBinaryOp(leftOp, rightOp, Add.apply, KnownType.Int)
            case BinaryOperator.Sub => flattenBinaryOp(leftOp, rightOp, Sub.apply, KnownType.Int)
            case BinaryOperator.Mul => flattenBinaryOp(leftOp, rightOp, Mul.apply, KnownType.Int)
            case BinaryOperator.Div => flattenBinaryOp(leftOp, rightOp, Div.apply, KnownType.Int)
            case BinaryOperator.Mod => flattenBinaryOp(leftOp, rightOp, Mod.apply, KnownType.Int)
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
                                  rightOp: Operand,
                                  compType: KnownType)
                                  (using funcState: FunctionState): VirtualReg = {
        val leftReg = toReg(leftOp, compType)
        val rightReg = toReg(rightOp, compType)

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
            case UnaryOperator.Chr => flattenChr(operand)
            case UnaryOperator.Ord => flattenOrd(operand)
            case UnaryOperator.Neg => flattenNeg(operand)
            case UnaryOperator.Len => ??? /* TODO: requires arrays */
        }
    }

    /**
      * Flatten a `chr` unary operator. Emits a `Chr` high level IR node
      * because the code generator needs have logic for the runtime error check (ascii
      * chars can only be 0-127 inclusive).
      *
      * @param operand The operand to the chr function
      * @param funcState The current scope function state
      * @return The virtual register containing the result
      */
    private def flattenChr(operand: Operand)
                          (using funcState: FunctionState): VirtualReg = {

        /* Chr input is an int so we use KnownType.Int */
        val srcReg = toReg(operand, KnownType.Int)

        /* Emit Chr high level IR instruction with matching reg sizes (both W) */
        val chrIntReg = funcState.anonymousReg(KnownType.Int)
        funcState.emitInstruction(wacc.backend.Chr(chrIntReg, srcReg, BadCharWidgetError))

        /* Downcast Char reg to Bool reg */
        val chrCharReg = funcState.anonymousReg(KnownType.Char)
        funcState.emitInstruction(SignExtend(chrCharReg, chrIntReg))

        chrCharReg
    }

    /**
      * Flatten an `ord` instruction.
      *
      * @param operand The operand passed to the Ord call
      * @param funcState The current scope function state
      * @return The virtual register containing the ord result
      */
    private def flattenOrd(operand: Operand)
                        (using funcState: FunctionState): VirtualReg = {

        /* Get the source char register (since `ord` takes chars) */
        val srcReg = toReg(operand, KnownType.Char)
        
        /* Allocate a new W register for the result */
        val destReg = funcState.anonymousReg(KnownType.Int)
        
        /* Sign extend to the larger register (B to W).
           Can't do a MOV here because moves have to be between
           registers of the same size - this is the point of sign extension */
        funcState.emitInstruction(SignExtend(destReg, srcReg))
        
        destReg
    }

    /**
      * Flatten a negation statement
      *
      * @param operand The operand passed to the negation statement
      * @param funcState The current scope function state
      * @return The virtual register with the negation result
      */
    private def flattenNeg(operand: Operand)
                          (using funcState: FunctionState): VirtualReg = {
        val reg = toReg(operand)
        val negReg = funcState.anonymousReg(KnownType.Int)
        funcState.emitInstruction(Neg(negReg, reg))
        negReg
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
        val srcReg = toReg(operand, KnownType.Bool)
        val resultReg = funcState.anonymousReg(KnownType.Bool)

        funcState.emitInstruction(Not(resultReg, srcReg))
        
        resultReg
    }

    /**
      * Helper function to flatten binary operations
      *
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param makeInstr Function to create the instruction given dest, src1, src2
      * @param operandType The type of the operands and result (default: Int)
      * @param funcState The current scope function state
      * @return The virtual register containing the result
      */
    private def flattenBinaryOp(leftOp: Operand,
                                rightOp: Operand,
                                makeInstr: (VirtualReg, VirtualReg, VirtualReg) => Instruction,
                                operandType: KnownType = KnownType.Int)
                               (using funcState: FunctionState): VirtualReg = {
        val leftReg = toReg(leftOp, operandType)
        val rightReg = toReg(rightOp, operandType)
        val resultReg = funcState.anonymousReg(operandType)

        funcState.emitInstruction(makeInstr(resultReg, leftReg, rightReg))
        
        resultReg
    }
                                  
    /**
      * Flattens a function AST node. Internally, it creates its own `FunctionState`
      * which it then passes to any other flatten function calls.
      *
      * @param typedFunc the function node to flatten
      * @return a `FunctionDef` high level IR object
      */
    private def flattenFunc(typedFunc: Func[SemanticData])
                           (using directiveState: GlobalFlattenContext): FunctionDef = {

        /* Create a fresh function state */
        given funcState: FunctionState = FunctionState()

        val Func(typ, ident, params, stmt, _) = typedFunc

        /* Function label */
        val funcLabel = Label(s"f_${ident.name}")

        val flatParams = params.map(flattenParam)
        flattenStmt(stmt)

        FunctionDef(funcLabel.name, flatParams, funcState.getAllInstructions)
    }

    /**
      * Flatten a function parameter
      *
      * @param param The function param to flatten
      * @param funcState The current scope function state
      * @return The virtual register containing the flattened param
      */
    private def flattenParam(param: Param[SemanticData])
                            (using funcState: FunctionState): VirtualReg = {

        
        val SemanticData.Var(_, varId, typ) = param.ident.info: @unchecked 

        /* Convert SemanticType to KnownType*/
        val kty: KnownType = (typ: @unchecked) match {
            case t: KnownType => t
        }

        funcState.defineReg(param.ident.name, varId, kty)
    }

    /**
      * Flatten a return statement
      *
      * @param expr The return statement to be flattened
      * @param funcState The current scope function state
      * @param globalContext The flattener global context
      */
    private def flattenReturn(expr: Expr[SemanticData])
                            (using funcState: FunctionState,
                                   globalContext: GlobalFlattenContext): Unit = {
 
        val flatExprReg = toReg(flattenExpr(expr))
        val returnReg = VirtualReg(-1, flatExprReg.size)
        
        /* Move return value to register -1 (return reg) */
        funcState.emitInstruction(Mov(returnReg, flatExprReg))
        funcState.emitInstruction(Ret)
    }

    /**
      * Flatten a call statement
      *
      * @param expr The call statement to be flattened
      * @param funcState The current scope function state
      * @param globalContext The flattener global context
      */
    private def flattenCall(ident: Ident[SemanticData],
                            argList: List[Expr[SemanticData]])
                            (using funcState: FunctionState,
                                   globalContext: GlobalFlattenContext): Operand = {
        
        /* Flatten function arguments */
        val flatArgs: List[Operand] = argList.map(arg => flattenExpr(arg))

        /* Extract KnownType of function return */
        val returnType = ident.info match {
            case SemanticData.Func(_, _, typ, _) => typ
            case _ => ???
        }
        val knownReturnType = returnType match
            case typ: KnownType => typ
        
        /* Allocate return register */
        val resultReg = funcState.anonymousReg(knownReturnType)

        /* Emit call instruction */
        val funcLabel = Label(s"f_${ident.name}")
        funcState.emitInstruction(wacc.backend.Call(funcLabel, flatArgs, Some(resultReg)))

        resultReg
    }
}