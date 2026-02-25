package wacc.backend

import wacc.frontend.*
import scala.collection.mutable.{ListBuffer, Map}
import wacc.ExitCodes.*
import wacc.backend.IR

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
    private val virtualRegMap = Map[QualifiedName, IR.VirtualReg]()
    private var nextVirtualId = 0

    /* The list of instructions for the function */
    private val instructions = ListBuffer[IR.Instruction]()

    /**
      * Look up the virtual register of a given variable. A precondition is that the variable
      * must already have an allocated VirtualReg.
      *
      * @param varName the string name of the variable
      * @param renameId the renamed ID from the frontend of the variable
      * @return the VirtualReg assigned to the variable from a prevoius `defineReg` call
      */
    def lookupReg(varName: String, renameId: Int): IR.VirtualReg = {
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
    def defineReg(varName: String, renameId: Int, typ: KnownType): IR.VirtualReg = {
        val nextReg = IR.VirtualReg(nextVirtualId, Size.fromType(typ))
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
    def defineReg(varName: String, renameId: Int, size: Size): IR.VirtualReg = {
        val nextReg = IR.VirtualReg(nextVirtualId, size)
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
    def anonymousReg(typ: KnownType): IR.VirtualReg = {
        val nextReg = IR.VirtualReg(nextVirtualId, Size.fromType(typ))
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
    def anonymousReg(size: Size): IR.VirtualReg = {
        val nextReg = IR.VirtualReg(nextVirtualId, size)
        nextVirtualId += 1
        nextReg
    }

    /**
      * Emits an instruction to the instruction buffer
      *
      * @param instr
      */
    def emitInstruction(instr: IR.Instruction): Unit = instructions += instr

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
    private val directives = ListBuffer[IR.DataDirective]()

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
    def getNextLabel(prefix: String): IR.Label = {
        val label = IR.Label(s"${prefix}_${nextLabelId}")
        nextLabelId += 1
        label
    }

    /* Mapping strings to their memory address locations (labels) */
    private val strMap = Map[String, IR.Label]()
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
    def addString(str: String): IR.Label = {
        strMap.get(str) match
            case None => {
                val strLabel = IR.Label(s"string_${strCount}")

                /* Escape chars must be written with backslashes in the assembly */
                val escapedStr = str
                    .replace("\"", "\\\"")
                    .replace("\'", "\\\'")
                    .replace("\n", "\\n")

                directives += IR.DataDirective(strLabel, escapedStr)
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
    private val ARRAY_LEN_HEADER_SIZE_B = 4

    /**
      * The main function of the Flattener. Inputs a typed AST (Program[SemanticData]) from the
      * frontend typechecker and outputs a `RegProgram` which is a flattened version of the AST
      * into a high level 3-register assembley IR.
      *
      * @param typedAST the type checker AST output from the frontend
      * @return a `RegProgram` 3-register assembley IR
      */
    def flatten(typedAST: Program[SemanticData]): IR.RegProgram = {
        val Program(funcs, stmt, _) = typedAST

        /* Initialise contexts */
        given funcState: FunctionState = FunctionState()
        given directiveState: GlobalFlattenContext = GlobalFlattenContext()

        /* Flatten the main statement which populates the funcState
           instructions buffer */
        flattenStmt(stmt)

        IR.RegProgram(funcs.map(flattenFunc),
                   directiveState.getAllDirectives,
                   funcState.getAllInstructions)
    }

    /**
      * Converts an operand to a virtual register by making an anonymous register
      * if the operand is an immediate
      *
      * @param op The operand to convert to a register
      * @param targetType The type of the operand
      * 
      * @return The virtual register for the operand
      */
    private def toReg(op: IR.Operand, targetType: KnownType = KnownType.Int)
                     (using funcState: FunctionState): IR.VirtualReg = op match {
        case reg: IR.VirtualReg => reg
        case imm: IR.Immediate => 
            val newReg = funcState.anonymousReg(targetType)
            funcState.emitInstruction(IR.Mov(newReg, imm))
            newReg
        case label: IR.Label => ???
    }
    
    /**
      * Converts an operand to a virtual register by making an anonymous register
      * if the operand is an immediate
      *
      * @param op The operand to convert to a register
      * @param size The size of the operand
      * 
      * @return The virtual register for the operand
      */
    private def toReg(op: IR.Operand, size: Size)
                     (using funcState: FunctionState): IR.VirtualReg = op match {
        case reg: IR.VirtualReg => reg
        case imm: IR.Immediate => 
            val newReg = funcState.anonymousReg(size)
            funcState.emitInstruction(IR.Mov(newReg, imm))
            newReg
        case label: IR.Label => ???
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
        case Stmt.Free(expr, _) => flattenFree(expr)
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
        
        val destReg: IR.VirtualReg = funcState.defineReg(ident.name, varId, kty)
        val srcOperand: IR.Operand = flattenRValue(rval)

        /* what if destReg's size and srcOperand's size are different? */

        funcState.emitInstruction(IR.Mov(destReg, srcOperand))
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
        val srcOperand: IR.Operand = flattenRValue(rval)
        
        /* We need to generate different high level IR based on the lvalue. If it's a pair access
            we have to use PairStore instead of a simple Mov, so we pattern match lval */
        lval match {
            case _: Ident[SemanticData] =>
                val destReg: IR.VirtualReg = flattenLValue(lval)
                funcState.emitInstruction(IR.Mov(destReg, srcOperand))
            case p: PairElem[SemanticData] => 
                val (pairPtr, pos) = resolvePairElem(p) /* get the base pointer and index */
                /* null check when accessing the pair p */
                funcState.emitInstruction(IR.NullCheck(pairPtr))
                funcState.emitInstruction(IR.PairStore(pairPtr, pos, srcOperand))
            case a: ArrayElem[SemanticData] =>
                val (arrayPtr, index) = resolveArrayElem(a)
                val SemanticData.Typed(_, aElemType, _) = a.info: @unchecked
                funcState.emitInstruction(IR.ArrayStore(arrayPtr, index, srcOperand, Size.fromSemType(aElemType)))
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): IR.VirtualReg = lval match {
        case ident: Ident[SemanticData] =>
            val SemanticData.Var(_, varId, _) = ident.info: @unchecked 
            funcState.lookupReg(ident.name, varId)
        
        case p: PairElem[SemanticData] =>
            /* To write to an inner pair (lvalue means that), we must first find out where 
                it lives in memory. If we have something like `fst fst p`, to 
                get the address of this whole expression, we need to get the address 
                of the inner `fst p`, and so on.  */
            flattenPairElem(p)

        case a: ArrayElem[SemanticData] =>
            flattenArrayElem(a)
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): IR.Operand = rval match {
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
            funcState.emitInstruction(IR.HeapAlloc(pairPtr, IR.Immediate(PAIR_HEAP_SIZE_B)))

            funcState.emitInstruction(IR.PairStore(pairPtr, PairPos.Fst, src1))
            funcState.emitInstruction(IR.PairStore(pairPtr, PairPos.Snd, src2))
            
            pairPtr
        case a: ArrayLiter[SemanticData] => flattenArrayLiter(a)
        case p: PairElem[SemanticData] => flattenPairElem(p)
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): IR.VirtualReg = {
        val PairElem(_, _, SemanticData.Typed(_, t, _)) = p: @unchecked
        val (pairPtr, pos) = resolvePairElem(p)

        /* emit the null check when referencing a pair */
        funcState.emitInstruction(IR.NullCheck(pairPtr))

        /* Extract the element size from its type and allocate an anonymous register
            based on that type. */
        val pairElemSize = Size.fromSemType(t)

        val destReg = funcState.anonymousReg(pairElemSize)

        /* Load the `pos`-index of the pair from pairPtr into destReg */
        funcState.emitInstruction(IR.PairLoad(pairPtr, pos, destReg))

        destReg
    }

    /**
     * Flattens an `ArrayLiter` AST node by allocating heap memory with a length header,
     * storing each element, and returning the pointer to the first element.
     * 
     * @param a The `ArrayLiter` AST node to flatten
     * @return A `VirtualReg` containing the pointer to the first array element
     */
    private def flattenArrayLiter(
        a: ArrayLiter[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): IR.VirtualReg = {
        val ArrayLiter(elements, info) = a
        val SemanticData.Typed(_, arrayType, Some(length)) = info: @unchecked
        
        /* extract element type - handle both Array and String types */
        val elemType = arrayType match {
            case KnownType.Array(et) => et
            case KnownType.String => KnownType.Char
            case _ => ??? /* unreachable for well-typed programs */
        }
        
        /* determine element size and total heap allocation needed */
        val elemSize = Size.fromSemType(elemType)
        val totalSize = ARRAY_LEN_HEADER_SIZE_B + length * Size.bytes(elemSize)
        
        /* allocate heap memory */
        val arrayBase = funcState.anonymousReg(Size.X)
        funcState.emitInstruction(IR.HeapAlloc(arrayBase, IR.Immediate(totalSize)))
        
        /* store length header at offset 0 */
        val lengthReg = funcState.anonymousReg(Size.W)
        funcState.emitInstruction(IR.Mov(lengthReg, IR.Immediate(length)))
        funcState.emitInstruction(IR.Store(lengthReg, IR.AddrImmOffset(arrayBase, IR.Immediate(0))))
        
        /* calculate pointer to first element (base + 4) */
        val arrayPtr = funcState.anonymousReg(Size.X)
        funcState.emitInstruction(IR.Add(arrayPtr, arrayBase, IR.Immediate(ARRAY_LEN_HEADER_SIZE_B)))
        
        /* flatten and store each element */
        elements.zipWithIndex.foreach { case (elem, idx) =>
            val elemValue = flattenExpr(elem)
            funcState.emitInstruction(IR.ArrayStore(arrayPtr, IR.Immediate(idx), elemValue, elemSize))
        }
        
        arrayPtr
    }

    /**
     * Flattens an `ArrayElem` AST node by loading the array element from the heap.
     * Handles multi-dimensional arrays by progressively indexing through each dimension.
     * 
     * @param a The `ArrayElem` AST node to flatten
     * @return A `VirtualReg` containing the loaded array element value
     */
    private def flattenArrayElem(
        a: ArrayElem[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): IR.VirtualReg = {
        val ArrayElem(ident, indices, info) = a
        val SemanticData.Typed(_, elemType, _) = info: @unchecked
        
        /* start with the base array pointer */
        val SemanticData.Var(_, varId, _) = ident.info: @unchecked
        var arrayPtr = funcState.lookupReg(ident.name, varId)
        
        /* apply each index, loading the intermediate results */
        indices.init.foreach { indexExpr =>
            val indexOp = flattenExpr(indexExpr)
            val nextPtr = funcState.anonymousReg(Size.X)
            funcState.emitInstruction(IR.ArrayLoad(arrayPtr, indexOp, nextPtr))
            arrayPtr = nextPtr
        }
        
        /* final index loads the actual element value */
        val finalIndex = flattenExpr(indices.last)
        val elemSize = Size.fromSemType(elemType)
        val resultReg = funcState.anonymousReg(elemSize)
        funcState.emitInstruction(IR.ArrayLoad(arrayPtr, finalIndex, resultReg))
        
        resultReg
    }
    

    /**
     * Resolve the virtual register that contains the pair and the position.
     * 
     * @param pairElem The `PairElem` to resolve
     * @return `(VirtualReg, PairPos)` the virtual base register of the pair and fst/snd
     */ 
    private def resolvePairElem(
        pairElem: PairElem[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): (IR.VirtualReg, PairPos) = {
        val PairElem(isFst, lval, _) = pairElem
         /* flatten the lvalue to get the pair register */
        val pairPtr = flattenLValue(lval)

        val pos = if isFst then PairPos.Fst else PairPos.Snd
        (pairPtr, pos)
    }

    /**
     * Resolve the array pointer and final index for an `ArrayElem`.
     * Handles multi-dimensional arrays by applying all but the last index.
     * 
     * @param arrayElem The `ArrayElem` to resolve
     * @return `(VirtualReg, Operand)` the array pointer and final index
     */ 
    private def resolveArrayElem(
        arrayElem: ArrayElem[SemanticData]
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext): (IR.VirtualReg, IR.Operand) = {
        val ArrayElem(ident, indices, _) = arrayElem
        
        /* start with the base array pointer */
        val SemanticData.Var(_, varId, _) = ident.info: @unchecked
        var arrayPtr = funcState.lookupReg(ident.name, varId)
        
        /* apply all indices except the last, loading intermediate array pointers */
        indices.init.foreach { indexExpr =>
            val indexOp = flattenExpr(indexExpr)
            val nextPtr = funcState.anonymousReg(Size.X)
            funcState.emitInstruction(IR.ArrayLoad(arrayPtr, indexOp, nextPtr))
            arrayPtr = nextPtr
        }
        
        /* return the final array pointer and last index */
        val finalIndex = flattenExpr(indices.last)
        (arrayPtr, finalIndex)
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
        val callInstr = IR.Call(IR.Label("exit"), List(exitCode), None)
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
        
        /* Extract type from lvalue to determine which read widget to use */
        val readType = lval match {
            case ident: Ident[SemanticData] =>
                val SemanticData.Var(_, _, semType) = ident.info: @unchecked
                semType

            case p: PairElem[SemanticData] =>
                val PairElem(_, _, SemanticData.Typed(_, semType, _)) = p: @unchecked
                semType

            case a: ArrayElem[SemanticData] =>
                val ArrayElem(_, _, SemanticData.Typed(_, semType, _)) = a: @unchecked
                semType
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
        
        /* Determine how to store the read value */
        lval match {
            case _: Ident[SemanticData] =>
                /* For simple identifiers, read directly into the variable's register */
                val destReg = flattenLValue(lval)
                funcState.emitInstruction(IR.Read(destReg, readWidget))
            
            case p: PairElem[SemanticData] =>
                /* pair elements are heap memory, so read into temp then store back */
                val (pairPtr, pos) = resolvePairElem(p)
                funcState.emitInstruction(IR.NullCheck(pairPtr))
                
                val tempReg = funcState.anonymousReg(knownType)
                funcState.emitInstruction(IR.PairLoad(pairPtr, pos, tempReg))
                funcState.emitInstruction(IR.Read(tempReg, readWidget))
                funcState.emitInstruction(IR.PairStore(pairPtr, pos, tempReg))
            
            case a: ArrayElem[SemanticData] =>
                /* array elements are also heap memory, so read into temp then store back */
                val (arrayPtr, index) = resolveArrayElem(a)
                
                val tempReg = funcState.anonymousReg(knownType)
                funcState.emitInstruction(IR.Read(tempReg, readWidget))
                funcState.emitInstruction(IR.ArrayStore(arrayPtr, index, tempReg, Size.fromSemType(knownType)))
        }
    }

    /**
     * Flattens an expression. 
     * 
     * From the type checker output, we know that the expr's type is either a
     * Pair or an Array.
     * 
     * @param expr the expression to free up. This is the pointer we want to free
     */
    private def flattenFree(expr: Expr[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext) : Unit = {
        require(
            expr.expressionType match {
                /* null has type pair */
                case KnownType.Pair(_, _) | KnownType.Array(_) | PairAny | Null => true
                case _ => false
            },
            "Backend Error: 'free' statement received " +
              s"invalid type ${expr.expressionType}. Must be an array or pair."
        )
        val freeType = expr.expressionType match {
            case KnownType.Array(_) => FreeType.Array
            case _ => FreeType.Pair
        }
        /* evaluate the expression to get the pointer. Since it's a pointer, we can
            use `Size.X` */
        val ptrReg = toReg(flattenExpr(expr), Size.X) 
        funcState.emitInstruction(IR.Free(ptrReg, freeType))
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
            case wacc.frontend.KnownType.Array(KnownType.Char) => PrintStringWidget
            case wacc.frontend.KnownType.Array(_) => PrintPointerWidget
            case wacc.frontend.Null => PrintPointerWidget
        

        funcState.emitInstruction(IR.Print(printVal, printWidget, newline))
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
        funcState.emitInstruction(IR.Cmp(toReg(flatCondition), IR.Immediate(0)))
        funcState.emitInstruction(IR.BranchCond(Condition.EQ, elseLabel))

        /* Fall through to the true case and then jump to the end */
        flattenStmt(ifBlock)
        funcState.emitInstruction(IR.BranchUncond(endLabel))

        /* Define the else section */
        funcState.emitInstruction(IR.LabelDef(elseLabel))
        flattenStmt(elseBlock)

        /* End of if statement label */
        funcState.emitInstruction(IR.LabelDef(endLabel))
    }

    private def flattenWhile(cond: Expr[SemanticData],
                             body: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext) = {
        
        /* Define labels */
        val bodyLabel = directiveState.getNextLabel(".L_body")
        val condLabel = directiveState.getNextLabel(".L_cond")

        /* Jump to the condition */
        funcState.emitInstruction(IR.BranchUncond(condLabel))

        /* Loop body */
        funcState.emitInstruction(IR.LabelDef(bodyLabel))
        flattenStmt(body)

        /* Evaluate the condition and jump back if it's true */
        funcState.emitInstruction(IR.LabelDef(condLabel))
        val flatCondition = flattenExpr(cond)
        funcState.emitInstruction(IR.Cmp(toReg(flatCondition), IR.Immediate(1)))
        funcState.emitInstruction(IR.BranchCond(Condition.EQ, bodyLabel))
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
                                  directiveState: GlobalFlattenContext): IR.Operand = typedExpr match
        case IntLiter(value, _) => IR.Immediate(value)
        case BoolLiter(value, _) => IR.Immediate(if value then 1 else 0)
        case CharLiter(value, _) => IR.Immediate(value)
        case StrLiter(value, _) => directiveState.addString(value)
        case PairLiter(_) => IR.Immediate(0) /* reference compiler uses 0 to represent `null` */
        case Ident(name, SemanticData.Var(_, renameId, _)) => funcState.lookupReg(name, renameId)
        case BinaryExpr(op, left, right, _) => flattenBinaryExpr(op, left, right)
        case UnaryExpr(op, expr, _) => flattenUnaryExpr(op, expr)
        case a: ArrayElem[SemanticData] => flattenArrayElem(a)
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
                                         directiveState: GlobalFlattenContext): IR.VirtualReg = {
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
                flattenBinaryOp(leftOp, rightOp, IR.And.apply, KnownType.Bool)
            case BinaryOperator.Or => 
                flattenBinaryOp(leftOp, rightOp, IR.Or.apply, KnownType.Bool)
            case BinaryOperator.Add => flattenBinaryOp(leftOp, rightOp, IR.Add.apply, KnownType.Int)
            case BinaryOperator.Sub => flattenBinaryOp(leftOp, rightOp, IR.Sub.apply, KnownType.Int)
            case BinaryOperator.Mul => flattenBinaryOp(leftOp, rightOp, IR.Mul.apply, KnownType.Int)
            case BinaryOperator.Div => flattenBinaryOp(leftOp, rightOp, IR.Div.apply, KnownType.Int)
            case BinaryOperator.Mod => flattenBinaryOp(leftOp, rightOp, IR.Mod.apply, KnownType.Int)
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
                                  leftOp: IR.Operand,
                                  rightOp: IR.Operand,
                                  compType: KnownType)
                                  (using funcState: FunctionState): IR.VirtualReg = {
        val leftReg = toReg(leftOp, compType)
        val rightReg = toReg(rightOp, compType)

        /* Emit the compare instruction */
        funcState.emitInstruction(IR.Cmp(leftReg, rightReg))

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
        funcState.emitInstruction(IR.SetCond(resultReg, condition))

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
                                        directiveState: GlobalFlattenContext): IR.VirtualReg = {
        val operand = flattenExpr(expr)

        op match {
            case UnaryOperator.Not => flattenNot(operand)
            case UnaryOperator.Chr => flattenChr(operand)
            case UnaryOperator.Ord => flattenOrd(operand)
            case UnaryOperator.Neg => flattenNeg(operand)
            case UnaryOperator.Len => flattenLen(operand)
        }
    }

    /**
      * Flatten a `len` unary operator. Loads the length header from array memory.
      *
      * @param arrayOp The array operand (pointer to first element)
      * @param funcState The current scope function state
      * @return The virtual register containing the array length
      */
    private def flattenLen(arrayOp: IR.Operand)
                          (using funcState: FunctionState): IR.VirtualReg = {
        /* convert operand to register if it's not already */
        val arrayPtr = arrayOp match {
            case reg: IR.VirtualReg => reg
            case imm: IR.Immediate =>
                val tempReg = funcState.anonymousReg(Size.X)
                funcState.emitInstruction(IR.Mov(tempReg, imm))
                tempReg
            case _ => ???
        }
        
        /* emit ArrayLen instruction to load length from array header */
        val lengthReg = funcState.anonymousReg(Size.W)
        funcState.emitInstruction(IR.ArrayLen(arrayPtr, lengthReg))
        
        lengthReg
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
    private def flattenChr(operand: IR.Operand)
                          (using funcState: FunctionState): IR.VirtualReg = {

        /* Chr input is an int so we use KnownType.Int */
        val srcReg = toReg(operand, KnownType.Int)

        /* Emit Chr high level IR instruction with matching reg sizes (both W) */
        val chrIntReg = funcState.anonymousReg(KnownType.Int)
        funcState.emitInstruction(IR.Chr(chrIntReg, srcReg, BadCharWidgetError))

        /* Downcast Char reg to Bool reg */
        val chrCharReg = funcState.anonymousReg(KnownType.Char)
        funcState.emitInstruction(IR.SignExtend(chrCharReg, chrIntReg))

        chrCharReg
    }

    /**
      * Flatten an `ord` instruction.
      *
      * @param operand The operand passed to the Ord call
      * @param funcState The current scope function state
      * @return The virtual register containing the ord result
      */
    private def flattenOrd(operand: IR.Operand)
                        (using funcState: FunctionState): IR.VirtualReg = {

        /* Get the source char register (since `ord` takes chars) */
        val srcReg = toReg(operand, KnownType.Char)
        
        /* Allocate a new W register for the result */
        val destReg = funcState.anonymousReg(KnownType.Int)
        
        /* Sign extend to the larger register (B to W).
           Can't do a MOV here because moves have to be between
           registers of the same size - this is the point of sign extension */
        funcState.emitInstruction(IR.SignExtend(destReg, srcReg))
        
        destReg
    }

    /**
      * Flatten a negation statement
      *
      * @param operand The operand passed to the negation statement
      * @param funcState The current scope function state
      * @return The virtual register with the negation result
      */
    private def flattenNeg(operand: IR.Operand)
                          (using funcState: FunctionState): IR.VirtualReg = {
        val reg = toReg(operand)
        val negReg = funcState.anonymousReg(KnownType.Int)
        funcState.emitInstruction(IR.Neg(negReg, reg))
        negReg
    }

    /**
      * Flatten a boolean NOT operation
      *
      * @param operand The operand to negate
      * @param funcState The current scope function state
      * @return The virtual register containing the negated result
      */
    private def flattenNot(operand: IR.Operand)
                          (using funcState: FunctionState): IR.VirtualReg = {
        val srcReg = toReg(operand, KnownType.Bool)
        val resultReg = funcState.anonymousReg(KnownType.Bool)

        funcState.emitInstruction(IR.Not(resultReg, srcReg))
        
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
    private def flattenBinaryOp(leftOp: IR.Operand,
                                rightOp: IR.Operand,
                                makeInstr: (IR.VirtualReg, IR.VirtualReg, IR.VirtualReg) => IR.Instruction,
                                operandType: KnownType = KnownType.Int)
                               (using funcState: FunctionState): IR.VirtualReg = {
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
                           (using directiveState: GlobalFlattenContext): IR.FunctionDef = {

        /* Create a fresh function state */
        given funcState: FunctionState = FunctionState()

        val Func(typ, ident, params, stmt, _) = typedFunc

        /* Function label */
        val funcLabel = IR.Label(s"f_${ident.name}")

        val flatParams = params.map(flattenParam)
        flattenStmt(stmt)

        IR.FunctionDef(funcLabel.name, flatParams, funcState.getAllInstructions)
    }

    /**
      * Flatten a function parameter
      *
      * @param param The function param to flatten
      * @param funcState The current scope function state
      * @return The virtual register containing the flattened param
      */
    private def flattenParam(param: Param[SemanticData])
                            (using funcState: FunctionState): IR.VirtualReg = {

        
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
        val returnReg = IR.VirtualReg(-1, flatExprReg.size)
        
        /* Move return value to register -1 (return reg) */
        funcState.emitInstruction(IR.Mov(returnReg, flatExprReg))
        funcState.emitInstruction(IR.Ret)
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
                                   globalContext: GlobalFlattenContext): IR.Operand = {
        
        /* Flatten function arguments */
        val flatArgs: List[IR.Operand] = argList.map(arg => flattenExpr(arg))

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
        val funcLabel = IR.Label(s"f_${ident.name}")
        funcState.emitInstruction(IR.Call(funcLabel, flatArgs, Some(resultReg)))

        resultReg
    }
}