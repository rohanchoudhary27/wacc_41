package wacc.backend

import wacc.frontend.*
import wacc.*
import scala.collection.mutable.{ListBuffer, Map}
import wacc.backend.TAC

/**
  * A state class managing the virtual register allocation and instructions that make up
  * a function. The main body of a WACC program is treated as a 'main' function.
  */
private class FunctionState(
) {

    /* A renamed variable from the AST. While the AST has this information already in
       SemanticData.Var, it's clearer to repackage it like this without the extra info */
    case class QualifiedName(varName: String, varId: Int)

    /* Map of variables to their virtual registers */
    private val virtualRegMap = Map[QualifiedName, TAC.VirtualReg]()
    private var nextVirtualId = 0

    /* The list of instructions for the function */
    private val instructions = ListBuffer[TAC.Instruction]()

    /**
      * Look up the virtual register of a given variable. A precondition is that the variable
      * must already have an allocated VirtualReg.
      *
      * @param varName the string name of the variable
      * @param renameId the renamed ID from the frontend of the variable
      * @return the VirtualReg assigned to the variable from a prevoius `defineReg` call
      */
    def lookupReg(varName: String, renameId: Int): TAC.VirtualReg = {
        virtualRegMap.get(QualifiedName(varName, renameId)) match
            case None => {
                BackendError.undefinedRegister(varName)
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
    def defineReg(varName: String, renameId: Int, typ: KnownType): TAC.VirtualReg = {
        val nextReg = TAC.VirtualReg(nextVirtualId, Size.fromType(typ))
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
    def anonymousReg(typ: KnownType): TAC.VirtualReg = anonymousReg(Size.fromType(typ))

    /**
      * Allocate a register as part of computing an expression (not to a variable) with
      * the given size.
      *
      * @param size the `Size` of the variable
      * @return
      */
    def anonymousReg(size: Size): TAC.VirtualReg = {
        val nextReg = TAC.VirtualReg(nextVirtualId, size)
        nextVirtualId += 1
        nextReg
    }

    /**
      * Emits an instruction to the instruction buffer
      *
      * @param instr
      */
    def emitInstruction(instr: TAC.Instruction): Unit = instructions += instr

    /**
      * Returns an immutable list of the function's instructions
      *
      * @return An immutable list of instructions
      */
    def getAllInstructions = instructions.toList
}

/**
  * State class for managing directives.
  */
private class GlobalFlattenContext(
) {
    private val directives = ListBuffer[TAC.DataDirective]()

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
    def getNextLabel(prefix: String): TAC.Label = {
        val label = TAC.Label(s"${prefix}_${nextLabelId}")
        nextLabelId += 1
        label
    }

    /* Mapping strings to their memory address locations (labels) */
    private val strMap = Map[String, TAC.Label]()
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
    def addString(str: String): TAC.Label = {
        strMap.get(str) match
            case None => {
                val strLabel = TAC.Label(LabelNameFormatter.stringName(strCount))

                /* Escape chars must be written with backslashes in the assembly */
                val escapedStr = str
                    .replace("\"", "\\\"")
                    .replace("\'", "\\\'")
                    .replace("\n", "\\n")

                directives += TAC.DataDirective(strLabel, escapedStr)
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
      * @param optimState: `FrontendOptimState` for extra info to the AST for WACC extensions
      * @return a `RegProgram` 3-register assembley IR
      */
    def flatten(typedAST: Program[SemanticData],
                optimState: FrontendOptimState = FrontendOptimState.empty): TAC.RegProgram = {

        val Program(_, _, funcs, stmt, _) = typedAST

        /* Initialise contexts */
        given funcState: FunctionState = FunctionState()
        given directiveState: GlobalFlattenContext = GlobalFlattenContext()
        given FrontendOptimState = optimState

        /* Flatten the main statement which populates the funcState
           instructions buffer */
        flattenStmt(stmt)

        TAC.RegProgram(funcs.map(flattenFunc),
                   directiveState.getAllDirectives,
                   funcState.getAllInstructions)
    }

    /**
      * Converts an operand to a virtual register by returning the register if the operand
      * is a register, making an anonymous register if the operand is an immediate, or making
      * an anonymous register with size `Size.X` if the operand is a label
      *
      * @param op The operand to convert to a register
      * @param targetType The type of the operand
      * 
      * @return The virtual register for the operand
      */
    private def toReg(op: TAC.Operand, targetType: KnownType = KnownType.Int)
                     (using FunctionState): TAC.VirtualReg =
        toReg(op, Size.fromType(targetType))
    
    /**
      * Converts an operand to a virtual register by returning the register if the operand
      * is a register, making an anonymous register if the operand is an immediate, or making
      * an anonymous register with size `Size.X` if the operand is a label
      *
      * @param op The operand to convert to a register
      * @param size The size of the operand
      * 
      * @return The virtual register for the operand
      */
    private def toReg(op: TAC.Operand, size: Size)
                     (using funcState: FunctionState): TAC.VirtualReg = op match {
        case reg: TAC.VirtualReg => reg
        case imm: TAC.Immediate => 
            val newReg = funcState.anonymousReg(size)
            funcState.emitInstruction(TAC.Mov(newReg, imm))
            newReg
        case label: TAC.Label => 
            /* Labels are memory addresses so they must go into a 64-bit 
               register (Size.X) regardless of the targetType parameter */
            val newReg = funcState.anonymousReg(Size.X)
            funcState.emitInstruction(TAC.Mov(newReg, label))
            newReg
    }

    /* Helper to emit array load instruction based on the optimstate */
    private def emitArrayLoad(
        array: TAC.VirtualReg,
        index: TAC.Operand,
        dest: TAC.VirtualReg,
        astNode: ArrayElem[SemanticData]
    )(using funcState: FunctionState, optimState: FrontendOptimState): Unit =
        if (optimState.safeArrayAccesses.contains(astNode))
            funcState.emitInstruction(TAC.ArrayLoadUnchecked(array, index, dest))
        else
            funcState.emitInstruction(TAC.ArrayLoad(array, index, dest))

    /* Helper to emit array store instruction based on the optimstate */
    private def emitArrayStore(
        array: TAC.VirtualReg,
        index: TAC.Operand,
        src: TAC.Operand,
        elemSize: Size,
        astNode: ArrayElem[SemanticData]
    )(using funcState: FunctionState, optimState: FrontendOptimState): Unit =
        if (optimState.safeArrayAccesses.contains(astNode))
            funcState.emitInstruction(TAC.ArrayStoreUnchecked(array, index, src, elemSize))
        else
            funcState.emitInstruction(TAC.ArrayStore(array, index, src, elemSize))


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
                                   directiveState: GlobalFlattenContext,
                                   optimState: FrontendOptimState): Unit = typedStmt match {

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
    private def flattenDecl(ident: Ident[SemanticData],
                            rval: RValue[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState): Unit = { 

        /* unlike assignments, declarations create a new variable binding, so we must call 
            defineReg to allocate a fresh `VirtualReg` and store it in the environment so 
            future lookups (eg `lvalue` or `expr` lookups) can find it */
        /* funcState's `defineReg` expects a `KnownType` but the type extracted from the
            `ident` is a `SemanticType`. But we know from type checker that it's always going
             to be a `KnownType`, so just convert it to a `KnownType`. */

        val kty: KnownType = ident.info.knownType
        
        val destReg: TAC.VirtualReg = funcState.defineReg(ident.name, ident.renameId, kty)
        val srcOperand: TAC.Operand = flattenRValue(rval)

        funcState.emitInstruction(TAC.Mov(destReg, srcOperand))
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
    private def flattenAsgn(lval: LValue[SemanticData],
                            rval: RValue[SemanticData])
                           (using funcState: FunctionState,
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState) = {

        /* assignments update existing state, and flattenLValue handles looking up
            the previously allocated VirtualReg for identifiers or memory for heap objects */
        val srcOperand: TAC.Operand = flattenRValue(rval)
        
        /* We need to generate different high level IR based on the lvalue. If it's a pair access
            we have to use PairStore instead of a simple Mov, so we pattern match lval */
        lval match {
            case _: Ident[SemanticData] =>
                val destReg: TAC.VirtualReg = flattenLValue(lval)
                funcState.emitInstruction(TAC.Mov(destReg, srcOperand))
            case p: PairElem[SemanticData] => 
                val (pairPtr, pos) = resolvePairElem(p) /* get the base pointer and index */
                /* null check when accessing the pair p IF it's not safe (maybe null) */
                if (!optimState.safePairAccesses.contains(p))
                    funcState.emitInstruction(TAC.NullCheck(pairPtr))
                funcState.emitInstruction(TAC.PairStore(pairPtr, pos, srcOperand))
            case a: ArrayElem[SemanticData] =>
                val (arrayPtr, index) = resolveArrayElem(a)
                emitArrayStore(arrayPtr, index, srcOperand, Size.fromSemType(a.info.semanticType), a)
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext,
            optimState: FrontendOptimState): TAC.VirtualReg = lval match {
        case ident: Ident[SemanticData] => funcState.lookupReg(ident.name, ident.renameId)

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
    private def flattenRValue(rval: RValue[SemanticData])
                             (using funcState: FunctionState,
                                    directiveState: GlobalFlattenContext,
                                    optimState: FrontendOptimState): TAC.Operand = rval match {
        case expr: Expr[SemanticData] => flattenExpr(expr)
        case call: Call[SemanticData] => flattenCall(call)
        case NewPair(e1, e2, _) => 

            /* flatten both expressions, they'll be put inside PairStore */
            val src1 = flattenExpr(e1)
            val src2 = flattenExpr(e2)

            /*  Pairs are always allocated in `MemoryLayout.PAIR_HEAP_SIZE_B` bytes on the heap, so
                get a new anonymous register with size `Size.X` and heap allocate with this many bytes.
                The freshly allocated anonymous register will be the base pointer of the pair, i.e.,
                the pointer that corresponds to the first element. this is simialr to C arrays
                where int *p = {1, 2, 3} the pointer p refers to the first element. */
            val pairPtr = funcState.anonymousReg(Size.X) 
            funcState.emitInstruction(TAC.HeapAlloc(pairPtr, TAC.Immediate(MemoryLayout.PAIR_HEAP_SIZE_B)))

            funcState.emitInstruction(TAC.PairStore(pairPtr, PairPos.Fst, src1))
            funcState.emitInstruction(TAC.PairStore(pairPtr, PairPos.Snd, src2))
            
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val (pairPtr, pos) = resolvePairElem(p)

        if (!optimState.safePairAccesses.contains(p)) {
            /* emit the null check when referencing a pair IF it's not safe */
            funcState.emitInstruction(TAC.NullCheck(pairPtr))
        }

        /* Extract the element size from its type and allocate an anonymous register
            based on that type. */
        val pairElemSize = Size.fromSemType(p.info.semanticType)

        val destReg = funcState.anonymousReg(pairElemSize)

        /* Load the `pos`-index of the pair from pairPtr into destReg */
        funcState.emitInstruction(TAC.PairLoad(pairPtr, pos, destReg))

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
    )(using funcState: FunctionState, 
            directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val ArrayLiter(elements, info) = a
        val SemanticData.Typed(_, arrayType, Some(length)) = info: @unchecked
        
        /* extract element type - handle both Array and String types */
        val elemType = (arrayType: @unchecked) match {
            case KnownType.Array(et) => et
            case KnownType.String => KnownType.Char
        }
        
        /* determine element size and total heap allocation needed */
        val elemSize = Size.fromSemType(elemType)
        val totalSize = MemoryLayout.ARRAY_LEN_HEADER_SIZE_B + length * Size.bytes(elemSize)
        
        /* allocate heap memory */
        val arrayBase = funcState.anonymousReg(Size.X)
        funcState.emitInstruction(TAC.HeapAlloc(arrayBase, TAC.Immediate(totalSize)))
        
        /* store length header at offset 0 */
        val lengthReg = funcState.anonymousReg(Size.W)
        funcState.emitInstruction(TAC.Mov(lengthReg, TAC.Immediate(length)))
        funcState.emitInstruction(TAC.Store(lengthReg, TAC.AddrImmOffset(arrayBase, TAC.Immediate(0))))
        
        /* calculate pointer to first element */
        val arrayPtr = funcState.anonymousReg(Size.X)
        funcState.emitInstruction(TAC.Add(arrayPtr, arrayBase, TAC.Immediate(MemoryLayout.ARRAY_LEN_HEADER_SIZE_B)))
        
        /* flatten and store each element */
        elements.zipWithIndex.foreach { case (elem, idx) =>
            val elemValue = flattenExpr(elem)
            funcState.emitInstruction(TAC.ArrayStore(arrayPtr, TAC.Immediate(idx), elemValue, elemSize))
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
    )(using funcState: FunctionState, 
            directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val ArrayElem(ident, indices, info) = a
        val elemType = info.semanticType
        
        /* start with the base array pointer */
        var arrayPtr = funcState.lookupReg(ident.name, ident.renameId)
        
        /* apply each index, loading the intermediate results */
        indices.init.foreach { indexExpr =>
            val indexOp = flattenExpr(indexExpr)
            val nextPtr = funcState.anonymousReg(Size.X)
            funcState.emitInstruction(TAC.ArrayLoad(arrayPtr, indexOp, nextPtr))
            arrayPtr = nextPtr
        }
        
        /* final index loads the actual element value */
        val finalIndex = flattenExpr(indices.last)
        val elemSize = Size.fromSemType(elemType)
        val resultReg = funcState.anonymousReg(elemSize)
        emitArrayLoad(arrayPtr, finalIndex, resultReg, a)
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
    )(using funcState: FunctionState, directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): (TAC.VirtualReg, PairPos) = {
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
    )(using funcState: FunctionState, 
            directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): (TAC.VirtualReg, TAC.Operand) = {
        val ArrayElem(ident, indices, _) = arrayElem
        
        /* start with the base array pointer */
        var arrayPtr = funcState.lookupReg(ident.name, ident.renameId)
        
        /* apply all indices except the last, loading intermediate array pointers */
        indices.init.foreach { indexExpr =>
            val indexOp = flattenExpr(indexExpr)
            val nextPtr = funcState.anonymousReg(Size.X)
            funcState.emitInstruction(TAC.ArrayLoad(arrayPtr, indexOp, nextPtr))
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
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState) = {
        val exitCode = flattenExpr(expr)
        val callInstr = TAC.Call(TAC.Label(LabelNameFormatter.exitLabel), List(exitCode), dest=None)
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
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState): Unit = {
        
        /* Extract type from lvalue to determine which read widget to use */
        val knownType = lval match {
            case ident: Ident[SemanticData] => ident.info.knownType
            case p: PairElem[SemanticData] => p.info.knownType
            case a: ArrayElem[SemanticData] => a.info.knownType
        }

        /* Read ONLY supports int and char */
        require(knownType == KnownType.Int || knownType == KnownType.Char)
        
        /* Select widget based on type */
        val readWidget = (knownType: @unchecked) match {
            case KnownType.Int => ReadIntegerWidget
            case KnownType.Char => ReadCharWidget
        }
        
        /* Determine how to store the read value */
        lval match {
            case _: Ident[SemanticData] =>
                /* For simple identifiers, read directly into the variable's register */
                val destReg = flattenLValue(lval)
                funcState.emitInstruction(TAC.Read(destReg, readWidget))
            
            case p: PairElem[SemanticData] =>
                /* pair elements are heap memory, so read into temp then store back */
                val (pairPtr, pos) = resolvePairElem(p)

                /* only emit a null check if it's maybe null, if we've proven that the pointer
                    is nonnull, don't emit a check. */
                if (!optimState.safePairAccesses.contains(p))
                    funcState.emitInstruction(TAC.NullCheck(pairPtr))
                
                val tempReg = funcState.anonymousReg(knownType)
                funcState.emitInstruction(TAC.PairLoad(pairPtr, pos, tempReg))
                funcState.emitInstruction(TAC.Read(tempReg, readWidget))
                funcState.emitInstruction(TAC.PairStore(pairPtr, pos, tempReg))
            
            case a: ArrayElem[SemanticData] =>
                /* array elements are also heap memory, so read into temp then store back */
                val (arrayPtr, index) = resolveArrayElem(a)
                
                val tempReg = funcState.anonymousReg(knownType)
                funcState.emitInstruction(TAC.Read(tempReg, readWidget))
                emitArrayStore(arrayPtr, index, tempReg, Size.fromSemType(knownType), a)
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
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState) : Unit = {
        require(
            expr.expressionType match {
                /* null has type pair */
                case KnownType.Pair(_, _) | KnownType.Array(_) | PairAny | Null => true
                case _ => false
            },
            InternalCompilerError.invalidFreeType(expr.expressionType.outputFormat)
        )
        val freeType = expr.expressionType match {
            case KnownType.Array(_) => FreeType.Array
            case _ => FreeType.Pair
        }
        /* evaluate the expression to get the pointer. Since it's a pointer, we can
            use `Size.X` */
        val ptrReg = toReg(flattenExpr(expr), Size.X) 
        funcState.emitInstruction(TAC.Free(ptrReg, freeType))
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
                                   directiveState: GlobalFlattenContext,
                                   optimState: FrontendOptimState) = {

        val printVal = flattenExpr(expr)
        val semType = expr.expressionType

        /* the semantic type cannot be `PairAny` or `?` because every expression's type is 
            known. */
        require(semType != PairAny && semType != ?)

        val printWidget: Widget = (semType: @unchecked) match
            case KnownType.Int =>  PrintIntWidget
            case KnownType.Bool => PrintBoolWidget
            case KnownType.Char => PrintCharWidget
            case KnownType.String => PrintStringWidget
            case KnownType.Pair(_, _) => PrintPointerWidget
            case KnownType.Array(KnownType.Char) => PrintStringWidget
            case KnownType.Array(_) => PrintPointerWidget
            case Null | EmptyArray => PrintPointerWidget
        

        funcState.emitInstruction(TAC.Print(printVal, printWidget, newline))
    }

    private def flattenIf(cond: Expr[SemanticData],
                          ifBlock: Stmt[SemanticData],
                          elseBlock: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext,
                                   optimState: FrontendOptimState) = {

        val flatCondition = flattenExpr(cond)

        /* Define labels */
        val elseLabel = directiveState.getNextLabel(LabelNameFormatter.elseLabel)
        val endLabel = directiveState.getNextLabel(LabelNameFormatter.ifEndLabel)

        /* If the condition is false, jump to the else case*/
        funcState.emitInstruction(TAC.Cmp(toReg(flatCondition), TAC.Immediate(Constants.FALSE)))
        funcState.emitInstruction(TAC.BranchCond(Condition.EQ, elseLabel))

        /* Fall through to the true case and then jump to the end */
        flattenStmt(ifBlock)
        funcState.emitInstruction(TAC.BranchUncond(endLabel))

        /* Define the else section */
        funcState.emitInstruction(TAC.LabelDef(elseLabel))
        flattenStmt(elseBlock)

        /* End of if statement label */
        funcState.emitInstruction(TAC.LabelDef(endLabel))
    }

    private def flattenWhile(cond: Expr[SemanticData],
                             body: Stmt[SemanticData])
                            (using funcState: FunctionState,
                                   directiveState: GlobalFlattenContext,
                                   optimState: FrontendOptimState) = {
        
        /* Define labels */
        val bodyLabel = directiveState.getNextLabel(LabelNameFormatter.whileBodyLabel)
        val condLabel = directiveState.getNextLabel(LabelNameFormatter.whileCondLabel)

        /* Jump to the condition */
        funcState.emitInstruction(TAC.BranchUncond(condLabel))

        /* Loop body */
        funcState.emitInstruction(TAC.LabelDef(bodyLabel))
        flattenStmt(body)

        /* Evaluate the condition and jump back if it's true */
        funcState.emitInstruction(TAC.LabelDef(condLabel))
        val flatCondition = flattenExpr(cond)
        funcState.emitInstruction(TAC.Cmp(toReg(flatCondition), TAC.Immediate(Constants.TRUE)))
        funcState.emitInstruction(TAC.BranchCond(Condition.EQ, bodyLabel))
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
                                     directiveState: GlobalFlattenContext,
                                     optimState: FrontendOptimState) = {
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
                                  directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState): TAC.Operand = typedExpr match
        case IntLiter(value, _) => TAC.Immediate(value)
        case BoolLiter(value, _) => TAC.Immediate(if value then Constants.TRUE else Constants.FALSE)
        case CharLiter(value, _) => TAC.Immediate(value)
        case StrLiter(value, _) => directiveState.addString(value)
        case PairLiter(_) => TAC.Immediate(MemoryLayout.NULL_PTR)
        case i: Ident[SemanticData] => funcState.lookupReg(i.name, i.renameId)
        case b: BinaryExpr[SemanticData] => flattenBinaryExpr(b)
        case u: UnaryExpr[SemanticData] => flattenUnaryExpr(u)
        case a: ArrayElem[SemanticData] => flattenArrayElem(a)
    
    /**
      * Flatten a binary expression
      *
      * @param binaryExpr The binary expression AST node
      * @param funcState The current scope function state
      * @param directiveState The global directive state
      * @return The virtual register containing the result
      */
    private def flattenBinaryExpr(
        binaryExpr: BinaryExpr[SemanticData]
    )(using funcState: FunctionState, 
            directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val BinaryExpr(op, left, right, _) = binaryExpr
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
                flattenBinaryOp(leftOp, rightOp, TAC.And.apply, KnownType.Bool)
            case BinaryOperator.Or => 
                flattenBinaryOp(leftOp, rightOp, TAC.Or.apply, KnownType.Bool)
            case BinaryOperator.Add => 
                flattenArithmetic(binaryExpr, leftOp, rightOp, TAC.Add.apply, TAC.AddUnchecked.apply) 
            case BinaryOperator.Sub =>
                flattenArithmetic(binaryExpr, leftOp, rightOp, TAC.Sub.apply, TAC.SubUnchecked.apply) 
            case BinaryOperator.Mul => 
                flattenArithmetic(binaryExpr, leftOp, rightOp, TAC.Mul.apply, TAC.MulUnchecked.apply) 
            case BinaryOperator.Div => 
                flattenArithmetic(binaryExpr, leftOp, rightOp, TAC.Div.apply, TAC.DivUnchecked.apply) 
            case BinaryOperator.Mod => 
                flattenArithmetic(binaryExpr, leftOp, rightOp, TAC.Mod.apply, TAC.ModUnchecked.apply) 
        }
    }

    /** 
     * Flatten an arithmetic (+, -, *, /, %) operation with overflow checks
     * 
     * @param b The binary expression AST node
     * @param leftOp The left operand of the binary expression
     * @param rightOp The right operand of the binary expression
     * @param constr The constructor function to generate the High Level IR
     * @param constrUnchecked The constructor function to generate the unchecked High Level IR
     */ 
    private def flattenArithmetic(
        b: BinaryExpr[SemanticData], 
        leftOp: TAC.Operand, 
        rightOp: TAC.Operand, 
        constr: (TAC.VirtualReg, TAC.VirtualReg, TAC.VirtualReg) => TAC.Instruction,
        constrUnchecked: (TAC.VirtualReg, TAC.VirtualReg, TAC.VirtualReg) => TAC.Instruction
    )(using funcState: FunctionState,
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val checkedOptimSet = b.op match {
            case BinaryOperator.Div | BinaryOperator.Mod => optimState.safeDivisions 
            case _ => optimState.safeOverflowExprs
        }
        if (checkedOptimSet.contains(b))
            flattenBinaryOp(leftOp, rightOp, constrUnchecked, KnownType.Int)
        else
            flattenBinaryOp(leftOp, rightOp, constr, KnownType.Int)
    }

    /**
      * Flatten a comparison operation
      *
      * @param op The comparison operator
      * @param leftOp The left operand
      * @param rightOp The right operand
      * @param funcState The current scope function state
      * @return The virtual register containing the comparison result
      * (Constants.TRUE or Constants.FALSE)
      */
    private def flattenComparison(op: BinaryOperator,
                                  leftOp: TAC.Operand,
                                  rightOp: TAC.Operand,
                                  compType: KnownType)
                                  (using funcState: FunctionState): TAC.VirtualReg = {
        val leftReg = toReg(leftOp, compType)
        val rightReg = toReg(rightOp, compType)

        /* Emit the compare instruction */
        funcState.emitInstruction(TAC.Cmp(leftReg, rightReg))

        /* Map frontend operator to backend condition */
        val condition = (op: @unchecked) match {
            case BinaryOperator.GT  => Condition.GT
            case BinaryOperator.GTE => Condition.GE
            case BinaryOperator.LT  => Condition.LT
            case BinaryOperator.LTE => Condition.LE
            case BinaryOperator.EQ  => Condition.EQ
            case BinaryOperator.NEQ => Condition.NE
        }

        /* Create result register and emit SetCond instruction */
        val resultReg = funcState.anonymousReg(KnownType.Bool)
        funcState.emitInstruction(TAC.SetCond(resultReg, condition))

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
    private def flattenUnaryExpr(
        u: UnaryExpr[SemanticData]
    )(using funcState: FunctionState,
            directiveState: GlobalFlattenContext, 
            optimState: FrontendOptimState): TAC.VirtualReg = {
        val UnaryExpr(op, expr, _) = u
        val operand = flattenExpr(expr)

        op match {
            case UnaryOperator.Not => flattenNot(operand)
            case UnaryOperator.Chr => flattenChr(u, operand)
            case UnaryOperator.Ord => flattenOrd(operand)
            case UnaryOperator.Neg => flattenNeg(u, operand)
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
    private def flattenLen(arrayOp: TAC.Operand)
                          (using funcState: FunctionState): TAC.VirtualReg = {
        /* convert operand to register if it's not already */
        val arrayPtr = toReg(arrayOp, Size.X)
        
        /* emit ArrayLen instruction to load length from array header */
        val lengthReg = funcState.anonymousReg(Size.W)
        funcState.emitInstruction(TAC.ArrayLen(arrayPtr, lengthReg))
        
        lengthReg
    }

    /**
      * Flatten a `chr` unary operator. Emits a `Chr` high level IR node
      * because the code generator needs have logic for the runtime error check (must be a valid
      * ascii char not just any int).
      *
      * @param u The unary expression node to check if the value is safe
      * @param operand The operand to the chr function
      * @param funcState The current scope function state
      * 
      * @return The virtual register containing the result
      */
    private def flattenChr(
        u: UnaryExpr[SemanticData],
        operand: TAC.Operand
    )(using funcState: FunctionState, optimState: FrontendOptimState): TAC.VirtualReg = {

        /* Chr input is an int so we use KnownType.Int */
        val srcReg = toReg(operand, KnownType.Int)

        /* Emit Chr high level IR instruction with matching reg sizes (both W) */
        val chrIntReg = funcState.anonymousReg(KnownType.Int)
        if (optimState.safeChrCalls.contains(u))
            funcState.emitInstruction(TAC.Chr(chrIntReg, srcReg, None))
        else
            funcState.emitInstruction(TAC.Chr(chrIntReg, srcReg, Some(BadCharWidgetError)))

        /* Downcast Char reg to Bool reg */
        val chrCharReg = funcState.anonymousReg(KnownType.Char)
        funcState.emitInstruction(TAC.SignExtend(chrCharReg, chrIntReg))

        chrCharReg
    }

    /**
      * Flatten an `ord` instruction.
      *
      * @param operand The operand passed to the Ord call
      * @param funcState The current scope function state
      * @return The virtual register containing the ord result
      */
    private def flattenOrd(operand: TAC.Operand)
                        (using funcState: FunctionState): TAC.VirtualReg = {

        /* Get the source char register (since `ord` takes chars) */
        val srcReg = toReg(operand, KnownType.Char)
        
        /* Allocate a new W register for the result */
        val destReg = funcState.anonymousReg(KnownType.Int)
        
        /* Sign extend to the larger register (B to W).
           Can't do a MOV here because moves have to be between
           registers of the same size - this is the point of sign extension */
        funcState.emitInstruction(TAC.SignExtend(destReg, srcReg))
        
        destReg
    }

    /**
      * Flatten a negation statement
      *
      * @param u The unary expression node to check if neg overflows or not
      * @param operand The operand passed to the negation statement
      * @param funcState The current scope function state
      * @return The virtual register with the negation result
      */
    private def flattenNeg(
        u: UnaryExpr[SemanticData], operand: TAC.Operand
    )(using funcState: FunctionState, optimState: FrontendOptimState): TAC.VirtualReg = {
        val reg = toReg(operand)
        val negReg = funcState.anonymousReg(KnownType.Int)
        if (optimState.safeOverflowExprs.contains(u))
            funcState.emitInstruction(TAC.NegUnchecked(negReg, reg))
        else
            funcState.emitInstruction(TAC.Neg(negReg, reg))
        negReg
    }

    /**
      * Flatten a boolean NOT operation
      *
      * @param operand The operand to negate
      * @param funcState The current scope function state
      * @return The virtual register containing the negated result
      */
    private def flattenNot(operand: TAC.Operand)
                          (using funcState: FunctionState): TAC.VirtualReg = {
        val srcReg = toReg(operand, KnownType.Bool)
        val resultReg = funcState.anonymousReg(KnownType.Bool)

        funcState.emitInstruction(TAC.Not(resultReg, srcReg))
        
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
    private def flattenBinaryOp(leftOp: TAC.Operand,
                                rightOp: TAC.Operand,
                                makeInstr: (TAC.VirtualReg, TAC.VirtualReg, TAC.VirtualReg) => TAC.Instruction,
                                operandType: KnownType)
                               (using funcState: FunctionState): TAC.VirtualReg = {
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
                           (using directiveState: GlobalFlattenContext,
                                  optimState: FrontendOptimState): TAC.FunctionDef = {

        /* Create a fresh function state */
        given funcState: FunctionState = FunctionState()

        val Func(typ, ident, params, stmt, _) = typedFunc

        /* Function label */
        val funcLabel = TAC.Label(LabelNameFormatter.funcEntry(ident.name))

        val flatParams = params.map(flattenParam)
        flattenStmt(stmt)

        TAC.FunctionDef(funcLabel.name, flatParams, funcState.getAllInstructions)
    }

    /**
      * Flatten a function parameter
      *
      * @param param The function param to flatten
      * @param funcState The current scope function state
      * @return The virtual register containing the flattened param
      */
    private def flattenParam(param: Param[SemanticData])
                            (using funcState: FunctionState): TAC.VirtualReg = {
        val ident = param.ident
        funcState.defineReg(ident.name, ident.renameId, ident.info.knownType)
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
                                   globalContext: GlobalFlattenContext,
                                   optimState: FrontendOptimState): Unit = {
 
        val flatExprReg = toReg(flattenExpr(expr))
        val returnReg = TAC.VirtualReg(MemoryLayout.VIRTUAL_RETURN_REG_ID, flatExprReg.size)
        
        /* Move return value to return reg */
        funcState.emitInstruction(TAC.Mov(returnReg, flatExprReg))
        funcState.emitInstruction(TAC.Ret)
    }

    /**
      * Flatten a call statement
      *
      * @param expr The call statement to be flattened
      * @param funcState The current scope function state
      * @param globalContext The flattener global context
      */
    private def flattenCall(callInstr: Call[SemanticData])
                           (using funcState: FunctionState,
                                  globalContext: GlobalFlattenContext,
                                  optimState: FrontendOptimState): TAC.Operand = {
        
        val Call(ident, argList, _) = callInstr
        
        /* Flatten function arguments */
        val flatArgs: List[TAC.Operand] = argList.map(arg => flattenExpr(arg))

        /* Extract KnownType of function return */
        val knownReturnType = ident.info.knownType
        
        /* Allocate return register */
        val resultReg = funcState.anonymousReg(knownReturnType)

        /* Emit call instruction */
        val funcLabel = TAC.Label(LabelNameFormatter.funcEntry(ident.name))

        /* Make the flat call node. If it is marked as self tail recursive, `TAC.SelfTailCall` is
           emitted. If it is marked as general tail recursive, `TAC.GeneralTailCall` is emitted */
        val flatCallInstr =
            if (optimState.selfTailRecCalls.contains(callInstr)) {
                TAC.SelfTailCall(funcLabel, flatArgs)
            } else if (optimState.generalTailRecCalls.contains(callInstr)) {
                TAC.GeneralTailCall(funcLabel, flatArgs)
            } else {
                TAC.Call(funcLabel, flatArgs, Some(resultReg))
            }

        funcState.emitInstruction(flatCallInstr)

        resultReg
    }
}

/**
 * Extension methods for SemanticType
 */
extension (typ: SemanticType) {
    /**
     * Enforces downcast from a SemanticType to a KnownType.
     * 
     * @throws IllegalStateException if the typechecker allowed an unkown type to leak
     */
    def asKnownType: KnownType = typ match {
        case kt: KnownType => kt
        case _ => BackendError.expectedKnownType(typ.outputFormat)
    }
}

/**
 * Extension methods for SemanticData
 */
extension (info: SemanticData) {
    /**
     * Extracts the SemanticType from any SemanticData node.
     * @throws IllegalStateException if the AST node unexpectedly lacks type information.
     */
    def semanticType: SemanticType = info match {
        case SemanticData.Typed(_, t, _)    => t 
        case SemanticData.Var(_, _, t)   => t
        case SemanticData.Func(_, _, t, _) => t
        case _ => BackendError.missingTypeInfo(info.toString)
    }

    /**
     * Extracts the KnownType from any SemanticData node.
     * @throws IllegalStateException if the AST node unexpectedly lacks type information or
     *                               cannot be converted to a `KnownType`
     */
    def knownType: KnownType = info.semanticType.asKnownType
}

/**
 * Extension methods for Ident
 */
extension (ident: Ident[SemanticData]) {
    /**
     * Get the renameId from an identifier
     * 
     * @return the renameId from the type information stored inside `info` parameter
     */
    def renameId: Int = ident.info match {
        case SemanticData.Var(_, renameId, _) => renameId
        case _ => BackendError.missingRenameId(ident.info.toString)
    }
}