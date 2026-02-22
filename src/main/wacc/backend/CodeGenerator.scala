package wacc.backend

import scala.collection.mutable.ListBuffer
import wacc.backend.RegisterAllocator.AllocationResult
import wacc.ExitCodes._

/**
  * Global context shared across all function code generation.
  * Manages label generation and data section directives.
  */
class GlobalGenContext {
    private var labelCounter = 0
    private val dataDirectives = ListBuffer[ArmDataDirective]()
    
    /** Get the final directive list */
    def finalDirectives: List[ArmDataDirective] = dataDirectives.toList

    /** Widget management */
    val dispatcher = RuntimeLibraryDispatcher()

    /** Generate a fresh unique label for compiler-generated code */
    def freshLabel(): ArmAddrLabel = {
        val l = s".L$labelCounter"
        labelCounter += 1
        ArmAddrLabel(l)
    }

    /**
      * Add a string constant to the data directives
      *
      * @param labelName The name of the label for the string to be stored
      * @param str The string to store
      * @return An `ArmAddrLabel` to the directive
      */
    def addStringConstant(labelName: String, str: String): ArmAddrLabel = {
        labelCounter += 1
        val label = ArmAddrLabel(labelName)
        dataDirectives += ArmDataDirective(label, str)
        label
    }
}

/**
  * State class managing code generation.
  * Tracks the allocation map and generated instructions.
  *
  * @param global the global context for labels and data directives
  * @param alloc the register allocation map (VirtualReg -> Location)
  */
class GenState(val global: GlobalGenContext, val alloc: Map[VirtualReg, Location]) {

    /* The current return label to branch to at the end of the current function */
    var currEpilogueLabel: Option[ArmAddrLabel] = None

    /* Program instructions (.text) */
    private val instrs = ListBuffer[ArmInstruction]()
    
    /** Get the final instruction list */
    def finalInstrs: List[ArmInstruction] = instrs.toList

    /** Add an instruction to the output */
    def +=(instr: ArmInstruction): Unit = instrs += instr

    /** Generate a fresh unique label */
    def freshLabel(): ArmAddrLabel = global.freshLabel()
}

/**
  * Code generator for translating High-Level IR to Low-Level IR.
  * 
  * Register allocation happens externally and is passed in via the alloc parameter.
  */
object CodeGenerator {

    /** Emit a single instruction to the output */
    private def emit(instr: ArmInstruction)(using st: GenState): Unit = 
        st += instr
    
    /** Look up the location of a virtual register */
    private def locOf(vReg: VirtualReg)(using st: GenState): Location = 
        st.alloc(vReg)
    
    /** Mark a widget as used (will be included in final assembly) */
    def useWidget(widget: Widget)(using st: GenState): Unit = st.global.dispatcher.use(widget)
    
    /** Get all collected widget assembly */
    def getWidgetAssembly()(using st: GenState): String = st.global.dispatcher.getCollectedAssembly

    /** Checks if an Immediate offset fits within the 9-bit signed immediate range (-256 to +255) 
     * allowed by the STR/LDR/STUR/LDUR instructions
     * 
     * @param offset the immediate offset value used in addressing mode
     */
    private def isSafeSignedOffset(offset: Int): Boolean = 
        offset >= -256 && offset<= 255

    /**
      * Helper to get the set of callee-saved registers (x19-x28) used in a function.
      * We only need to save the ones we actually assign
      */
    private def getUsedCalleeSaved(alloc: Map[VirtualReg, Location]): List[PhysicalReg] = {
        val used = alloc.values.collect { 
            case r: PhysicalReg if PhysicalReg.calleeSaved.contains(r) => r 
        }.toSet

        used.toList.sortBy(_.index)
    }

    /**
      * Helper to calculate the size of the local variables area.
      * Scans for the lowest StackSlot offset and aligns the size to 16 bytes.
      */
    private def getLocalsSize(alloc: Map[VirtualReg, Location]): Int = {
        val minOffset = alloc.values.collect { 
            case StackSlot(offset) => offset 
        }.minOption.getOrElse(0)
        
        /* offsets are negative so size is abs(minOffset) */
        val size = Math.abs(minOffset)
        
        if (size % 16 == 0) size else size + (16 - (size % 16)) /* 16 byte alignment */
    }

    /**
      * Generate Low-Level IR for the program.
      * 
      * @param regProgram the high-level IR program
      * @param allocs the register allocation result containing maps for main and all functions
      * @return the complete ARM program with data section and text section
      */
    def generate(regProgram: RegProgram,
                 allocs: AllocationResult): (ArmProgram, RuntimeLibraryDispatcher) = {

        /* Init global context for directives and labels and populate directives */
        val globalCtx = new GlobalGenContext()
        regProgram.data.foreach {dir => globalCtx.addStringConstant(dir.label.name, dir.storedValue)}

        /* GenState for the entire program (accumulates all functions and main) */
        given st: GenState = GenState(globalCtx, allocs.mainAlloc)
        regProgram.funcs.foreach { f => 
            val funcAlloc = allocs.funcAllocs.getOrElse(f.name, Map.empty)
            val funcInstrs = generateFunc(f, funcAlloc, globalCtx)
            funcInstrs.foreach(emit)
        }

        /* Emit code for the main function */
        emit(ArmLabelDef(ArmAddrLabel("main")))

        /* Calculate local variables size needed to put onto stack 
            and the calle-saved registers used */
        val localsSize = getLocalsSize(allocs.mainAlloc)
        val savedRegs = getUsedCalleeSaved(allocs.mainAlloc)

        /* To ensure compatibility with the register allocator's offset calculations,
            the stack frame is organized such that the FP remains adjacent to the LR
            at the top of the frame, with local variables immediately below it.
            Callee-saved registers are pushed to the bottom of the stack. This layout 
            preserves the relative offsets of local variables to the FP regardless of
            the number of callee-saved registers saved in the stack. */
        generateFuncPrologue(localsSize, savedRegs)
        generateInstrs(regProgram.body)
        /* main return 0 */
        emit(ArmMovImm(Size.W, PhysicalReg.argRegisters(0),
                       ArmImmediate(code(Success))))
        generateFuncEpilogue(localsSize, savedRegs)

        /* Final low-level IR for the program */
        val program = ArmProgram(globalCtx.finalDirectives, st.finalInstrs)

        (program, globalCtx.dispatcher)
    }

    /**
      * Generate low level IR for the function prologue.
      * 
      * The prologue sets up the stack frame:
      *     1. Frame Pointer (FP) and Link Register (LR) are saved to the stack
      *     2. The FP is updated to point to the current stack top 
      *     3. Space is reserved for local variables by subtracting from SP (`sub sp, sp, <size>`)
      *     4. Any callee-saved registers (x19-x28) used by the function are pushed onto
      *        the stack below the local variables to preserve offsets
      *
      * @param localsSize The size in bytes required for local variables (aligned to 16)
      * @param savedRegs  The list of used callee-saved physical registers to save
      */
    private def generateFuncPrologue(
        localsSize: Int, savedRegs: List[PhysicalReg]
    )(using GenState) = {
        /* save FP and LR */
        emit(ArmStorePair(Size.X,
                         PhysicalReg.framePointer,
                         PhysicalReg.linkReg,
                         ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))))

        /* set FP to be SP */
        emit(ArmMovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer))

        /* reserve space for locals (stack slots) */
        if (localsSize > 0) {
            /* sub sp, sp, <localsSize> */
            emit(ArmSubImm(
                Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer, ArmImmediate(localsSize)
            ))
        }

        /* push callee-saved registers in pairs */
        savedRegs.grouped(2).foreach {
            case List(r1, r2) => /* do stp xi, x(i+1), [sp, #-16]! */
                emit(ArmStorePair(
                    Size.X, r1, r2, ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
                ))
            case List(r1) => /* push 16 bytes for alignment */
                emit(ArmStore(
                    Size.X, r1, ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))
                ))
            case _ => ()
        }
    }
    
    /**
      * Generates the function epilogue
      *
      * The epilogue restores the previous state before returning to the caller:
      *     1. Callee-saved registers are popped from the stack
      *     2. The stack space reserved for local variables is reclaimed
      *     3. The caller's FP and the LR are restored
      *     4. `ret` emitted
      *
      * @param localsSize The size in bytes of the local variable area to reclaim
      * @param savedRegs  The list of callee-saved registers to restore
      */
    private def generateFuncEpilogue(
        localsSize: Int, savedRegs: List[PhysicalReg]
    )(using GenState) = {
        savedRegs.grouped(2).toList.reverse.foreach {
            case List(r1, r2) => 
                /* (r1, r2) was pushed in prologue, and stack top has r1 at low addr, r2 at high 
                    addr. LDP loads low addr into first register, high into second, so we load 
                    into r1, r2 */
                /* do ldp r1, r2, [sp], #16 */
                emit(ArmLoadPair(Size.X, r1, r2, 
                                 ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))))
            case List(r1) =>
                emit(ArmLoad(Size.X, r1, 
                             ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))))
            case _ =>
        }
        /* we have to free local variable space too */
        if (localsSize > 0) {
            emit(ArmAddImm(Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer, 
                           ArmImmediate(localsSize)))
        }
        /* restore FP and LR */
        emit(ArmLoadPair(Size.X,
                          PhysicalReg.framePointer,
                          PhysicalReg.linkReg, 
                          ArmAddrPostIndex(PhysicalReg.stackPointer, ArmImmediate(16))))
        emit(ArmRet)
    }

    /**
      * Generate Low-Level IR for a function definition.
      * 
      * @param func the function definition
      * @param alloc the register allocation map for this function
      * @param ctx the global context for labels and data directives
      * @return the low-level ARM instruction list
      */
    def generateFunc(func: FunctionDef, alloc: Map[VirtualReg, Location], ctx: GlobalGenContext): List[ArmInstruction] = {
        given st: GenState = GenState(ctx, alloc)
        
        /* Function label */
        emit(ArmLabelDef(ArmAddrLabel(func.name)))

        /* calculate requirements for prologue/epilogue */
        val localsSize = getLocalsSize(alloc)
        val savedRegs = getUsedCalleeSaved(alloc)

        generateFuncPrologue(localsSize, savedRegs)

        /* Parameters are moved to their allocated registers (by the RegisterAllocator)
           because they're caller save. E.g. they'll be used if the function calls another
           function. */
        func.args.zipWithIndex.foreach { case (arg, index) =>
            if (index < 8) {
                /* Parameter's assigned location by register allocator */
                val destLoc = alloc(arg)
                storeToLoc(PhysicalReg.argRegisters(index), destLoc, arg.size)
            }
        }

        val epilogueLabel = ArmAddrLabel(s".L_${func.name}_epilogue")
        st.currEpilogueLabel = Some(epilogueLabel)
        generateInstrs(func.body)

        emit(ArmLabelDef(epilogueLabel))
        generateFuncEpilogue(localsSize, savedRegs)
        
        st.finalInstrs
    }
    
    /**
      * Generate ARM instructions for a list of high-level instructions.
      * Wrapper that dispatches each instruction to generateInstr.
      */
    private def generateInstrs(instrs: List[Instruction])(using st: GenState): Unit =
        instrs.foreach(generateInstr)

    
    /**
      * Load a value from a location into a physical register. 
      * 
      * Handles large stack signed offsets by using a scratch register for address 
      * calculation. 
      *
      * @param loc the location to load from
      * @param size the register size
      * @param scratch the scratch register to use if loading from stack
      * @return the PhysicalReg containing the value
      */
    private def loadToReg(loc: Location, size: Size, scratch: PhysicalReg)
                         (using st: GenState): PhysicalReg = loc match {
        case reg: PhysicalReg => reg
        case StackSlot(offset) =>
            if (isSafeSignedOffset(offset)) {
                if (offset < 0) /* negative offset, use LDUR */ {
                    emit(ArmLdur(size, scratch, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))))  
                } else { /* positive offset, use LDR */
                    emit(ArmLoad(
                        size, scratch, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))
                    ))
                }
                
            } else {
                /* Large offset, so we need to load the offset into x17, then `ldr [fp, x17]` */
                val offsetReg = PhysicalReg.ipcScratch(1) // use x17
                emit(ArmMovImm(Size.X, offsetReg, ArmImmediate(offset)))
                emit(ArmLoad(size, scratch, ArmAddrRegOffset(PhysicalReg.framePointer, offsetReg)))
            }
            scratch
    }
    
    /**
      * Store a value from a physical register to a location. 
      * 
      * Handles large stack signed offsets by using a scratch register for address 
      * calculation. 
      *
      * @param src the source PhysicalReg
      * @param loc the destination location
      * @param size the register size
      */
    private def storeToLoc(
        src: PhysicalReg, loc: Location, size: Size
    )(using st: GenState): Unit = loc match {
        case dest: PhysicalReg =>
            if (src != dest) emit(ArmMovReg(size, dest, src))
        case StackSlot(offset) =>
            if (isSafeSignedOffset(offset)) {
                if (offset < 0) { /* negative offset, use STUR */
                    emit(ArmStur(
                        size, src, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))
                    ))
                } else { /* positive offset, use STR */
                    emit(ArmStore(
                        size, src, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))
                    ))
                }
            } else {
                /* large offset, so we need to load the offset into x17, then `str [fp, x17]` */
                val offsetReg = PhysicalReg.ipcScratch(1) // use x17
                emit(ArmMovImm(Size.X, offsetReg, ArmImmediate(offset)))
                emit(ArmStore(
                    size, src, ArmAddrRegOffset(PhysicalReg.framePointer, offsetReg)
                ))
            }
            
    }

    /**
     * helper function for loading and storing for binary operations (Mul, Div, Mod)
     */
    private def binOpRegs(dest: VirtualReg, src1: VirtualReg, src2: Operand)
                         (opBlock: (PhysicalReg, PhysicalReg, PhysicalReg) => Unit)
                         (using st: GenState): Unit = {
        
        /* Look up in register allocator where our variables are */
        val destLoc = locOf(dest)

        /* If src1 is a StackSlot then put into x16 scratch register, otherwise return curret register */
        val src1Reg = loadToReg(locOf(src1), dest.size, PhysicalReg.ipcScratch(0))

        /* Load src2 into the x17 scratch register */
        val src2Reg = src2 match {
            /* If virtual register, just load to x17 */
            case v: VirtualReg => loadToReg(locOf(v), dest.size, PhysicalReg.ipcScratch(1))
            /* If immediate, then move to x17 */
            case Immediate(v) => 
                val temp = PhysicalReg.ipcScratch(1)
                emit(ArmMovImm(dest.size, temp, ArmImmediate(v)))
                temp
            case _ => ???
        }

        /* If the dest location is a register then continue as is. 
           If the dest location is in stack then make dest x16 scratch register. */
        val resultReg = destLoc match {
            case r: PhysicalReg => r
            case _ => PhysicalReg.ipcScratch(0)
        }

        opBlock(resultReg, src1Reg, src2Reg)

        /* If comparison equal then save the valid result */
        storeToLoc(resultReg, destLoc, dest.size)
    }
    /**
      * Return the `Size` of an operand
      *
      * @param op The operand to get the size of
      * @return The register size of the opernad (Size.W / Size.X)
      */
    private def operandSize(op: Operand) = {
        op match {
            case v: VirtualReg => v.size
            case _: Immediate => Size.W
            case _: Label => Size.X
        }
    }


    /**
      * Generate a binary operation (register-register or register-immediate).
      * 
      * This function handles loading operands from their locations, choosing the correct
      * instruction variant (reg-reg vs reg-imm), and storing the result.
      *
      * @param dest the destination virtual register
      * @param src1 the first source virtual register
      * @param src2 the second source operand (VirtualReg or Immediate)
      * @param regOp constructor for register-register operation
      * @param immOp constructor for register-immediate operation
      */
    private def generateBinaryOp(
        dest: VirtualReg,
        src1: VirtualReg,
        src2: Operand,
        regOp: (Size, PhysicalReg, PhysicalReg, PhysicalReg) => ArmInstruction,
        immOp: (Size, PhysicalReg, PhysicalReg, ArmImmediate) => ArmInstruction
    )(using st: GenState): Unit = {
        val destLoc = locOf(dest)
        val src1Loc = locOf(src1)
        
        /* Load first operand */
        val src1Reg = loadToReg(src1Loc, dest.size, PhysicalReg.ipcScratch(0))

        /* Determine result register */
        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch(0)
        }
        
        (src2: @unchecked) match {
            case src2: VirtualReg =>
                /* Register-register operation */
                val src2Loc = locOf(src2)
                val src2Reg = loadToReg(src2Loc, dest.size, PhysicalReg.ipcScratch(1))
                emit(regOp(dest.size, resultReg, src1Reg, src2Reg))

            case Immediate(value) =>
                /* Register-immediate operation */
                emit(immOp(dest.size, resultReg, src1Reg, ArmImmediate(value)))
        }

        /* Store result */
        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
     * Generate low-level AArch64 IR for mutliplication with integer overflow checking
     * 
     * @param dest: destination virtual register to store result to
     * @param src1: first source virtual register
     * @param src1: second operand (can be an immediate  or virtual register)
     */
    private def generateMul(dest: VirtualReg, src1: VirtualReg, src2: Operand) (using st: GenState): Unit = {
        /* Tells RuntimeLibraryDispatcher to add the integer overflow widget to .s file */
        useWidget(IntOverflowErrorWidget)

        binOpRegs(dest, src1, src2) {
            (resultReg, src1Reg, src2Reg) =>

                /* Take two 32 bit (w) numbers and multiply, store as a 64 bit (x) register */
                emit(ArmSMull(resultReg, src1Reg, src2Reg))

                /* Take lower 32 bits of result and sign extend to 64 into x8.
                   If no overflow then the 32-bit version will be identical to the 64bit SMULL result. */
                val scratch3 = PhysicalReg.mathScratch
                emit(ArmSxtw(scratch3, resultReg))
                emit(ArmCmpReg(Size.X, resultReg, scratch3))

                /* If comparison not equal then an overflow occured, so branch to error widget */
                emit(ArmBranchCond(Condition.NE, ArmAddrLabel(IntOverflowErrorWidget.label)))
        }
    }

    /**
     * Generate low-level AArch64 IR for divide and modulo with division by zero checking
     * 
     * @param dest: destination virtual register to store result to
     * @param src1: first source virtual register
     * @param src1: second operand (can be an immediate  or virtual register)
     * @param isMod: flag to indicate whether integer division or modulo is to be carried out
     */
    private def generateDivMod(dest: VirtualReg, src1: VirtualReg, src2: Operand, isMod: Boolean) 
        (using st: GenState): Unit = {
        /* Tells RuntimeLibraryDispatcher to add the divisioon by zero widget to .s file */
        useWidget(DivByZeroErrorWidget)

        binOpRegs(dest, src1, src2) {
            (resultReg, src1Reg, src2Reg) => 
                /* Divide by zero check, if so jump to divide by zero widget */
                emit(ArmCbz(dest.size, src2Reg, ArmAddrLabel(DivByZeroErrorWidget.label)))

                /* Use flag to pick either integer division or modulo */
                if (isMod) {
                    /* Calculate modulo using integer division */
                    val scratch3 = PhysicalReg.mathScratch
                    emit(ArmDivReg(dest.size, scratch3, src1Reg, src2Reg))
                    emit(ArmMSub(dest.size, resultReg, scratch3, src2Reg, src1Reg))
                    /* Calculate integer division */
                } else {
                    emit(ArmDivReg(dest.size, resultReg, src1Reg, src2Reg))
                }
        }
    }

    /**
      * Generate the low-level IR for a unary operation
      *
      * @param dest The destination virtual register
      * @param src The source virtual register
      * @param optWidget Some(widget) if one is to be used, else None
      * @param emitFunc A function that emits the low-level IR
      * @param st A GenState object
      */
    private def generateUnaryOp(dest: VirtualReg,
                                src: VirtualReg,
                                optWidget: Option[Widget],
                                emitFunc: (PhysicalReg, PhysicalReg) => Unit)
                               (using st: GenState): Unit = {

        /* Enable a widget if provided with one */
        optWidget.foreach(useWidget)

        /* Get the source and destination locations (stack/reg) */
        val srcLoc = locOf(src)
        val destLoc = locOf(dest)
        
        /* Load source operand */
        val srcReg = loadToReg(srcLoc, src.size, PhysicalReg.ipcScratch(0))
        
        /* Determine result register */
        val destReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch(1)
        }
        
        /* Emit the unary operation specific low level IR */
        emitFunc(destReg, srcReg)
        
        /* Store result back to its original location */
        storeToLoc(destReg, destLoc, dest.size)
    }

    /**
      * Generate ARM instructions for a single high-level instruction.
      * Pattern matches on all instruction types and delegates to specific handlers.
      */
    private def generateInstr(instr: Instruction)(using genState: GenState): Unit = (instr: @unchecked) match {
        /* Data movement */
        
        case Mov(dest, src) => generateMov(dest, src)
        case Load(dest, addr) => ??? // TODO
        case Store(src, addr) => ??? // TODO
        
        /* Arithmetic operations */
        case Add(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmAddReg.apply, ArmAddImm.apply)
        case Sub(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmSubReg.apply, ArmSubImm.apply)
        case Mul(dest, src1, src2) => generateMul(dest, src1, src2)
        case Div(dest, src1, src2) => generateDivMod(dest, src1, src2, isMod = false)
        case Mod(dest, src1, src2) => generateDivMod(dest, src1, src2, isMod = true)
        case Neg(dest, src) => generateNeg(dest, src)
        
        /* Bitwise operations */
        case And(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmAndReg.apply, ArmAndImm.apply)
        case Or(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmOrReg.apply, ArmOrImm.apply)
        case Not(dest, src) => 
            generateNot(dest, src)

        case Lsl(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmLslReg.apply, ArmLslImm.apply)
        case Lsr(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmLsrReg.apply, ArmLsrImm.apply)
        case Asr(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmAsrReg.apply, ArmAsrImm.apply)
        
        /* Comparison and conditional */
        case Cmp(src1, src2) => generateCmp(src1, src2)
        case SetCond(dest, cond) => generateSetCond(dest, cond)
        
        /* Control flow */
        case BranchUncond(label) => emit(ArmBranchUncond(ArmAddrLabel(label.name)))
        case BranchCond(cond, label) => emit(ArmBranchCond(cond, ArmAddrLabel(label.name)))
        case LabelDef(label) => emit(ArmLabelDef(ArmAddrLabel(label.name)))
        
        /* Function calls */
        case Call(label, args, destOpt) => generateCall(label, args, destOpt)
        case Ret => generateReturn()
        
        /* I/O operations */
        case Print(src, widget, newline) => generatePrint(src, widget, newline)
        case Read(dest, widget) => generateRead(dest, widget)

        /* Character functions */
        case Chr(dest, src, widget) => generateChr(dest, src, widget)

        /* Stack operations */
        case Push(src) => ??? // TODO
        case Pop(dest) => ??? // TODO
        
        /* Array operations */
        case ArrayLoad(array, index, dest) => ??? // TODO
        case ArrayStore(array, index, src) => ??? // TODO
        
        /* Pair operations */
        case PairLoad(pair, index, dest) => ??? // TODO
        case PairStore(pair, index, src) => ??? // TODO
        
        /* Memory management */
        case HeapAlloc(dest, size) => ??? // TODO
        
        /* Special operations */
        case Lea(dest, label) => ??? // TODO
        case SignExtend(dest, src) => ??? // TODO
    }

    /**
      * Generate the low level IR for a MOV high level IR instruction
      *
      * @param dest The destination virtual address of the MOV
      * @param src The source virtual address of the MOV
      */
    private def generateMov(dest: VirtualReg, src: Operand)(using GenState) = {

        /* Get the source value in a scratch register to avoid memory to memory moves */
        val tempReg = src match {

            /* Move from stack to scratch register (or return the physical register if it
               is already in one) */
            case srcVReg: VirtualReg =>
                loadToReg(locOf(srcVReg), dest.size, PhysicalReg.ipcScratch(0))

            /* Move an immediate to scratch */
            case Immediate(value) =>
                val scratch = PhysicalReg.ipcScratch(0)
                emit(ArmMovImm(dest.size, scratch, ArmImmediate(value))) 
                scratch
            
            /* Load the memory address into scratch using lea */
            case Label(name) =>
                val scratch = PhysicalReg.ipcScratch(0)
                emit(ArmLea(scratch, ArmAddrLabel(name)))
                scratch
        }

        /* Store `src` in the destination location (register/stack) */
        storeToLoc(tempReg, locOf(dest), dest.size)
    }

    /**
      * Generate low-level IR for a Print instruction.
      * Registers the widget and delegates to generateCall for the actual call sequence.
      * Optionally emits a newline via PrintlnWidget.
      *
      * @param src the operand to print
      * @param widget the print widget for the appropriate type
      * @param newline whether to also emit a newline after printing
      */
    private def generatePrint(src: Operand, widget: Widget, newline: Boolean)
                             (using st: GenState): Unit = {
        useWidget(widget)
        generateCall(Label(widget.label), List(src), None)

        if (newline) {
            useWidget(PrintlnWidget)
            generateCall(Label(PrintlnWidget.label), List(), None)
        }
    }

    /**
      * Generate the low level IR for a Read high level IR instruction.
      *
      * @param dest The destination virtual register to read into
      * @param widget The read widget to use
      * @param st The generator state
      */
    private def generateRead(dest: VirtualReg, widget: Widget)
                            (using st: GenState): Unit = {
        useWidget(widget)
        generateCall(Label(widget.label), List(dest), Some(dest))
    }

    /**
      * Generates the low level IR for a `Chr` high level IR node
      *
      * @param src
      * @param widget
      * @param st
      */
    private def generateChr(dest: VirtualReg, src: VirtualReg, widget: Widget)
                           (using st: GenState): Unit = {

        generateUnaryOp(dest, src, Some(widget), {(s, d) => {
            val maxValidChar = 127
            /* Move the operand into w0 for the error function */
            emit(ArmMovReg(Size.W, PhysicalReg.argRegisters(0), s))

            /* If the int value is not 0-127 inclusive, runtime error */
            emit(ArmCmpImm(Size.W, s, ArmImmediate(maxValidChar)))
            emit(ArmBranchCond(Condition.HI, ArmAddrLabel(widget.label)))

            if (d != s) {
                emit(ArmMovReg(Size.W, d, s))
            }
        }})
    }

    /**
      * Align an integer number of bytes to 16 bytes for stack pushing
      *
      * @param numBytes The number of bytes to push
      * @return The 16-byte aligned number of bytes to push
      */
    private def alignBytes(numBytes: Int): Int = {
        if (numBytes % 16 == 0) numBytes 
        else numBytes + (16 - (numBytes % 16))
    }

    /**
      * Generate the low level IR for a Call high level IR instruction
      *
      * @param label The label of the function
      * @param args The argument operands to the function
      * @param destOpt The register that the return value is expected
      */
    private def generateCall(label: Label,
                             args: List[Operand],
                             destOpt: Option[VirtualReg])
                             (using GenState) = {

        val stackArgs = args.drop(8)
        val regArgs = args.take(8)

        /* Allocate stack space (moving stack pointer) to accomodate arguments
            beyond the first 8 which must live on the stack.
            
            we use a byte per arg because when calling CLib funcs this is a requirement
            and also byte alignment is faster for hardware.
        */
        val unalignedStackBytes = stackArgs.length * 8 
        val stackBytesAdded = alignBytes(unalignedStackBytes)
        emit(ArmSubImm(Size.X,
                        PhysicalReg.stackPointer,
                        PhysicalReg.stackPointer,
                        ArmImmediate(stackBytesAdded)))

        /* Space is now allocated, so store excess arguments on the stack */
        stackArgs.zipWithIndex.foreach { (arg, i) =>

            val size = operandSize(arg)
            val offset = i * 8 /* offset from the stack pointer */

            /* Put the argument in a scratch register */
            val valReg = arg match {
                case v: VirtualReg => loadToReg(locOf(v), size, PhysicalReg.ipcScratch(0))
                case Immediate(value) => 
                    emit(ArmMovImm(size, PhysicalReg.ipcScratch(0), ArmImmediate(value)))
                    PhysicalReg.ipcScratch(0)
                case Label(name) => 
                    emit(ArmLea(PhysicalReg.ipcScratch(0), ArmAddrLabel(name)))
                    PhysicalReg.ipcScratch(0)
            }

            /* Store the argument (in scratch) on the stack */
            emit(ArmStore(size,
                          valReg, 
                          ArmAddrImmOffset(PhysicalReg.stackPointer,
                                           ArmImmediate(offset))))
        }

        /* For each function of the register arguments */
        regArgs.zipWithIndex.foreach { case (arg, index) =>

            /* Get the physical register of the current argument */
            val targetReg = PhysicalReg.argRegisters(index)
            
            /* Get the value in the target register */
            arg match {
                case argVReg: VirtualReg =>
                    /* Move out of a stack slot into a scratch register */
                    val valReg = loadToReg(locOf(argVReg), Size.W, PhysicalReg.ipcScratch(0))
                    
                    /* Move the ith argument into the wi */
                    if (valReg != targetReg) {
                        emit(ArmMovReg(Size.W, targetReg, valReg))
                    }
                
                case Immediate(value) =>
                    /* Move the immediate directly to the argument register */
                    emit(ArmMovImm(Size.W, targetReg, ArmImmediate(value)))
                
                case Label(name) => {
                    /* Move the label (string directive in this case) to
                       the argument register */
                    emit(ArmLea(targetReg, ArmAddrLabel(name)))
                }
            }
        }

        /* Branch to the function */
        emit(ArmBl(ArmAddrLabel(label.name)))

        /* Post function call, we restore the stack pointer (collapse the frame) */
        if (stackBytesAdded > 0) {
            emit(ArmAddImm(Size.X, 
                           PhysicalReg.stackPointer, 
                           PhysicalReg.stackPointer, 
                           ArmImmediate(stackBytesAdded)))
        }

        /* Move the function return value to the expected register */
        destOpt.foreach { dest =>
            val resultReg = PhysicalReg.argRegisters(0) 
            storeToLoc(resultReg, locOf(dest), dest.size)
        }
    }

    /**
      * Generate the low level IR for a returns statement
      *
      * @param genState The current function scope code generator state
      */
    private def generateReturn()(using genState: GenState) = {
        val optEpilogueLabel = genState.currEpilogueLabel
        optEpilogueLabel.foreach(l => emit(ArmBranchUncond(l)))
    }

    /**
      * Generate low-level IR for a Cmp instruction.
      * Compares two operands by loading them into registers and emitting ArmCmpReg or ArmCmpImm.
      * 
      * @param src1 the first source operand (left side of comparison)
      * @param src2 the second source operand (right side of comparison)
      */
    private def generateCmp(src1: VirtualReg, src2: Operand)(using st: GenState): Unit = {
        val src1Loc = locOf(src1)
        
        /* Load first operand into a register */
        val src1Reg = loadToReg(src1Loc, src1.size, PhysicalReg.ipcScratch(0))

        /* Handle second operand and emit comparison */
        (src2: @unchecked) match {
            case src2VReg: VirtualReg =>
                /* Register-register comparison */
                val src2Loc = locOf(src2VReg)
                val src2Reg = loadToReg(src2Loc, src1.size, PhysicalReg.ipcScratch(1))
                emit(ArmCmpReg(src1.size, src1Reg, src2Reg))

            case Immediate(value) =>
                /* Register-immediate comparison */
                emit(ArmCmpImm(src1.size, src1Reg, ArmImmediate(value)))
        }
    }

    /**
      * Generate low-level IR for a SetCond instruction.
      * 
      * @param dest the destination virtual register
      * @param cond the condition to test
      */
    private def generateSetCond(dest: VirtualReg, cond: Condition)(using st: GenState): Unit = {
        val destLoc = locOf(dest)
        
        /* Determine result register */
        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch(0)
        }
        
        /* Emit conditional set instruction */
        emit(ArmCSet(dest.size, resultReg, cond))
        
        /* Store result */
        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
      * Generate low-level IR for boolean NOT.
      * 
      * @param dest the destination virtual register
      * @param src the source virtual register to not
      */
    private def generateNot(dest: VirtualReg, src: VirtualReg)(using st: GenState): Unit = {
        generateUnaryOp(dest, src, None, { (destReg, srcReg) =>

            /* Compare with 0 and set to 1 if equal (i.e. invert the boolean) */
            emit(ArmCmpImm(src.size, srcReg, ArmImmediate(0)))
            emit(ArmCSet(dest.size, destReg, Condition.EQ))
        })
    }

    /**
      * Generate the low level IR for a negation (-x)
      *
      * @param dest the destination virtual register
      * @param src the source virtual register to not
      */
    private def generateNeg(dest: VirtualReg, src: VirtualReg)(using st: GenState): Unit = {
        generateUnaryOp(dest, src, None, { (destReg, srcReg) =>
            emit(ArmNeg(dest.size, destReg, srcReg))
        })
    }
}
