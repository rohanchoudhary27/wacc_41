package wacc.backend

import scala.collection.mutable.ListBuffer
import wacc.backend.RegisterAllocator.AllocationResult
import wacc.ExitCodes.*
import wacc.backend.TAC
import wacc.backend.Arm

/**
  * Global context shared across all function code generation.
  * Manages label generation and data section directives.
  */
class GlobalGenContext {
    private var labelCounter = 0
    private val dataDirectives = ListBuffer[Arm.DataDirective]()
    
    /** Get the final directive list */
    def finalDirectives: List[Arm.DataDirective] = dataDirectives.toList

    /** Widget management */
    val dispatcher = RuntimeLibraryDispatcher()

    /** Generate a fresh unique label for compiler-generated code */
    def freshLabel(): Arm.AddrLabel = {
        val l = s".L$labelCounter"
        labelCounter += 1
        Arm.AddrLabel(l)
    }

    /**
      * Add a string constant to the data directives
      *
      * @param labelName The name of the label for the string to be stored
      * @param str The string to store
      * @return An `Arm.AddrLabel` to the directive
      */
    def addStringConstant(labelName: String, str: String): Arm.AddrLabel = {
        labelCounter += 1
        val label = Arm.AddrLabel(labelName)
        dataDirectives += Arm.DataDirective(label, str)
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
class GenState(val global: GlobalGenContext, val alloc: Map[TAC.VirtualReg, Location]) {

    /* The current return label to branch to at the end of the current function */
    var currEpilogueLabel: Option[Arm.AddrLabel] = None

    /* Program instructions (.text) */
    private val instrs = ListBuffer[Arm.Instruction]()
    
    /** Get the final instruction list */
    def finalInstrs: List[Arm.Instruction] = instrs.toList

    /** Add an instruction to the output */
    def +=(instr: Arm.Instruction): Unit = instrs += instr

    /** Generate a fresh unique label */
    def freshLabel(): Arm.AddrLabel = global.freshLabel()
}

/**
  * Code generator for translating High-Level IR to Low-Level IR.
  * 
  * Register allocation happens externally and is passed in via the alloc parameter.
  */
object CodeGenerator {

    /** Emit a single instruction to the output */
    private def emit(instr: Arm.Instruction)(using st: GenState): Unit = 
        st += instr
    
    /** Look up the location of a virtual register */
    private def locOf(vReg: TAC.VirtualReg)(using st: GenState): Location = 
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
        offset >= AARch64Limits.ARM_IMMEDIATE_SIGNED_OFFSET_LOW 
            && offset<= AARch64Limits.ARM_IMMEDIATE_SIGNED_OFFSET_HIGH

    /**
      * Helper to get the set of callee-saved registers (x19-x28) used in a function.
      * We only need to save the ones we actually assign
      */
    private def getUsedCalleeSaved(alloc: Map[TAC.VirtualReg, Location]): List[PhysicalReg] = {
        val used = alloc.values.collect { 
            case r: PhysicalReg if PhysicalReg.calleeSaved.contains(r) => r 
        }.toSet

        used.toList.sortBy(_.index)
    }

    /**
      * Helper to calculate the size of the local variables area.
      * Scans for the lowest StackSlot offset and aligns the size to 16 bytes.
      */
    private def getLocalsSize(alloc: Map[TAC.VirtualReg, Location]): Int = {
        val minOffset = alloc.values.collect { 
            case StackSlot(offset) => offset 
        }.minOption.getOrElse(0)
        
        /* offsets are negative so size is abs(minOffset) */
        val size = Math.abs(minOffset)
        
        /* 16 byte alignment */
        alignBytes(size)
    }

    /**
     * Emits a null pointer check for a given register.
     * Jumps to the `NullDerefErrorWidget`'s label if the pointer is null.
     * 
     * @param ptrReg the register to check
     */ 
    private def nullCheck(ptrReg: PhysicalReg)(using st: GenState): Unit = {
        useWidget(NullDerefErrorWidget)
        /* emit the following assembly
            ```
            cmp ptrReg, #0
	        b.eq ${NullDerefErrorWidget.label}
            ```
            to check that the ptrReg is null or not */
        emit(Arm.CmpImm(Size.X, ptrReg, Arm.Immediate(MemoryLayout.NULL_PTR)))
        emit(Arm.BranchCond(Condition.EQ, Arm.AddrLabel(NullDerefErrorWidget.label)))
    }
    
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
                    emit(
                        Arm.Ldur(size, scratch, 
                            Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(offset))
                        )
                    )  
                } else { /* positive offset, use LDR */
                    emit(Arm.Load(
                        size, scratch, 
                        Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(offset))
                    ))
                }
                
            } else {
                /* Large offset, so we need to load the offset into x17, then `ldr [fp, x17]` */
                val offsetReg = PhysicalReg.ipcScratch2 // use x17
                generateMovImmediate(Size.X, offsetReg, Arm.Immediate(offset))
                emit(Arm.Load(size, scratch, Arm.AddrRegOffset(PhysicalReg.framePointer, offsetReg)))
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
            if (src != dest) emit(Arm.MovReg(size, dest, src))
        case StackSlot(offset) =>
            if (isSafeSignedOffset(offset)) {
                if (offset < 0) { /* negative offset, use STUR */
                    emit(Arm.Stur(
                        size, src, Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(offset))
                    ))
                } else { /* positive offset, use STR */
                    emit(Arm.Store(
                        size, src, Arm.AddrImmOffset(PhysicalReg.framePointer, Arm.Immediate(offset))
                    ))
                }
            } else {
                /* Large offset so scratch register needed but it must not be the
                   same as the source register else we will destroy it before it
                   can be stored on the stack. */
                val offsetReg = if (src == PhysicalReg.ipcScratch2) {
                    PhysicalReg.ipcScratch1
                } else {
                    PhysicalReg.ipcScratch2
                }

                generateMovImmediate(Size.X, offsetReg, Arm.Immediate(offset))
                emit(Arm.Store(
                    size, src, Arm.AddrRegOffset(PhysicalReg.framePointer, offsetReg)
                ))
            }
            
    }

    /**
      * Evaluates an operand and ensures its value is loaded into a specific 
      * physical target register. Emits a move instruction if the register allocator 
      * placed it elsewhere.
      *
      * @param op The source operand to evaluate
      * @param size The size of the operand
      * @param targetReg The required physical register destination
      * @param strict If true, forces the value exactly into `targetReg` (emitting a 
      *               move if needed). If false (default), simply returns the existing 
      *               physical register if it was already allocated.
      * 
      * @return The physical register that currently holds the evaluated operand
      */
    private def loadOperandToTarget(
        op: TAC.Operand, size: Size, targetReg: PhysicalReg, strict: Boolean = false
    )(using st: GenState): PhysicalReg = op match {
        case v: TAC.VirtualReg =>
            val loadedReg = loadToReg(locOf(v), size, targetReg)
            if (strict && loadedReg != targetReg) {
                emit(Arm.MovReg(size, targetReg, loadedReg))
                targetReg
            } else {
                loadedReg
            }
        case TAC.Immediate(value) =>
            generateMovImmediate(size, targetReg, Arm.Immediate(value))
            targetReg
        case TAC.Label(name) =>
            emit(Arm.Lea(targetReg, Arm.AddrLabel(name)))
            targetReg
    }

     /**
      * Align an integer number of bytes to 16 bytes for stack pushing
      *
      * @param numBytes The number of bytes to push
      * @return The 16-byte aligned number of bytes to push
      */
    private def alignBytes(numBytes: Int): Int = {
        if (numBytes % AARch64Limits.ARM_STACK_ALIGNMENT_BYTES == 0) numBytes 
        else numBytes + (
            AARch64Limits.ARM_STACK_ALIGNMENT_BYTES 
                - (numBytes % AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
        )
    }

    /**
      * Return the `Size` of an operand
      *
      * @param op The operand to get the size of
      * @return The register size of the opernad (Size.W / Size.X)
      */
    private def operandSize(op: TAC.Operand) = {
        op match {
            case v: TAC.VirtualReg => v.size
            case _: TAC.Immediate => Size.W
            case _: TAC.Label => Size.X
        }
    }

    /**
      * Helper function for binary operations that handles repeated logic of widget enabling,
      * register loading, and the `storeToLoc` call
      *
      * @param dest The destionation virtual register of the binary operation
      * @param src1 The first source register of the binary operation
      * @param optWidget None for on widget, Some(Widget) for a widget to enable
      * @param opBlock A function for emitting the instructions for the specific binary operation
      * @param st A GenState object
      */
    private def withBinaryContext(dest: TAC.VirtualReg, 
                                  src1: TAC.VirtualReg, 
                                  optWidget: Option[Widget])
                                 (opBlock: (PhysicalReg, PhysicalReg) => Unit)
                                 (using st: GenState): Unit = {

        /* Widget enabling */
        optWidget.foreach(useWidget)

        /* Setup registers */
        val destLoc = locOf(dest)
        val src1Reg = loadToReg(locOf(src1), dest.size, PhysicalReg.ipcScratch1)
        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch1
        }

        /* Emit the instruction for the binary operation */
        opBlock(resultReg, src1Reg)

        /* Store the binary operation result in the result register */
        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
      * Generate Low-Level IR for the program.
      * 
      * @param regProgram the high-level IR program
      * @param allocs the register allocation result containing maps for main and all functions
      * @return the complete ARM program with data section and text section
      */
    def generate(regProgram: TAC.RegProgram,
                 allocs: AllocationResult): (Arm.Program, RuntimeLibraryDispatcher) = {

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
        emit(Arm.LabelDef(Arm.AddrLabel("main")))

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
        generateMovImmediate(Size.W, PhysicalReg.returnRegister, Arm.Immediate(code(CompilerSuccess)))
        generateFuncEpilogue(localsSize, savedRegs)

        /* Final low-level IR for the program */
        val program = Arm.Program(globalCtx.finalDirectives, st.finalInstrs)

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
        emit(Arm.StorePair(Size.X,
                         PhysicalReg.framePointer,
                         PhysicalReg.linkReg,
                         Arm.AddrPreIndex(
                            PhysicalReg.stackPointer, 
                            Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
                        )))

        /* set FP to be SP */
        emit(Arm.MovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer))

        /* reserve space for locals (stack slots) */
        if (localsSize > 0) {
            /* sub sp, sp, <localsSize> */
            emit(Arm.SubImm(
                Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer, Arm.Immediate(localsSize)
            ))
        }

        /* push callee-saved registers in pairs */
        savedRegs.grouped(AARch64Limits.ARM_STACK_REG_PER_PUSH).foreach {
            case List(r1, r2) => /* do stp xi, x(i+1), [sp, #-16]! */
                emit(Arm.StorePair(
                    Size.X, r1, r2, Arm.AddrPreIndex(
                        PhysicalReg.stackPointer, 
                        Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
                    )
                ))
            case List(r1) => /* push 16 bytes for alignment */
                emit(Arm.Store(
                    Size.X, r1, Arm.AddrPreIndex(
                        PhysicalReg.stackPointer, 
                        Arm.Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
                    )
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
        savedRegs.grouped(AARch64Limits.ARM_STACK_REG_PER_PUSH).toList.reverse.foreach {
            case List(r1, r2) => 
                /* (r1, r2) was pushed in prologue, and stack top has r1 at low addr, r2 at high 
                    addr. LDP loads low addr into first register, high into second, so we load 
                    into r1, r2 */
                /* do ldp r1, r2, [sp], #16 */
                emit(
                    Arm.LoadPair(
                        Size.X, r1, r2, Arm.AddrPostIndex(
                            PhysicalReg.stackPointer, 
                            Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
                        )
                    )
                )
            case List(r1) =>
                emit(
                    Arm.Load(Size.X, r1, 
                        Arm.AddrPostIndex(
                            PhysicalReg.stackPointer, 
                            Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)
                        )
                    )
                )
            case _ =>
        }
        /* we have to free local variable space too */
        if (localsSize > 0) {
            emit(Arm.AddImm(Size.X, PhysicalReg.stackPointer, PhysicalReg.stackPointer, 
                           Arm.Immediate(localsSize)))
        }
        /* restore FP and LR */
        emit(
            Arm.LoadPair(
                Size.X, PhysicalReg.framePointer, PhysicalReg.linkReg, 
                Arm.AddrPostIndex(PhysicalReg.stackPointer, 
                    Arm.Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))
            )
        )
        emit(Arm.Ret)
    }

    /**
      * Generate Low-Level IR for a function definition.
      * 
      * @param func the function definition
      * @param alloc the register allocation map for this function
      * @param ctx the global context for labels and data directives
      * @return the low-level ARM instruction list
      */
    def generateFunc(func: TAC.FunctionDef,
                     alloc: Map[TAC.VirtualReg, Location],
                     ctx: GlobalGenContext): List[Arm.Instruction] = {

        given st: GenState = GenState(ctx, alloc)
        
        /* Function label */
        emit(Arm.LabelDef(Arm.AddrLabel(func.name)))

        /* calculate requirements for prologue/epilogue */
        val localsSize = getLocalsSize(alloc)
        val savedRegs = getUsedCalleeSaved(alloc)

        generateFuncPrologue(localsSize, savedRegs)

        /* Parameters are moved to their allocated registers (by the RegisterAllocator)
           because they're caller save. E.g. they'll be used if the function calls another
           function. */
        func.args.zipWithIndex.foreach { case (arg, index) =>
            if (index < AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS) {
                /* Parameter's assigned location by register allocator */
                val destLoc = alloc(arg)
                storeToLoc(PhysicalReg.argRegisters(index), destLoc, arg.size)
            }
        }

        /* Register the epilogue label */
        val epilogueLabel = Arm.AddrLabel(s".L_${func.name}_epilogue")
        st.currEpilogueLabel = Some(epilogueLabel)

        /* Generate function body */
        generateInstrs(func.body)

        /* Generate function eiplogue */
        emit(Arm.LabelDef(epilogueLabel))
        generateFuncEpilogue(localsSize, savedRegs)
        
        st.finalInstrs
    }
    
    /**
      * Generate ARM instructions for a list of high-level instructions.
      * Wrapper that dispatches each instruction to generateInstr.
      */
    private def generateInstrs(instrs: List[TAC.Instruction])(using st: GenState): Unit =
        instrs.foreach(generateInstr)

    /**
      * Generate ARM instructions for a single high-level instruction.
      * Pattern matches on all instruction types and delegates to specific handlers.
      */
    private def generateInstr(instr: TAC.Instruction)
                             (using genState: GenState): Unit = (instr: @unchecked) match {

        /* Data movement */
        case TAC.Mov(dest, src) => generateMov(dest, src)
        case TAC.Load(dest, addr) =>
            generateMemOp(dest, addr, Arm.Ldur.apply, Arm.Load.apply) {
                /* afterwards, store the loaded value into the destination register */
                storeToLoc(PhysicalReg.ipcScratch1, locOf(dest), dest.size)
            }
        case TAC.Store(src, addr) =>
            /* pre-load src into scratch0 */
            loadToReg(locOf(src), src.size, PhysicalReg.ipcScratch1)
            generateMemOp(src, addr, Arm.Stur.apply, Arm.Store.apply)()
        
        /* Arithmetic operations */
        case TAC.Add(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.AddsReg.apply, Arm.AddsImm.apply, Some(IntOverflowErrorWidget), {
                /* Post main operation instruction emission*/
                emit(Arm.BranchCond(Condition.VS, Arm.AddrLabel(IntOverflowErrorWidget.label)))
            })
        case TAC.Sub(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.SubsReg.apply, Arm.SubsImm.apply, Some(IntOverflowErrorWidget), {
                /* Post main operation instruction emission*/
                emit(Arm.BranchCond(Condition.VS, Arm.AddrLabel(IntOverflowErrorWidget.label)))
            })

        case TAC.Mul(dest, src1, src2) => generateMul(dest, src1, src2)
        case TAC.Div(dest, src1, src2) => generateDivMod(dest, src1, src2, isMod = false)
        case TAC.Mod(dest, src1, src2) => generateDivMod(dest, src1, src2, isMod = true)
        case TAC.Neg(dest, src) => generateNeg(dest, src)
        
        /* Bitwise operations */
        case TAC.And(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.AndReg.apply, Arm.AndImm.apply)
        case TAC.Or(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.OrReg.apply, Arm.OrImm.apply)
        case TAC.Not(dest, src) => 
            generateNot(dest, src)

        /* Shifts */
        case TAC.Lsl(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.LslReg.apply, Arm.LslImm.apply)
        case TAC.Lsr(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.LsrReg.apply, Arm.LsrImm.apply)
        case TAC.Asr(dest, src1, src2) => 
            generateStandardBinOp(dest, src1, src2, Arm.AsrReg.apply, Arm.AsrImm.apply)
        
        /* Comparison and conditional */
        case TAC.Cmp(src1, src2) => generateCmp(src1, src2)
        case TAC.SetCond(dest, cond) => generateSetCond(dest, cond)
        
        /* Control flow */
        case TAC.BranchUncond(label) => emit(Arm.BranchUncond(Arm.AddrLabel(label.name)))
        case TAC.BranchCond(cond, label) => emit(Arm.BranchCond(cond, Arm.AddrLabel(label.name)))
        case TAC.LabelDef(label) => emit(Arm.LabelDef(Arm.AddrLabel(label.name)))
        
        /* Function calls */
        case TAC.Call(label, args, destOpt) => generateCall(label, args, destOpt)
        case TAC.Ret => generateReturn()
        
        /* I/O operations */
        case TAC.Print(src, widget, newline) => generatePrint(src, widget, newline)
        case TAC.Read(dest, widget) => generateRead(dest, widget)

        /* Character functions */
        case TAC.Chr(dest, src, widget) => generateChr(dest, src, widget)
        
        /* Array operations */
        case TAC.ArrayLoad(array, index, dest) => 
            generateArrayLoad(array, index, dest)
        case TAC.ArrayStore(array, index, src, elemSize) => 
            generateArrayStore(array, index, src, elemSize)
        case TAC.ArrayLen(array, dest) => generateArrayLen(array, dest)
        
        /* Pair operations */
        case TAC.PairLoad(pair, index, dest) => generatePairLoad(pair, index, dest)
        case TAC.PairStore(pair, index, src) => generatePairStore(pair, index, src)
        case TAC.NullCheck(ptr) => generateNullCheck(ptr)
        
        /* Memory management */
        case TAC.HeapAlloc(dest, size) => generateHeapAlloc(dest, size)
        case TAC.Free(ptr, freeType) => generateFree(ptr, freeType)
        
        /* Special operations */
        case TAC.SignExtend(dest, src) => generateSignExtend(dest, src)
    }

    /**
      * Generate the low level IR for simple binary operations (Add, Sub, boolean, shifts) where
      * simple means they either have simple VS flag overflow checks or no overflow check at all
      *
      * @param dest The destination virtual register
      * @param src1 The first source register
      * @param src2 The second source register
      * @param regOp The register to register constructor function for the binary operation
      * @param immOp The register to register constructor function for the binary operation
      * @param optWidget None for no widget, Some(Widget) for a widget to be enabled
      * @param postOp A function to emit (optional) instructions after the simple binary
      * operation constructor (e.g. used for VS overflow flag branching)
      * @param st A GenState object
      */
    private def generateStandardBinOp(
        dest: TAC.VirtualReg,
        src1: TAC.VirtualReg,
        src2: TAC.Operand,
        regOp: (Size, PhysicalReg, PhysicalReg, PhysicalReg) => Arm.Instruction,
        immOp: (Size, PhysicalReg, PhysicalReg, Arm.Immediate) => Arm.Instruction,
        optWidget: Option[Widget] = None,
        postOp: => Unit = ()
    )(using st: GenState): Unit = {
        
        /* Enable widget */
        optWidget.foreach(useWidget)
        
        /* Setup registers */
        val destLoc = locOf(dest)
        val src1Reg = loadToReg(locOf(src1), dest.size, PhysicalReg.ipcScratch1)
        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch1
        }
        
        /* Generate the register-register / register-immediate instruction */
        (src2: @unchecked) match {
            case src2VReg: TAC.VirtualReg =>
                val src2Reg = loadToReg(locOf(src2VReg), dest.size, PhysicalReg.ipcScratch2)
                emit(regOp(dest.size, resultReg, src1Reg, src2Reg))

            case TAC.Immediate(value) =>
                emit(immOp(dest.size, resultReg, src1Reg, Arm.Immediate(value)))
        }

        /* Execute any post-instruction logic */
        postOp

        /* Store final result to destination register */
        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
      * Helper function for loading and storing to registers for complex binary operations
      * (Mul, Div, Mod) - complex in the sense of overflow checking not via the VS flag
      *
      * @param dest The destination virtual register
      * @param src1 The first source register
      * @param src2 The second source register
      * @param optWidget None for no widget, Some(Widget) for a widget to be enabled
      * @param opBlock The instruction emission function
      * @param st A GenState object
      */
    private def generateComplexBinOp(dest: TAC.VirtualReg,
                                     src1: TAC.VirtualReg,
                                     src2: TAC.Operand,
                                     optWidget: Option[Widget])
                                    (opBlock: (PhysicalReg, PhysicalReg, PhysicalReg) => Unit)
                                    (using st: GenState): Unit = {

        /* Binary context handles  register loading, storing and widget */
        withBinaryContext(dest, src1, optWidget) { (resultReg, src1Reg) =>
        
            /* Load the second register */
            val src2Reg = src2 match {
                case v: TAC.VirtualReg => loadToReg(locOf(v), dest.size, PhysicalReg.ipcScratch2)
                case TAC.Immediate(v) => 
                    val temp = PhysicalReg.ipcScratch2
                    generateMovImmediate(dest.size, temp, Arm.Immediate(v))
                    temp
                case TAC.Label(name) => 
                    val temp = PhysicalReg.ipcScratch2
                    emit(Arm.Lea(temp, Arm.AddrLabel(name)))
                    temp
            }

            /* Operation specific instruction emission */
            opBlock(resultReg, src1Reg, src2Reg)
        }

    }

    /**
     * Generate low-level AArch64 IR for mutliplication with integer overflow checking
     * 
     * @param dest: destination virtual register to store result to
     * @param src1: first source virtual register
     * @param src1: second operand (can be an immediate  or virtual register)
     */
    private def generateMul(dest: TAC.VirtualReg,
                            src1: TAC.VirtualReg,
                            src2: TAC.Operand)
                           (using st: GenState): Unit = {

        /* Binary context handles  register loading, storing and widget */
        generateComplexBinOp(dest, src1, src2, Some(IntOverflowErrorWidget)) {
            (resultReg, src1Reg, src2Reg) =>

                /* Take two 32 bit (w) numbers and multiply, store as a 64 bit (x) register */
                emit(Arm.SMull(resultReg, src1Reg, src2Reg))

                /* Take lower 32 bits of result and sign extend to 64 into x8.
                   If no overflow then the 32-bit version will be identical
                   to the 64bit SMULL result. */
                val scratch3 = PhysicalReg.mathScratch
                emit(Arm.Sxtw(scratch3, resultReg))
                emit(Arm.CmpReg(Size.X, resultReg, scratch3))

                /* If comparison not equal then an overflow occured, so branch to error widget */
                emit(Arm.BranchCond(Condition.NE, Arm.AddrLabel(IntOverflowErrorWidget.label)))
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
    private def generateDivMod(dest: TAC.VirtualReg,
                               src1: TAC.VirtualReg,
                               src2: TAC.Operand,
                               isMod: Boolean)
                              (using st: GenState): Unit = {

        /* Binary context handles  register loading, storing and widget */
        generateComplexBinOp(dest, src1, src2, Some(DivByZeroErrorWidget)) {
            (resultReg, src1Reg, src2Reg) => 
                /* Divide by zero check, if so jump to divide by zero widget */
                emit(Arm.Cbz(dest.size, src2Reg, Arm.AddrLabel(DivByZeroErrorWidget.label)))

                /* Use flag to pick either integer division or modulo */
                if (isMod) {
                    /* Calculate modulo using integer division */
                    val scratch3 = PhysicalReg.mathScratch
                    emit(Arm.DivReg(dest.size, scratch3, src1Reg, src2Reg))
                    emit(Arm.MSub(dest.size, resultReg, scratch3, src2Reg, src1Reg))
                    /* Calculate integer division */
                } else {
                    emit(Arm.DivReg(dest.size, resultReg, src1Reg, src2Reg))
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
    private def generateUnaryOp(dest: TAC.VirtualReg,
                                src: TAC.VirtualReg,
                                optWidget: Option[Widget],
                                emitFunc: (PhysicalReg, PhysicalReg) => Unit)
                               (using st: GenState): Unit = {

        /* Enable a widget if provided with one */
        optWidget.foreach(useWidget)

        /* Get the source and destination locations (stack/reg) */
        val srcLoc = locOf(src)
        val destLoc = locOf(dest)
        
        /* Load source operand */
        val srcReg = loadToReg(srcLoc, src.size, PhysicalReg.ipcScratch1)
        
        /* Determine result register */
        val destReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch2
        }
        
        /* Emit the unary operation specific low level IR */
        emitFunc(destReg, srcReg)
        
        /* Store result back to its original location */
        storeToLoc(destReg, destLoc, dest.size)
    }

    /**
      * Generates low level IR to allocate heap memory.
      * 
      * Moves the requested size into the first argument register (`x0`), emits a `bl`
      * to `_malloc` widget, and saves the returned memory pointer from `x0` (which is the address
      * of the malloced heap space) into the specified destination virtual register.
      *
      * @param dest the destination virtual register to store the allocated heap pointer
      * @param size the operand specifying the number of bytes to allocate
      */
    private def generateHeapAlloc(dest: TAC.VirtualReg,
                                  size: TAC.Operand)
                                 (using st: GenState): Unit = {
        val widget = MallocWidget
        useWidget(widget)
        /* size goes into x0, dest is returned */
        generateCall(TAC.Label(widget.label), List(size), Some(dest))
    }

    /**
      * Generates the low-level IR for a `Free` instruction to deallocate heap memory.
      *
      * This method uses the memory freeing widget `FreePairWidget` and emits a call to 
      * it, passing the target pointer in the first argument register. The widget itself is 
      * responsible for triggering a runtime error (`_errNull`) if the pointer is null, and 
      * calling the C standard library `free` function to deallocate the memory.
      * 
      * Only deallocates the outermost pointer, inner nested structures are not recursively freed.
      *
      * @param ptr The virtual register containing the 64 bit pointer to be freed.
      */
    private def generateFree(ptr: TAC.VirtualReg, freeType: FreeType)(using st: GenState): Unit = {
        val widget = freeType match {
            case FreeType.Pair => FreePairWidget
            case FreeType.Array => FreeArrayWidget
        }
        useWidget(widget)
        generateCall(TAC.Label(widget.label), List(ptr), None)
    }

    /**
      * Generates the low-level IR for a sign extension instruction used to go from a smaller
      * register in the high level IR to a larger one e.g. a B register to a W register.
      *
      * @param dest The destination register for the sign extension (larger size)
      * @param src The source register for the sign extension (smaller size) 
      * @param st The generator state
      */
    private def generateSignExtend(dest: TAC.VirtualReg,
                                   src: TAC.VirtualReg)
                                  (using st: GenState): Unit = {
        val srcLoc = locOf(src)
        val destLoc = locOf(dest)

        /* Load smaller register (source) to scratch. Passing 'src.size' means we load
           into a register with the smaller size */
        val srcReg = loadToReg(srcLoc, src.size, PhysicalReg.ipcScratch1)

        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch2
        }

        /* Move to the larger register so we use dest.size */
        if (srcReg != resultReg) {
            emit(Arm.MovReg(dest.size, resultReg, srcReg))
        }

        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
      * Generates low level IR to safely load a value from a heap allocated pair
      * 
      * Loads the pair's base pointer into a scratch register (`x16`).
      * Determines the exact memory offset (0 bytes for `fst`, 8 bytes for `snd`) and loads 
      * the 8 byte slot into a second scratch register (`x17`) before moving it to the destination.
      *
      * @param pairPtr the virtual register holding the base pointer of the pair
      * @param pos the position within the pair (`fst` or `snd`) to read from
      * @param dest the destination virtual register to store the loaded value
      */
    private def generatePairLoad(pairPtr: TAC.VirtualReg, pos: PairPos, dest: TAC.VirtualReg)
                                (using st: GenState): Unit = {
        /* load the base pointer into strach register x16 */
        val ptrReg = loadToReg(locOf(pairPtr), Size.X, PhysicalReg.ipcScratch1)
        
        val offset = pos match {
            case PairPos.Fst => 0
            case PairPos.Snd => MemoryLayout.PAIR_SLOT_SIZE
        }
        
        /* we first load the memory address (ptrReg + offset) into a scratch register x17, then
            load that to the destination location because AArch64 forbids direct memory to memory 
            transfers (if dest is a memory location on the stack).

            also note that we load the full 8-byte pair slot (pair slots are always 8 bytes) from
            the heap into the 64 bit (corrsponds to Size.X) scratch register */
        val destScratch = PhysicalReg.ipcScratch2
        emit(Arm.Load(Size.X, destScratch, Arm.AddrImmOffset(ptrReg, Arm.Immediate(offset))))
        storeToLoc(destScratch, locOf(dest), dest.size)
    }

    /**
      * Generates low level IR to check if a pointer is null
      */
    private def generateNullCheck(ptr: TAC.VirtualReg)(using st: GenState): Unit = {
        // Load the pointer into a scratch register
        val ptrReg = loadToReg(locOf(ptr), Size.X, PhysicalReg.ipcScratch1)
        
        nullCheck(ptrReg) 
    }

    /**
      * Generates low level IR to safely store a value into a heap allocated pair.
      * 
      * Loads the pair's base pointer into a scratch register (`x16`).
      * Transfers the source operand into a 64-bit scratch register (`x17`), calculates the
      * memory offset (0 for `Fst`, 8 for `Snd`), and emits the store instruction to overwrite
      * the heap memory.
      *
      * @param pairPtr the virtual register holding the base pointer of the pair
      * @param pos the position within the pair (`Fst` or `Snd`) to write to
      * @param src the source operand containing the value to store
      */
    private def generatePairStore(pairPtr: TAC.VirtualReg, pos: PairPos, src: TAC.Operand)
                                 (using st: GenState): Unit = {
        /* load the base pointer into strach register x16 */
        val ptrReg = loadToReg(locOf(pairPtr), Size.X, PhysicalReg.ipcScratch1)
        
        val offset = pos match {
            case PairPos.Fst => 0
            case PairPos.Snd => MemoryLayout.PAIR_SLOT_SIZE
        }
        
        /* Load the source value into x17 (scratch register) (see the comment in generatePairLoad)
            about why we store this in a scratch register first. */
        val srcScratch = PhysicalReg.ipcScratch2
        val finalSrcReg = loadOperandToTarget(src, Size.X, srcScratch)
        
        /* Store the 8 byte scrath register into heap memory */
        emit(Arm.Store(Size.X, finalSrcReg, Arm.AddrImmOffset(ptrReg, Arm.Immediate(offset))))
    }

    /**
      * Generates low level IR for a generic memory operation (Load or Store).
      * Handles address resolution and base register loading.
      *
      * @param reg the virtual register to load into or store from
      * @param addr the memory address
      * @param immOp constructor for immediate-offset memory op
      * @param regOp constructor for register-offset memory op
      * @param postOp logic to run after the memory operation
      */
    private def generateMemOp(
        reg: TAC.VirtualReg,
        addr: TAC.Address,
        immOp: (Size, PhysicalReg, Arm.Address) => Arm.Instruction,
        regOp: (Size, PhysicalReg, Arm.Address) => Arm.Instruction,
    )(postOp: => Unit = ())(using st: GenState): Unit = {
        val scratch0 = PhysicalReg.ipcScratch1

        addr match {
            case TAC.AddrImmOffset(base, TAC.Immediate(offset)) =>
                val baseReg = loadToReg(locOf(base), Size.X, PhysicalReg.ipcScratch2)
                emit(immOp(reg.size, scratch0, Arm.AddrImmOffset(baseReg, Arm.Immediate(offset))))
            case TAC.AddrRegOffset(base, offsetReg) =>
                val baseReg = loadToReg(locOf(base), Size.X, PhysicalReg.ipcScratch2)
                val offReg = loadToReg(locOf(offsetReg), Size.X, PhysicalReg.ipcScratch2)
                emit(regOp(reg.size, scratch0, Arm.AddrRegOffset(baseReg, offReg)))
            case _ => ???
        }

        postOp
    }

    /* Retrieves the appropriate array runtime widget based on size and operation. */
    private def getArrayWidget(elemSize: Size, isStore: Boolean): Widget = (isStore, elemSize) match {
        case (false, Size.B) => ArrayLoad1Widget
        case (false, Size.W) => ArrayLoad4Widget
        case (false, Size.X) => ArrayLoad8Widget
        case (true, Size.B)  => ArrayStore1Widget
        case (true, Size.W)  => ArrayStore4Widget
        case (true, Size.X)  => ArrayStore8Widget
    }

    /* Loads the array pointer into x7, the index into x17, and calls the appropriate widget. */
    private def emitArrayWidgetCall(array: TAC.VirtualReg, index: TAC.Operand, elemSize: Size, isStore: Boolean)
                                   (using st: GenState): Unit = {
        val arrayPtrReg = PhysicalReg.widgetResultRegister  // x7
        val indexReg = PhysicalReg.ipcScratch2       // x17

        /* load operands into the shared calling convention registers */
        loadOperandToTarget(array, Size.X, arrayPtrReg, strict = true)
        loadOperandToTarget(index, Size.W, indexReg, strict = true)

        /* retrieve, register, and call the correct widget */
        val widget = getArrayWidget(elemSize, isStore)
        useWidget(widget)
        emit(Arm.Bl(Arm.AddrLabel(widget.label)))
    }

    /**
      * Generates low level IR for an ArrayLoad instruction.
      * Uses the appropriate runtime widget based on the destination register size.
      *
      * Calling convention: array ptr in X7, index in X17, result returned in X7.
      * 
      * @param array the virtual register holding the array pointer
      * @param index the operand specifying the index to load from
      * @param dest the destination virtual register to store the loaded value
      */
    private def generateArrayLoad(array: TAC.VirtualReg, index: TAC.Operand, dest: TAC.VirtualReg)
                                 (using st: GenState): Unit = {
        emitArrayWidgetCall(array, index, dest.size, isStore = false)
        
        /* Result is in X7, move to destination */
        storeToLoc(PhysicalReg.widgetResultRegister, locOf(dest), dest.size)
    }

    /**
      * Generates low level IR for an ArrayStore instruction.
      * Uses the appropriate runtime widget based on the element size.
      *
      * Calling convention: array ptr in X7, index in X17, value in X8.
      */
    private def generateArrayStore(array: TAC.VirtualReg, index: TAC.Operand, src: TAC.Operand, elemSize: Size)
                                  (using st: GenState): Unit = {
        val valueReg = PhysicalReg.mathScratch // x8
        loadOperandToTarget(src, elemSize, valueReg, strict = true)
        
        /* setup x7, x17, and call the store widget */
        emitArrayWidgetCall(array, index, elemSize, isStore = true)
    }

    /**
      * Generates low level IR to load array length from header.
      * 
      * Loads the array pointer, then loads the 4-byte length field from offset -4
      * (the array pointer points to the first element, length is stored before it).
      *
      * @param arrayPtr the virtual register holding the pointer to first array element
      * @param dest the destination virtual register to store the length
      */
    private def generateArrayLen(arrayPtr: TAC.VirtualReg, dest: TAC.VirtualReg)
                                (using st: GenState): Unit = {
        /* load the array pointer into scratch register x16 */
        val ptrReg = loadToReg(locOf(arrayPtr), Size.X, PhysicalReg.ipcScratch1)
        
        /* load the length from offset -4 */
        val destScratch = PhysicalReg.ipcScratch2
        emit(Arm.Ldur(Size.W, destScratch, Arm.AddrImmOffset(ptrReg, Arm.Immediate(-4))))
        
        /* store to destination location */
        storeToLoc(destScratch, locOf(dest), dest.size)
    }

    /**
      * Generate the low level IR for a MOV high level IR instruction
      *
      * @param dest The destination virtual address of the MOV
      * @param src The source virtual address of the MOV
      */
    private def generateMov(dest: TAC.VirtualReg, src: TAC.Operand)(using GenState) = {

        /* Get the source value in a scratch register to avoid memory to memory moves */
        val scratch = PhysicalReg.ipcScratch1
        val tempReg = loadOperandToTarget(src, dest.size, scratch)

        /* Store `src` in the destination location (register/stack) */
        storeToLoc(tempReg, locOf(dest), dest.size)
    }

    /**
      * Generates the low level IR for an immediate move. Handles moves for immediates that
      * don't fit in 16 bits by splitting the move in two with MOVK and MOVZ together.
      * 
      * This function fully replaces the old ArmMovImm low level IR so we now have no need for
      * it we can just use this function (which only uses Movk and Movz). Note that anyway Mov
      * in aarch64 was just an alias for Movk/Movz/Orr.
      *
      * @param dest The destination physical register of the move
      * @param immValue The immediate value to move
      * @param size The size of the destination register
      * @param st A GenState object
      */
    private def generateMovImmediate(
        size: Size, dest: PhysicalReg, immValue: Arm.Immediate
    )(using st: GenState): Unit = {

        val numericalValue = immValue.value
 
        /* If the value fits in 16 bits we can move it in one go. If it fits in 16 bits,
           it will be positive as negative numbers require 32 bits (wacc ints are 32 bit) */
        if (numericalValue >= 0 && numericalValue <= AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH) {
            emit(Arm.Movz(size, dest, Arm.Immediate(numericalValue)))
        } else {
            val lower16 = numericalValue & AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH
            val upper16 = (numericalValue >>> ShiftAmounts.value(ShiftAmounts._16)) &
                                              AARch64Limits.ARM_IMMEDIATE_UNSIGNED_HIGH

            /* If we're moving a negative immediate, we must move to a Size.W register since
               this means the negative bit is still the MSB. If we were to use `size`, it might
               be Size.X which would give leading 0s (since wacc ints fit in 32 bits only) which
               would make the value positive */
            val negAdjustedSize = if numericalValue < 0 then Size.W else size
            
            /* Move in two parts, using Movk to keep the lower half when
               moving the upper half in */
            emit(Arm.Movz(negAdjustedSize, dest, Arm.Immediate(lower16)))
            emit(Arm.Movk(negAdjustedSize, dest, Arm.Immediate(upper16), ShiftAmounts._16))

            /* If the negative immediate's destination was Size.X, then we now need to
               sign extend to leading 0s */
            if (numericalValue < 0 && size == Size.X) {
                emit(Arm.Sxtw(dest, dest))
            }
        }
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
    private def generatePrint(src: TAC.Operand, widget: Widget, newline: Boolean)
                             (using st: GenState): Unit = {
        useWidget(widget)
        generateCall(TAC.Label(widget.label), List(src), None)

        if (newline) {
            useWidget(PrintlnWidget)
            generateCall(TAC.Label(PrintlnWidget.label), List(), None)
        }
    }

    /**
      * Generate the low level IR for a Read high level IR instruction.
      *
      * @param dest The destination virtual register to read into
      * @param widget The read widget to use
      * @param st The generator state
      */
    private def generateRead(dest: TAC.VirtualReg, widget: Widget)
                            (using st: GenState): Unit = {
        useWidget(widget)
        generateCall(TAC.Label(widget.label), List(dest), Some(dest))
    }

    /**
      * Generates the low level IR for a `Chr` high level IR node
      *
      * @param src
      * @param widget
      * @param st
      */
    private def generateChr(dest: TAC.VirtualReg, src: TAC.VirtualReg, widget: Widget)
                           (using st: GenState): Unit = {

        generateUnaryOp(dest, src, Some(widget), {(d, s) => {

            /* Move the operand into w0 for the error function */
            emit(Arm.MovReg(Size.W, PhysicalReg.returnRegister, s))

            /* If the int value is not 0-127 inclusive, runtime error */
            emit(Arm.CmpImm(Size.W, s, Arm.Immediate(Constants.ASCII_MAX)))
            emit(Arm.BranchCond(Condition.HI, Arm.AddrLabel(widget.label)))

            if (d != s) {
                emit(Arm.MovReg(Size.W, d, s))
            }
        }})
    }

    /**
      * Generate the low level IR for a Call high level IR instruction
      *
      * @param label The label of the function
      * @param args The argument operands to the function
      * @param destOpt The register that the return value is expected
      */
    private def generateCall(label: TAC.Label,
                             args: List[TAC.Operand],
                             destOpt: Option[TAC.VirtualReg])
                             (using GenState) = {

        val stackArgs = args.drop(AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS)
        val regArgs = args.take(AARch64Limits.ARM_NUM_REGS_FOR_FUNC_ARGS)

        /* Allocate stack space (moving stack pointer) to accomodate arguments
            beyond the first 8 which must live on the stack.
            
            we use a byte per arg because when calling CLib funcs this is a requirement
            and also byte alignment is faster for hardware.
        */
        val unalignedStackBytes = stackArgs.length * MemoryLayout.BYTES_PER_STACK_ARG
        val stackBytesAdded = alignBytes(unalignedStackBytes)
        emit(Arm.SubImm(Size.X,
                        PhysicalReg.stackPointer,
                        PhysicalReg.stackPointer,
                        Arm.Immediate(stackBytesAdded)))

        /* Space is now allocated, so store excess arguments on the stack */
        stackArgs.zipWithIndex.foreach { (arg, i) =>

            val size = operandSize(arg)
            val offset = i * MemoryLayout.BYTES_PER_STACK_ARG /* offset from the stack pointer */

            /* Put the argument in a scratch register */
            val scratch = PhysicalReg.ipcScratch1
            val valReg = loadOperandToTarget(arg, size, scratch)

            /* Store the argument (in scratch) on the stack */
            emit(Arm.Store(size,
                          valReg, 
                          Arm.AddrImmOffset(PhysicalReg.stackPointer,
                                           Arm.Immediate(offset))))
        }

        /* For each function of the register arguments */
        regArgs.zipWithIndex.foreach { case (arg, index) =>

            /* Get the physical register of the current argument */
            val targetReg = PhysicalReg.argRegisters(index)

            val opSize = operandSize(arg)
            
            /* Get the value in the target register */
            arg match {
                case argVReg: TAC.VirtualReg =>
                    /* Move out of a stack slot into a scratch register */
                    val valReg = loadToReg(locOf(argVReg), opSize, PhysicalReg.ipcScratch1)
                    
                    /* Move the ith argument into the wi */
                    if (valReg != targetReg) {
                        emit(Arm.MovReg(opSize, targetReg, valReg))
                    }
                
                case TAC.Immediate(value) =>
                    /* Move the immediate directly to the argument register */
                    generateMovImmediate(opSize, targetReg, Arm.Immediate(value))
                
                case TAC.Label(name) => {
                    /* Move the label (string directive in this case) to
                       the argument register */
                    emit(Arm.Lea(targetReg, Arm.AddrLabel(name)))
                }
            }
        }

        /* Branch to the function */
        emit(Arm.Bl(Arm.AddrLabel(label.name)))

        /* Post function call, we restore the stack pointer (collapse the frame) */
        if (stackBytesAdded > 0) {
            emit(Arm.AddImm(Size.X, 
                           PhysicalReg.stackPointer, 
                           PhysicalReg.stackPointer, 
                           Arm.Immediate(stackBytesAdded)))
        }

        /* Move the function return value to the expected register */
        destOpt.foreach { dest =>
            val resultReg = PhysicalReg.returnRegister 
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
        optEpilogueLabel.foreach(l => emit(Arm.BranchUncond(l)))
    }

    /**
      * Generate low-level IR for a Cmp instruction.
      * Compares two operands by loading them into registers and emitting Arm.CmpReg or Arm.CmpImm.
      * 
      * @param src1 the first source operand (left side of comparison)
      * @param src2 the second source operand (right side of comparison)
      */
    private def generateCmp(src1: TAC.VirtualReg, src2: TAC.Operand)(using st: GenState): Unit = {
        val src1Loc = locOf(src1)
        
        /* Load first operand into a register */
        val src1Reg = loadToReg(src1Loc, src1.size, PhysicalReg.ipcScratch1)

        /* Handle second operand and emit comparison */
        (src2: @unchecked) match {
            case src2VReg: TAC.VirtualReg =>
                /* Register-register comparison */
                val src2Loc = locOf(src2VReg)
                val src2Reg = loadToReg(src2Loc, src1.size, PhysicalReg.ipcScratch2)
                emit(Arm.CmpReg(src1.size, src1Reg, src2Reg))

            case TAC.Immediate(value) =>
                /* Register-immediate comparison */
                emit(Arm.CmpImm(src1.size, src1Reg, Arm.Immediate(value)))
        }
    }

    /**
      * Generate low-level IR for a SetCond instruction.
      * 
      * @param dest the destination virtual register
      * @param cond the condition to test
      */
    private def generateSetCond(dest: TAC.VirtualReg, cond: Condition)(using st: GenState): Unit = {
        val destLoc = locOf(dest)
        
        /* Determine result register */
        val resultReg = destLoc match {
            case reg: PhysicalReg => reg
            case _ => PhysicalReg.ipcScratch1
        }
        
        /* Emit conditional set instruction */
        emit(Arm.CSet(dest.size, resultReg, cond))
        
        /* Store result */
        storeToLoc(resultReg, destLoc, dest.size)
    }

    /**
      * Generate low-level IR for boolean NOT.
      * 
      * @param dest the destination virtual register
      * @param src the source virtual register to not
      */
    private def generateNot(dest: TAC.VirtualReg, src: TAC.VirtualReg)(using st: GenState): Unit = {
        generateUnaryOp(dest, src, None, { (destReg, srcReg) =>

            /* Compare with 0 and set to 1 if equal (i.e. invert the boolean) */
            emit(Arm.CmpImm(src.size, srcReg, Arm.Immediate(Constants.FALSE)))
            emit(Arm.CSet(dest.size, destReg, Condition.EQ))
        })
    }

    /**
      * Generate the low level IR for a negation (-x)
      *
      * @param dest the destination virtual register
      * @param src the source virtual register to not
      */
    private def generateNeg(dest: TAC.VirtualReg, src: TAC.VirtualReg)(using st: GenState): Unit = {
        generateUnaryOp(dest, src, Some(IntOverflowErrorWidget), { (destReg, srcReg) =>
            emit(Arm.Negs(dest.size, destReg, srcReg))
            emit(Arm.BranchCond(Condition.VS, Arm.AddrLabel(IntOverflowErrorWidget.label)))
        })
    }
}
