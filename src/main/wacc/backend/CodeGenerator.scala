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

        generateFuncPrologue()
        generateInstrs(regProgram.body)
        emit(ArmMovImm(Size.W, PhysicalReg.argRegisters(0),
                       ArmImmediate(code(Success))))
        generateFuncEpilogue()

        /* Final low-level IR for the program */
        val program = ArmProgram(globalCtx.finalDirectives, st.finalInstrs)

        (program, globalCtx.dispatcher)
    }

    /**
      * Generate low level IR for the function prologue (saving the return addr to the link
      * register and setting the frame pointer)
      */
    private def generateFuncPrologue()(using GenState) = {
        emit(ArmStorePair(Size.X,
                         PhysicalReg.framePointer,
                         PhysicalReg.linkReg,
                         ArmAddrPreIndex(PhysicalReg.stackPointer, ArmImmediate(-16))))
        emit(ArmMovReg(Size.X, PhysicalReg.framePointer, PhysicalReg.stackPointer))
    }
    
    /**
      * Generate low level IR for the function epilogue (restoring the link and
      * frame pointer registers)
      */
    private def generateFuncEpilogue()(using GenState) = {
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
        
        // Function label
        emit(ArmLabelDef(ArmAddrLabel(func.name)))
        
        generateFuncPrologue()
        generateInstrs(func.body)
        generateFuncEpilogue()
   
        emit(ArmRet)
        
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
      * If the location is already a PhysicalReg, returns it directly.
      * If the location is a StackSlot, emits a load instruction to the scratch register.
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
            emit(ArmLoad(size, scratch, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))))
            scratch
    }
    
    /**
      * Store a value from a physical register to a location.
      * If the location is a PhysicalReg, emits a move instruction (if different).
      * If the location is a StackSlot, emits a store instruction.
      *
      * @param src the source PhysicalReg
      * @param loc the destination location
      * @param size the register size
      */
    private def storeToLoc(src: PhysicalReg, loc: Location, size: Size)(using st: GenState): Unit = loc match {
        case dest: PhysicalReg =>
            if (src != dest) emit(ArmMovReg(size, dest, src))
        case StackSlot(offset) =>
            emit(ArmStore(size, src, ArmAddrImmOffset(PhysicalReg.framePointer, ArmImmediate(offset))))
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
        case Mul(dest, src1, src2) => ??? // TODO
        case Div(dest, src1, src2) => ??? // TODO
        case Mod(dest, src1, src2) => ??? // TODO
        case Neg(dest, src) => ??? // TODO
        
        /* Bitwise operations */
        case And(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmAndReg.apply, ArmAndImm.apply)
        case Or(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmOrReg.apply, ArmOrImm.apply)
        case Not(dest, src) => ??? // TODO
        case Lsl(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmLslReg.apply, ArmLslImm.apply)
        case Lsr(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmLsrReg.apply, ArmLsrImm.apply)
        case Asr(dest, src1, src2) => 
            generateBinaryOp(dest, src1, src2, ArmAsrReg.apply, ArmAsrImm.apply)
        
        /* Comparison and conditional */
        case Cmp(src1, src2) => ??? // TODO
        case SetCond(dest, cond) => ??? // TODO
        
        /* Control flow */
        case BranchUncond(label) => emit(ArmBranchUncond(ArmAddrLabel(label.name)))
        case BranchCond(cond, label) => emit(ArmBranchCond(cond, ArmAddrLabel(label.name)))
        case LabelDef(label) => emit(ArmLabelDef(ArmAddrLabel(label.name)))
        
        /* Function calls */
        case Call(label, args, destOpt) => generateCall(label, args, destOpt)
        case Ret => emit(ArmRet)
        
        /* Print */
        case Print(src, widget, newline) => generatePrint(src, widget, newline)
        
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

        /* For each function argument */
        args.zipWithIndex.foreach { case (arg, index) =>

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

        /* Move the function return value to the expected register */
        destOpt.foreach { dest =>
            val resultReg = PhysicalReg.argRegisters(0) 
            storeToLoc(resultReg, locOf(dest), dest.size)
        }
    }
}
