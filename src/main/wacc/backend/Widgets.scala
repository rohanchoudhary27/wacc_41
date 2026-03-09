package wacc.backend

import wacc.ExitCodes
import wacc.backend.Arm.*

/**
 * Represents a snippet of AArch64 assembly that provides common functionality
 * Each widget contains its own assembly definition and may depend on other widgets
 */
abstract class Widget {
    /** The unique label for the subroutine */
    def label: String

    /** A text describing the purpose of this widget, is embedded as a comment line just before the
        widget source */
    def description: String

    /** String constants required by this widget */
    def dataDirectives: List[Arm.DataDirective] = List.empty

    /** The low-level IR instructions for this widget */
    def instrs: List[Arm.Instruction]

    /** Any other widgets this widget calls */
    def dependencies: Set[Widget] = Set.empty
}

/** Runtime error messages used by error handler widgets */
object RuntimeError {
    private val prefix = "fatal error: "

    val divByZero     = s"${prefix}division or modulo by zero\\n"
    val intOverflow   = s"${prefix}integer overflow or underflow occurred\\n"
    val nullDeref     = s"${prefix}null pair dereferenced or freed\\n"
    val outOfMemory   = s"${prefix}out of memory\\n"
    val outOfBounds   = s"${prefix}array index %d out of bounds\\n"
    val badChar       = s"${prefix}int %d is not ascii character 0-127\\n"

    /** Byte length when assembled: \\n is 2 chars in Scala but 1 byte in assembly */
    extension (s: String) def asmLength: Int = s.length - 1
}

/**
 * The dispatcher manages which widgets are required by the current compilation stage and
 * ensures that only necessary widgets are included in the final .s file.
 */
class WidgetLibraryDispatcher {

    /** Represent active widgets as a set, we don't want duplicates */
    private val activeWidgets = scala.collection.mutable.Set.empty[Widget]

    /** Represent active directives as a set, we don't want duplicates */
    private val activeDirectives = scala.collection.mutable.Set.empty[DataDirective]

    /**
     * Marks a widget as required, adding any dependencies as well
     * 
     * This is called by the Code Generator whenever a widget
     * is used.
     * 
     * @param w The widget to include in the final output
     */
    def use(w: Widget): Unit = {
        if (!activeWidgets.contains(w)) {
            activeWidgets += w
            activeDirectives ++= w.dataDirectives
            // Recursively add dependencies
            w.dependencies.foreach(use)
        }
    }

    def collectWidgetDirectiveIR: List[DataDirective] = activeDirectives.toList
    def collectWidgetInstrIR: List[Instruction]  = activeWidgets.toList.flatMap(w => w.instrs)
}

// =============================================================================
//  IO WIDGETS
// =============================================================================

object PrintlnWidget extends Widget {
    override val label: String = "_println"
    override val description: String = "prints a new line to stdout"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._println_str0"), ""))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._println_str0")),
        Bl(AddrLabel("puts")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object PrintIntWidget extends Widget {
    override val label: String = "_printi"
    override val description: String = "prints an integer value to stdout"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._printi_str0"), "%d"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(1),
               PhysicalReg.returnRegister),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._printi_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object PrintStringWidget extends Widget {
    override val label: String = "_prints"
    override val description: String = "prints a string value to stdout"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._prints_str0"), "%.*s"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(2),
               PhysicalReg.returnRegister),
        Ldur(Size.W,
             PhysicalReg.argRegisters(1),
             AddrImmOffset(PhysicalReg.returnRegister, Immediate(-4))),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._prints_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object PrintBoolWidget extends Widget {
    override val label: String = "_printb"
    override val description: String = "prints a boolean value to stdout"
    override val dataDirectives = List(
        DataDirective(AddrLabel(".L._printb_str0"), "false"),
        DataDirective(AddrLabel(".L._printb_str1"), "true"),
        DataDirective(AddrLabel(".L._printb_str2"), "%.*s")
    )

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.returnRegister,
               Immediate(0)),
        BranchCond(Condition.NE,
                   AddrLabel(".L_printb0")),
        Lea(PhysicalReg.argRegisters(2),
            AddrLabel(".L._printb_str0")),
        BranchUncond(AddrLabel(".L_printb1")),
        LabelDef(AddrLabel(".L_printb0")),
        Lea(PhysicalReg.argRegisters(2),
            AddrLabel(".L._printb_str1")),
        LabelDef(AddrLabel(".L_printb1")),
        Ldur(Size.W,
             PhysicalReg.argRegisters(1),
             AddrImmOffset(PhysicalReg.argRegisters(2), Immediate(-4))),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._printb_str2")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object PrintCharWidget extends Widget {
    override val label: String = "_printc"
    override val description: String = "prints a char value to stdout"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._printc_str0"), "%c"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(1),
               PhysicalReg.returnRegister),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._printc_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object PrintPointerWidget extends Widget {
    override val label: String = "_printp"
    override val description: String = "prints a pointer value to stdout"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._printp_str0"), "%p"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(1),
               PhysicalReg.returnRegister),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._printp_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ReadIntegerWidget extends Widget {
    override val label: String = "_readi"
    override val description: String = "reads an int value from stdin"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._readi_str0"), "%d"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.returnRegister,
                  PhysicalReg.linkReg,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(1),
               PhysicalReg.stackPointer),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._readi_str0")),
        Bl(AddrLabel("scanf")),
        LoadPair(Size.X,
                 PhysicalReg.returnRegister,
                 PhysicalReg.linkReg,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ReadCharWidget extends Widget {
    override val label: String = "_readc"
    override val description: String = "reads a char value from stdin"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._readc_str0"), " %c"))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.returnRegister,
                  PhysicalReg.linkReg,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        MovReg(Size.X,
               PhysicalReg.argRegisters(1),
               PhysicalReg.stackPointer),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._readc_str0")),
        Bl(AddrLabel("scanf")),
        LoadPair(Size.X,
                 PhysicalReg.returnRegister,
                 PhysicalReg.linkReg,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

// =============================================================================
// ERROR WIDGETS
// =============================================================================

object DivByZeroErrorWidget extends Widget {
    override val label: String = "_errDivZero"
    override val description: String = "division or modulo by zero error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errDivZero_str0"), RuntimeError.divByZero))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errDivZero_str0")),
        Bl(AddrLabel("_prints")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

object IntOverflowErrorWidget extends Widget {
    override val label: String = "_errOverflow"
    override val description: String = "integer overflow or underflow error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errOverflow_str0"), RuntimeError.intOverflow))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errOverflow_str0")),
        Bl(AddrLabel("_prints")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

object NullDerefErrorWidget extends Widget {
    override val label: String = "_errNull"
    override val description: String = "null pair dereference or free error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errNull_str0"), RuntimeError.nullDeref))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errNull_str0")),
        Bl(AddrLabel("_prints")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

object OutOfMemoryErrorWidget extends Widget {
    override val label: String = "_errOutOfMemory"
    override val description: String = "heap allocation failure error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errOutOfMemory_str0"), RuntimeError.outOfMemory))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errOutOfMemory_str0")),
        Bl(AddrLabel("_prints")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

object ArrayBoundsErrorWidget extends Widget {
    override val label: String = "_errOutOfBounds"
    override val description: String = "array index out of bounds error handler"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errOutOfBounds_str0"), RuntimeError.outOfBounds))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errOutOfBounds_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

object BadCharWidgetError extends Widget {
    override val label: String = "_errBadChar"
    override val description: String = "A char integer value must be between 0 and 127 inclusive"
    override val dataDirectives = List(DataDirective(AddrLabel(".L._errBadChar_str0"), RuntimeError.badChar))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(".L._errBadChar_str0")),
        Bl(AddrLabel("printf")),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel("fflush")),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel("exit"))
    )
}

// --- MEMORY ALLOCATION WIDGETS ---

object MallocWidget extends Widget {
    override val label: String = "_malloc"
    override val description: String = "allocate heap memory with out-of-memory checking"
    override val dependencies: Set[Widget] = Set(OutOfMemoryErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.ipcScratch1,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Bl(AddrLabel("malloc")),
        Cbz(Size.X,
            PhysicalReg.returnRegister,
            AddrLabel("_errOutOfMemory")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.ipcScratch1,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object FreePairWidget extends Widget {
    override val label: String = "_freepair"
    override val description: String = "free a pair with null checking"
    override val dependencies: Set[Widget] = Set(NullDerefErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Cbz(Size.X,
            PhysicalReg.returnRegister,
            AddrLabel("_errNull")),
        Bl(AddrLabel("free")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object FreeArrayWidget extends Widget {
    override val label: String = "_freearray"
    override val description: String = "free an array by adjusting pointer past length header"

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        SubImm(Size.X,
               PhysicalReg.returnRegister,
               PhysicalReg.returnRegister,
               Immediate(4)),
        Bl(AddrLabel("free")),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

// =============================================================================
//  ARRAY BOUNDS WIDGETS
// =============================================================================

object ArrayLoad1Widget extends Widget {
    override val label: String = "_arrLoad1"
    override val description: String = "bounds-checked array load for 1-byte elements (char/bool)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        
        MovReg(Size.X, PhysicalReg.argRegisters(1), PhysicalReg.ipcScratch2),
        
        CmpImm(Size.W, PhysicalReg.ipcScratch2, Immediate(0)),
        BranchCond(Condition.LT, AddrLabel("_errOutOfBounds")),
        
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        BranchCond(Condition.GE, AddrLabel("_errOutOfBounds")),
        
        LoadByte(Size.W,
                 PhysicalReg.widgetResultRegister,
                 AddrRegOffset(PhysicalReg.widgetResultRegister,
                               PhysicalReg.ipcScratch2)),
        
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ArrayStore1Widget extends Widget {
    override val label: String = "_arrStore1"
    override val description: String = "bounds-checked array store for 1-byte elements (char/bool)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.ipcScratch2,
               Immediate(0)),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.LT),
        BranchCond(Condition.LT,
                   AddrLabel("_errOutOfBounds")),
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.GE),
        BranchCond(Condition.GE,
                   AddrLabel("_errOutOfBounds")),
        StoreByte(Size.W,
                  PhysicalReg.mathScratch,
                  AddrRegOffset(PhysicalReg.widgetResultRegister,
                                PhysicalReg.ipcScratch2)),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ArrayLoad4Widget extends Widget {
    override val label: String = "_arrLoad4"
    override val description: String = "bounds-checked array load for 4-byte elements (int)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.ipcScratch2,
               Immediate(0)),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.LT),
        BranchCond(Condition.LT,
                   AddrLabel("_errOutOfBounds")),
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.GE),
        BranchCond(Condition.GE,
                   AddrLabel("_errOutOfBounds")),
        Load(Size.W,
             PhysicalReg.widgetResultRegister,
             AddrRegShift(PhysicalReg.widgetResultRegister,
                          PhysicalReg.ipcScratch2,
                          ShiftAmounts._2)),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ArrayStore4Widget extends Widget {
    override val label: String = "_arrStore4"
    override val description: String = "bounds-checked array store for 4-byte elements (int)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.ipcScratch2,
               Immediate(0)),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.LT),
        BranchCond(Condition.LT,
                   AddrLabel("_errOutOfBounds")),
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.GE),
        BranchCond(Condition.GE,
                   AddrLabel("_errOutOfBounds")),
        Store(Size.W,
              PhysicalReg.mathScratch,
              AddrRegShift(PhysicalReg.widgetResultRegister,
                           PhysicalReg.ipcScratch2,
                           ShiftAmounts._2)),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ArrayLoad8Widget extends Widget {
    override val label: String = "_arrLoad8"
    override val description: String = "bounds-checked array load for 8-byte elements (pointers)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.ipcScratch2,
               Immediate(0)),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.LT),
        BranchCond(Condition.LT,
                   AddrLabel("_errOutOfBounds")),
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.GE),
        BranchCond(Condition.GE,
                   AddrLabel("_errOutOfBounds")),
        Load(Size.X,
             PhysicalReg.widgetResultRegister,
             AddrRegShift(PhysicalReg.widgetResultRegister,
                          PhysicalReg.ipcScratch2,
                          ShiftAmounts._3)),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}

object ArrayStore8Widget extends Widget {
    override val label: String = "_arrStore8"
    override val description: String = "bounds-checked array store for 8-byte elements (pointers)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        StorePair(Size.X,
                  PhysicalReg.linkReg,
                  PhysicalReg.returnRegister,
                  AddrPreIndex(PhysicalReg.stackPointer,
                               Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        CmpImm(Size.W,
               PhysicalReg.ipcScratch2,
               Immediate(0)),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.LT),
        BranchCond(Condition.LT,
                   AddrLabel("_errOutOfBounds")),
        Ldur(Size.W,
             PhysicalReg.linkReg,
             AddrImmOffset(PhysicalReg.widgetResultRegister,
                           Immediate(-4))),
        CmpReg(Size.W,
               PhysicalReg.ipcScratch2,
               PhysicalReg.linkReg),
        Csel(Size.X,
             PhysicalReg.argRegisters(1),
             PhysicalReg.ipcScratch2,
             PhysicalReg.argRegisters(1),
             Condition.GE),
        BranchCond(Condition.GE,
                   AddrLabel("_errOutOfBounds")),
        Store(Size.X,
              PhysicalReg.mathScratch,
              AddrRegShift(PhysicalReg.widgetResultRegister,
                           PhysicalReg.ipcScratch2,
                           ShiftAmounts._3)),
        LoadPair(Size.X,
                 PhysicalReg.linkReg,
                 PhysicalReg.returnRegister,
                 AddrPostIndex(PhysicalReg.stackPointer,
                               Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
        Ret
    )
}
