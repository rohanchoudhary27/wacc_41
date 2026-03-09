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

/**
  * Helper functions (templates) for the widget classes to reduce repeated IR
  */
object WidgetHelpers {

    /**
      * IR template for an array load or store with bounds checking 
      *
      * @param label The label pointing to the start of the widget
      * @param actionInstr The specific load/store instruction
      * @return A template list of `Arm.Instruction` for array load/store with bounds checking
      */
    def buildArrayBoundsCall(label: String, actionInstr: Arm.Instruction): List[Arm.Instruction] = {
        List(
            LabelDef(AddrLabel(label)),
            StorePair(Size.X,
                      PhysicalReg.linkReg,
                      PhysicalReg.returnRegister,
                      AddrPreIndex(PhysicalReg.stackPointer, Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            
            MovReg(Size.X, PhysicalReg.argRegisters(1), PhysicalReg.ipcScratch2),
            
            CmpImm(Size.W, PhysicalReg.ipcScratch2, Immediate(0)),
            BranchCond(Condition.LT, AddrLabel(ArrayBoundsErrorWidget.label)),
            
            Ldur(Size.W, PhysicalReg.linkReg, AddrImmOffset(PhysicalReg.widgetResultRegister, Immediate(-4))),
            CmpReg(Size.W, PhysicalReg.ipcScratch2, PhysicalReg.linkReg),
            BranchCond(Condition.GE, AddrLabel(ArrayBoundsErrorWidget.label)),
            
            actionInstr,
            
            LoadPair(Size.X,
                     PhysicalReg.linkReg,
                     PhysicalReg.returnRegister,
                     AddrPostIndex(PhysicalReg.stackPointer, Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            Ret
        )
    }

    /**
      * IR template for a heap allocation
      *
      * @param label The label pointing to the start of the widget
      * @param setupInstrs The specific instructions before the CLib call to malloc/free
      * @return A template list of `Arm.Instruction` for heap allocation
      */
    def buildAllocationCall(label: String,
                            setupInstrs: List[Arm.Instruction]): List[Arm.Instruction] = {
        List(
            LabelDef(AddrLabel(label)),
            StorePair(Size.X,
                      PhysicalReg.linkReg,
                      PhysicalReg.ipcScratch1,
                      AddrPreIndex(PhysicalReg.stackPointer,
                                   Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        ) ++ setupInstrs ++ List(
            LoadPair(Size.X,
                     PhysicalReg.linkReg,
                     PhysicalReg.ipcScratch1,
                     AddrPostIndex(PhysicalReg.stackPointer,
                                   Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            Ret
        )
    }

    /**
      * IR template for a error widgets
      *
      * @param label The label pointing to the start of the widget
      * @param errorPrintLabel The label string pointing to the string directive error message
      * @return A template list of `Arm.Instruction` for error widgets
      */
    def bulidStandardErrorCall(label: String,
                               errorPrintLabel: String): List[Arm.Instruction] = {
        List(
            LabelDef(AddrLabel(label)),
            Lea(PhysicalReg.returnRegister,
                AddrLabel(errorPrintLabel)),
            Bl(AddrLabel(PrintStringWidget.label)),
            Movz(Size.W,
                PhysicalReg.returnRegister,
                Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
            Bl(AddrLabel(CLibLabels.exitLabel))
        )
    }

    /**
      * IR template for read clib calls
      *
      * @param label The label pointing to the start of the widget
      * @param formatStrLabel The label string pointing to the format string directive
      * @return A template list of `Arm.Instruction` for reads
      */
    def buildStandardReadCall(
        label: String,
        formatStrLabel: String
    ): List[Arm.Instruction] = {
        List(
            LabelDef(AddrLabel(label)),
            StorePair(Size.X,
                      PhysicalReg.returnRegister,
                      PhysicalReg.linkReg,
                      AddrPreIndex(PhysicalReg.stackPointer, Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            
            MovReg(Size.X, PhysicalReg.argRegisters(1), PhysicalReg.stackPointer),
            
            Lea(PhysicalReg.returnRegister, AddrLabel(formatStrLabel)),
            
            Bl(AddrLabel(CLibLabels.scanfLabel)),
            
            LoadPair(Size.X,
                     PhysicalReg.returnRegister,
                     PhysicalReg.linkReg,
                     AddrPostIndex(PhysicalReg.stackPointer, Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            Ret
        )
    }

    /**
      * IR template for standard same-line print clib calls
      *
      * @param label The label pointing to the start of the widget
      * @param formatStrLabel The label string pointing to the format string directive
      * @return A template list of `Arm.Instruction` for same-line prints
      */
    def buildStandardPrintCall(
        label: String,
        formatStrLabel: String
    ): List[Arm.Instruction] = {
        buildCLibIOCall(
            label = label,
            cLibFunc = CLibLabels.printfLabel,
            setupInstrs = List(
                MovReg(Size.X, PhysicalReg.argRegisters(1), PhysicalReg.returnRegister),
                Lea(PhysicalReg.returnRegister, AddrLabel(formatStrLabel))
            )
        )
    }

    /**
      * IR template for line print clib calls, templating both println and other same-line prints
      *
      * @param label The label pointing to the start of the widget
      * @param cLibFunc The string label name of the clib print function being called for printing
      * @return A template list of `Arm.Instruction` for prints
      */
    def buildCLibIOCall(label: String,
                        cLibFunc: String,
                        setupInstrs: List[Arm.Instruction]): List[Arm.Instruction] = {
        List(
            LabelDef(AddrLabel(label)),
            StorePair(Size.X,
                      PhysicalReg.linkReg,
                      PhysicalReg.returnRegister,
                      AddrPreIndex(PhysicalReg.stackPointer,
                                   Immediate(-AARch64Limits.ARM_STACK_ALIGNMENT_BYTES)))
        ) ++ setupInstrs ++ List(
            Bl(AddrLabel(cLibFunc)),
            Movz(Size.X, PhysicalReg.returnRegister, Immediate(0)),
            Bl(AddrLabel(CLibLabels.fflushLabel)),
            LoadPair(Size.X,
                     PhysicalReg.linkReg,
                     PhysicalReg.returnRegister,
                     AddrPostIndex(PhysicalReg.stackPointer,
                                   Immediate(AARch64Limits.ARM_STACK_ALIGNMENT_BYTES))),
            Ret
        )
    }
}


// =============================================================================
//  IO WIDGETS
// =============================================================================

object PrintlnWidget extends Widget {
    override val label: String = "_println"
    override val description: String = "prints a new line to stdout"

    private val str0Label = ".L._println_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(str0Label), ""))

    override val instrs = WidgetHelpers.buildCLibIOCall(
            label = label,
            cLibFunc = CLibLabels.putsLabel,
            setupInstrs = List(
                Lea(PhysicalReg.returnRegister, AddrLabel(str0Label))
            )
        )
}

object PrintIntWidget extends Widget {
    override val label: String = "_printi"
    override val description: String = "prints an integer value to stdout"

    private val printStr = ".L._printi_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(printStr), "%d"))

    override val instrs = WidgetHelpers.buildStandardPrintCall(label, printStr)
}

object PrintStringWidget extends Widget {
    override val label: String = "_prints"
    override val description: String = "prints a string value to stdout"

    private val printStr = ".L._prints_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(printStr), "%.*s"))

    override val instrs = WidgetHelpers.buildCLibIOCall(
        label = label,
        cLibFunc = CLibLabels.printfLabel,
        setupInstrs = List(
            MovReg(Size.X, PhysicalReg.argRegisters(2), PhysicalReg.returnRegister),
            Ldur(Size.W, PhysicalReg.argRegisters(1), AddrImmOffset(PhysicalReg.returnRegister, Immediate(-4))),
            Lea(PhysicalReg.returnRegister, AddrLabel(printStr))
        )
    )

}

object PrintBoolWidget extends Widget {
    override val label: String = "_printb"
    override val description: String = "prints a boolean value to stdout"

    private val falseStrPrint = ".L_printb0"
    private val trueStrPrint = ".L_printb1"

    private val falseStr = ".L._printb_str0"
    private val trueStr = ".L._printb_str1"
    private val formatSpecifier = ".L._printb_str2"
    override val dataDirectives = List(
        DataDirective(AddrLabel(falseStr), "false"),
        DataDirective(AddrLabel(trueStr), "true"),
        DataDirective(AddrLabel(formatSpecifier), "%.*s")
    )

    override val instrs = WidgetHelpers.buildCLibIOCall(
        label = label,
        cLibFunc = CLibLabels.printfLabel,
        setupInstrs = List(
            CmpImm(Size.W,
                   PhysicalReg.returnRegister,
                   Immediate(0)),
            BranchCond(Condition.NE,
                       AddrLabel(falseStrPrint)),
            Lea(PhysicalReg.argRegisters(2),
                AddrLabel(falseStr)),
            BranchUncond(AddrLabel(trueStrPrint)),
            LabelDef(AddrLabel(falseStrPrint)),
            Lea(PhysicalReg.argRegisters(2),
                AddrLabel(trueStr)),
            LabelDef(AddrLabel(trueStrPrint)),
            Ldur(Size.W,
                 PhysicalReg.argRegisters(1),
                 AddrImmOffset(PhysicalReg.argRegisters(2), Immediate(-4))),
            Lea(PhysicalReg.returnRegister,
                AddrLabel(formatSpecifier))
        )
    )
}

object PrintCharWidget extends Widget {
    override val label: String = "_printc"
    override val description: String = "prints a char value to stdout"

    private val printStr = ".L._printc_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(printStr), "%c"))

    override val instrs = WidgetHelpers.buildStandardPrintCall(label, printStr)
}

object PrintPointerWidget extends Widget {
    override val label: String = "_printp"
    override val description: String = "prints a pointer value to stdout"

    private val printStr = ".L._printp_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(printStr), "%p"))

    override val instrs = WidgetHelpers.buildStandardPrintCall(label, printStr)
}

object ReadIntegerWidget extends Widget {
    override val label: String = "_readi"
    override val description: String = "reads an int value from stdin"

    private val readLabel = ".L._readi_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(readLabel), "%d"))
    override val instrs = WidgetHelpers.buildStandardReadCall(label, readLabel)
}

object ReadCharWidget extends Widget {
    override val label: String = "_readc"
    override val description: String = "reads a char value from stdin"

    private val readLabel = ".L._readc_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(readLabel), " %c"))

    override val instrs = WidgetHelpers.buildStandardReadCall(label, readLabel)
}

// =============================================================================
// ERROR WIDGETS
// =============================================================================

object DivByZeroErrorWidget extends Widget {
    override val label: String = "_errDivZero"
    override val description: String = "division or modulo by zero error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)

    private val errorPrintLabel = ".L._errDivZero_str0"
    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.divByZero))

    override val instrs = WidgetHelpers.bulidStandardErrorCall(label, errorPrintLabel)
}

object IntOverflowErrorWidget extends Widget {
    override val label: String = "_errOverflow"
    override val description: String = "integer overflow or underflow error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)

    private val errorPrintLabel = ".L._errOverflow_str0"

    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.intOverflow))

    override val instrs = WidgetHelpers.bulidStandardErrorCall(label, errorPrintLabel)
}

object NullDerefErrorWidget extends Widget {
    override val label: String = "_errNull"
    override val description: String = "null pair dereference or free error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)

    private val errorPrintLabel = ".L._errNull_str0"

    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.nullDeref))

    override val instrs = WidgetHelpers.bulidStandardErrorCall(label, errorPrintLabel)
}

object OutOfMemoryErrorWidget extends Widget {
    override val label: String = "_errOutOfMemory"
    override val description: String = "heap allocation failure error handler"
    override val dependencies: Set[Widget] = Set(PrintStringWidget)

    private val errorPrintLabel = ".L._errOutOfMemory_str0"

    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.outOfMemory))

    override val instrs = WidgetHelpers.bulidStandardErrorCall(label, errorPrintLabel)
}

object ArrayBoundsErrorWidget extends Widget {
    override val label: String = "_errOutOfBounds"
    override val description: String = "array index out of bounds error handler"

    private val errorPrintLabel = ".L._errOutOfBounds_str0"

    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.outOfBounds))

    override val instrs = WidgetHelpers.bulidStandardErrorCall(label, errorPrintLabel)
}

object BadCharWidgetError extends Widget {
    override val label: String = "_errBadChar"
    override val description: String = "A char integer value must be between 0 and 127 inclusive"

    private val errorPrintLabel = ".L._errBadChar_str0"

    override val dataDirectives = List(DataDirective(AddrLabel(errorPrintLabel), RuntimeError.badChar))

    override val instrs = List(
        LabelDef(AddrLabel(label)),
        Lea(PhysicalReg.returnRegister,
            AddrLabel(errorPrintLabel)),
        Bl(AddrLabel(CLibLabels.printfLabel)),
        Movz(Size.X,
             PhysicalReg.returnRegister,
             Immediate(0)),
        Bl(AddrLabel(CLibLabels.fflushLabel)),
        Movz(Size.W,
             PhysicalReg.returnRegister,
             Immediate(ExitCodes.code(ExitCodes.RuntimeErrorCode))),
        Bl(AddrLabel(CLibLabels.exitLabel))
    )
}

// =============================================================================
// MEMORY ALLOC WIDGETS
// =============================================================================

object MallocWidget extends Widget {
    override val label: String = "_malloc"
    override val description: String = "allocate heap memory with out-of-memory checking"
    override val dependencies: Set[Widget] = Set(OutOfMemoryErrorWidget)

    override val instrs = WidgetHelpers.buildAllocationCall(
        label = label,
        setupInstrs = List(
            Bl(AddrLabel(CLibLabels.mallocLabel)),
            Cbz(Size.X,
                PhysicalReg.returnRegister,
                AddrLabel(OutOfMemoryErrorWidget.label)),
        )
    )
}

object FreePairWidget extends Widget {
    override val label: String = "_freepair"
    override val description: String = "free a pair with null checking"
    override val dependencies: Set[Widget] = Set(NullDerefErrorWidget)

    override val instrs = WidgetHelpers.buildAllocationCall(
        label = label,
        setupInstrs = List(
            Cbz(Size.X,
                PhysicalReg.returnRegister,
                AddrLabel(NullDerefErrorWidget.label)),
            Bl(AddrLabel(CLibLabels.freeLabel)),
        )
    )
}

object FreeArrayWidget extends Widget {
    override val label: String = "_freearray"
    override val description: String = "free an array by adjusting pointer past length header"

    override val instrs = WidgetHelpers.buildAllocationCall(
        label = label,
        setupInstrs = List(
            SubImm(Size.X,
                PhysicalReg.returnRegister,
                PhysicalReg.returnRegister,
                Immediate(4)),
            Bl(AddrLabel(CLibLabels.freeLabel)),
        )
    )
}

// =============================================================================
//  ARRAY BOUNDS WIDGETS
// =============================================================================

object ArrayLoad1Widget extends Widget {
    override val label: String = "_arrLoad1"
    override val description: String = "bounds-checked array load for 1-byte elements (char/bool)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        LoadByte(Size.W, PhysicalReg.widgetResultRegister, 
                 AddrRegOffset(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2))
    )
}

object ArrayStore1Widget extends Widget {
    override val label: String = "_arrStore1"
    override val description: String = "bounds-checked array store for 1-byte elements (char/bool)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        StoreByte(Size.W, PhysicalReg.mathScratch, 
                  AddrRegOffset(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2))
    )
}

object ArrayLoad4Widget extends Widget {
    override val label: String = "_arrLoad4"
    override val description: String = "bounds-checked array load for 4-byte elements (int)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        Load(Size.W, PhysicalReg.widgetResultRegister, 
             AddrRegShift(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2, ShiftAmounts._2))
    )
}

object ArrayStore4Widget extends Widget {
    override val label: String = "_arrStore4"
    override val description: String = "bounds-checked array store for 4-byte elements (int)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        Store(Size.W, PhysicalReg.mathScratch, 
              AddrRegShift(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2, ShiftAmounts._2))
    )
}

object ArrayLoad8Widget extends Widget {
    override val label: String = "_arrLoad8"
    override val description: String = "bounds-checked array load for 8-byte elements (pointers)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        Load(Size.X, PhysicalReg.widgetResultRegister, 
             AddrRegShift(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2, ShiftAmounts._3))
    )
}

object ArrayStore8Widget extends Widget {
    override val label: String = "_arrStore8"
    override val description: String = "bounds-checked array store for 8-byte elements (pointers)"
    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)

    override val instrs = WidgetHelpers.buildArrayBoundsCall(label, 
        Store(Size.X, PhysicalReg.mathScratch, 
              AddrRegShift(PhysicalReg.widgetResultRegister, PhysicalReg.ipcScratch2, ShiftAmounts._3))
    )
}