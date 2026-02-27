package wacc.backend

import wacc.ExitCodes
import RuntimeError.asmLength

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

    /** The actual assembly instructions for this widget */
    def source: String

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
class RuntimeLibraryDispatcher {

    /** Represent active widgets as a set, we don't want duplicates */
    private val activeWidgets = scala.collection.mutable.Set.empty[Widget]

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
            // Recursively add dependencies
            w.dependencies.foreach(use)
        }
    }

    /**
      * Writes the assembly for all required widgets to a BufferedWriter.
      * 
      * @param bw the buffered writer to write to
      */
    def writeCollectedAssembly(bw: java.io.BufferedWriter): Unit = {
        bw.write("// RUNTIME LIBRARY:\n\n")
        val widgetList = activeWidgets.toList
        widgetList.zipWithIndex.foreach { (widget, idx) =>
            bw.write(widget.source)
            if (idx < widgetList.length - 1) bw.write("\n\n")
        }
    }
}

/** Runtime widget to implement `_println`, printing a new line to stdout */
object PrintlnWidget extends Widget {
    override val label: String = "_println"

    override val description: String = "prints a new line to stdout"

    override val source: String = 
        s"""// ${description}
            |// length of .L._println_str0
            |     .word 0
            |.L._println_str0:
            |    .asciz ""
            |    .align 4
            |_println:
            |     // push {lr}
            |     stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |     adr x0, .L._println_str0
            |     bl puts
            |     mov x0, #0
            |     bl fflush
            |     // pop {lr}
            |     ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |     ret""".stripMargin
}


/** Runtime widget to implement `_printi`, printing an integer to stdout */
object PrintIntWidget extends Widget {
    override val label: String = "_printi"

    override val description: String = "prints an integer value to stdout"

    override val source: String =
        s"""// ${description}
            |// length of .L._printi_str0
            |    .word 2
            |.L._printi_str0:
            |    .asciz "%d"
            |    .align 4
            |_printi:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x1, x0
            |    adr x0, .L._printi_str0
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget to implement `_prints`, printing a string to stdout */
object PrintStringWidget extends Widget {
    override val label: String = "_prints"

    override val description: String = "prints a string value to stdout"

    override val source: String =
        s"""// ${description}
            |// length of .L._prints_str0
            |    .word 4
            |.L._prints_str0:
            |    .asciz "%.*s"
            |    .align 4
            |_prints:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x2, x0
            |    ldur w1, [x0, #-4]
            |    adr x0, .L._prints_str0
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget to implement `_printb`, printing a bool to stdout */
object PrintBoolWidget extends Widget {
    override val label: String = "_printb"

    override val description: String = "prints a boolean value to stdout"

    override val source: String =
        s"""// ${description}
            |// length of .L._printb_str0
            |    .word 5
            |.L._printb_str0:
            |    .asciz "false"
            |// length of .L._printb_str1
            |    .word 4
            |.L._printb_str1:
            |    .asciz "true"
            |// length of .L._printb_str2
            |    .word 4
            |.L._printb_str2:
            |    .asciz "%.*s"
            |    .align 4
            |_printb:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w0, #0
            |    b.ne .L_printb0
            |    adr x2, .L._printb_str0
            |    b .L_printb1
            |.L_printb0:
            |    adr x2, .L._printb_str1
            |.L_printb1:
            |    ldur w1, [x2, #-4]
            |    adr x0, .L._printb_str2
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget to implement `_printc`, printing a char to stdout */
object PrintCharWidget extends Widget {
    override val label: String = "_printc"

    override val description: String = "prints a char value to stdout"

    override val source: String =
        s"""// ${description}
            |// length of .L._printc_str0
            |    .word 2
            |.L._printc_str0:
            |    .asciz "%c"
            |    .align 4
            |_printc:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x1, x0
            |    adr x0, .L._printc_str0
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}


/** Runtime widget to implement `_printp`, printing a pointer value to stdout */
object PrintPointerWidget extends Widget {
    override val label: String = "_printp"

    override val description: String = "prints a pointer value to stdout"

    override val source: String =
        s"""// ${description}
            |// length of .L._printp_str0
            |    .word 2
            |.L._printp_str0:
            |    .asciz "%p"
            |    .align 4
            |_printp:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x1, x0
            |    adr x0, .L._printp_str0
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget to implement `_readi`, reading an integer from stdin */
object ReadIntegerWidget extends Widget {
    override val label: String = "_readi"

    override val description: String = "reads an int value from stdin"

    override val source: String =
        s"""// ${description}
            |// length of .L._readi_str0
            |    .word 2
            |.L._readi_str0:
            |    .asciz "%d"
            |    .align 4
            |_readi:
            |    // X0 contains the "original" value of the destination of the read
            |    // allocate space on the stack to store the read: preserve alignment!
            |    // the passed default argument should be stored in case of EOF
            |    // aarch64 mandates ${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}-byte SP alignment
            |       // at all times, might as well merge the stores
            |    // push {x0, lr}
            |    stp x0, lr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x1, sp
            |    adr x0, .L._readi_str0
            |    bl scanf
            |    // pop {x0, lr}
            |    ldp x0, lr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget to implement `_readc`, reading a char from stdin */
object ReadCharWidget extends Widget {
    override val label: String = "_readc"

    override val description: String = "reads a char value from stdin"

    override val source: String =
        s"""// ${description}
            |// length of .L._readc_str0
            |    .word 3
            |.L._readc_str0:
            |    .asciz " %c"
            |    .align 4
            |_readc:
            |    // X0 contains the "original" value of the destination of the read
            |    // allocate space on the stack to store the read: preserve alignment!
            |    // the passed default argument should be stored in case of EOF
            |    // aarch64 mandates ${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}-byte SP alignment
            |       // at all times, might as well merge the stores
            |    // push {x0, lr}
            |    stp x0, lr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    mov x1, sp
            |    adr x0, .L._readc_str0
            |    bl scanf
            |    // pop {x0, lr}
            |    ldp x0, lr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin
}

/** Runtime widget that implements `_errDivZero`, terminating on division/modulo by zero. */
object DivByZeroErrorWidget extends Widget {
    override val label: String = "_errDivZero"

    override val description: String = "division or modulo by zero error handler"

    override val source: String =
        s"""// ${description}
            |// length of .L._errDivZero_str0
            |    .word ${RuntimeError.divByZero.asmLength}
            |.L._errDivZero_str0:
            |    .asciz "${RuntimeError.divByZero}"
            |    .align 4
            |_errDivZero:
            |    adr x0, .L._errDivZero_str0
            |    bl _prints
            |    mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |    bl exit""".stripMargin

    override val dependencies: Set[Widget] = Set(PrintStringWidget)
}

/** Runtime widget that implements `_errOverflow`, terminating on integer overflow/underflow. */
object IntOverflowErrorWidget extends Widget {
    override val label: String = "_errOverflow"

    override val description: String = "integer overflow or underflow error handler"

    override val source: String =
        s"""// ${description}
            |// length of .L._errOverflow_str0
            |    .word ${RuntimeError.intOverflow.asmLength}
            |.L._errOverflow_str0:
            |    .asciz "${RuntimeError.intOverflow}"
            |    .align 4
            |_errOverflow:
            |    adr x0, .L._errOverflow_str0
            |    bl _prints
            |    mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |    bl exit""".stripMargin

    override val dependencies: Set[Widget] = Set(PrintStringWidget)
}

/** Runtime widget that implements `_errNull`, terminating on null pair dereference/free. */

object NullDerefErrorWidget extends Widget {
    override val label: String = "_errNull"

    override val description: String = "null pair dereference or free error handler"

    override val source: String =
        s"""// ${description}
            |// length of .L._errNull_str0
            |    .word ${RuntimeError.nullDeref.asmLength}
            |.L._errNull_str0:
            |    .asciz "${RuntimeError.nullDeref}"
            |    .align 4
            |_errNull:
            |    adr x0, .L._errNull_str0
            |    bl _prints
            |    mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |    bl exit""".stripMargin

    override val dependencies: Set[Widget] = Set(PrintStringWidget)
}

/** Runtime widget that implements `_errOutOfMemory`, terminating on heap allocation failure */
object OutOfMemoryErrorWidget extends Widget {
    override val label: String = "_errOutOfMemory"

    override val description: String = "heap allocation failure error handler"

    override val source: String =
        s"""// ${description}
            |// length of .L._errOutOfMemory_str0
            |    .word ${RuntimeError.outOfMemory.asmLength}
            |.L._errOutOfMemory_str0:
            |    .asciz "${RuntimeError.outOfMemory}"
            |    .align 4
            |_errOutOfMemory:
            |    adr x0, .L._errOutOfMemory_str0
            |    bl _prints
            |    mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |    bl exit""".stripMargin

    override val dependencies: Set[Widget] = Set(PrintStringWidget)
}

/** Runtime widget that implements `_errOutOfBounds`, terminating on array bounds errors. */
object ArrayBoundsErrorWidget extends Widget {
    override val label: String = "_errOutOfBounds"

    override val description: String = "array index out of bounds error handler"

    override val source: String =
        s"""// ${description}
            |// length of .L._errOutOfBounds_str0
            |    .word ${RuntimeError.outOfBounds.asmLength}
            |.L._errOutOfBounds_str0:
            |    .asciz "${RuntimeError.outOfBounds}"
            |    .align 4
            |_errOutOfBounds:
            |    adr x0, .L._errOutOfBounds_str0
            |    bl printf
            |    mov x0, #0
            |    bl fflush
            |    mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |    bl exit""".stripMargin
}

/** Runtime widget that implements `_malloc`, wrapping `malloc` with out of memory checking */
object MallocWidget extends Widget {
    override val label: String = "_malloc"

    override val description: String = "allocate heap memory with out-of-memory checking"

    override val source: String =
        s"""// ${description}
            |_malloc:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    bl malloc
            |    cbz x0, _errOutOfMemory
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(OutOfMemoryErrorWidget)
}

/** Runtime widget that implements `_freepair`, freeing a pair pointer with null checking. */
object FreePairWidget extends Widget {
    override val label: String = "_freepair"

    override val description: String = "free a pair with null checking"

    override val source: String =
        s"""// ${description}
            |_freepair:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cbz x0, _errNull
            |    bl free
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(NullDerefErrorWidget)
}

/** Runtime widget that implements `_freearray`, freeing an array pointer.
  * The array pointer points to element 0, but the malloced block starts 4 bytes
  * before (at the length header), so we subtract 4 before calling free. */
object FreeArrayWidget extends Widget {
    override val label: String = "_freearray"

    override val description: String = "free an array by adjusting pointer past length header"

    override val source: String =
        s"""// ${description}
            |_freearray:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    sub x0, x0, #4
            |    bl free
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set()
}

/** Runtime widget that implements `_arrLoad1`, bounds-checked load of 1-byte array elements. */
object ArrayLoad1Widget extends Widget {
    override val label: String = "_arrLoad1"

    override val description: String = "bounds-checked array load for 1-byte elements (char/bool)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: array ptr passed in X7, index in X17, 
            |//LR (W30) is used as general register, and return into X7
            |_arrLoad1:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    ldrb w7, [x7, x17]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/** Runtime widget that implements `_arrStore1`, bounds-checked store of 1-byte array elements. */
object ArrayStore1Widget extends Widget {
    override val label: String = "_arrStore1"

    override val description: String = "bounds-checked array store for 1-byte elements (char/bool)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: array ptr passed in X7, ind in X17, value to store in X8, LR (W30) general 
            |_arrStore1:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    strb w8, [x7, x17]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/** Runtime widget that implements `_arrLoad4`, bounds-checked load of 4-byte array elements. */
object ArrayLoad4Widget extends Widget {
    override val label: String = "_arrLoad4"

    override val description: String = "bounds-checked array load for 4-byte elements (int)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is general, and return into X7
            |_arrLoad4:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    ldr w7, [x7, x17, lsl #2]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/**  Runtime widget that implements `_arrStore4`, bounds-checked store of 4-byte array elements. */
object ArrayStore4Widget extends Widget {
    override val label: String = "_arrStore4"

    override val description: String = "bounds-checked array store for 4-byte elements (int)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: arr ptr passed in X7, ind in X17, value to store in X8, LR (W30) is general 
            |_arrStore4:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    str w8, [x7, x17, lsl #2]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/** Runtime widget that implements `_arrLoad8`, bounds-checked load of 8-byte array elements. */
object ArrayLoad8Widget extends Widget {
    override val label: String = "_arrLoad8"

    override val description: String = "bounds-checked array load for 8-byte elements (pointers)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: arr ptr passed in X7, ind in X17, LR (W30) is general, and return into X7
            |_arrLoad8:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    ldr x7, [x7, x17, lsl #3]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/** Runtime widget that implements `_arrStore8`, bounds-checked store of 8-byte array elements. */
object ArrayStore8Widget extends Widget {
    override val label: String = "_arrStore8"

    override val description: String = "bounds-checked array store for 8-byte elements (pointers)"

    override val source: String =
        s"""// ${description}
            |// Special calling convention: arr ptr passed in X7, ind in X17, value to store in X8, LR (W30) is general 
            |_arrStore8:
            |    // push {lr}
            |    stp lr, xzr, [sp, #-${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}]!
            |    cmp w17, #0
            |    csel x1, x17, x1, lt // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.lt _errOutOfBounds
            |    ldur w30, [x7, #-4]
            |    cmp w17, w30
            |    csel x1, x17, x1, ge // this must be a 64-bit move so that it doesn't truncate if the move fails
            |    b.ge _errOutOfBounds
            |    str x8, [x7, x17, lsl #3]
            |    // pop {lr}
            |    ldp lr, xzr, [sp], #${AARch64Limits.ARM_STACK_ALIGNMENT_BYTES}
            |    ret""".stripMargin

    override val dependencies: Set[Widget] = Set(ArrayBoundsErrorWidget)
}

/** Runtime widget for an invalid int given to the `chr` function (not 0-127 inclusive) */
object BadCharWidgetError extends Widget {
    override val label: String = "_errBadChar"

    override val description: String = "A char integer value must be between 0 and 127 inclusive"

    override val source: String =
        s"""//${description}
            |// length of .L._errBadChar_str0
            |   .word ${RuntimeError.badChar.asmLength}
            |.L._errBadChar_str0:
            |   .asciz "${RuntimeError.badChar}"
            |.align 4
            |_errBadChar:
            |   adr x0, .L._errBadChar_str0
            |   bl printf
            |   mov x0, #0
            |   bl fflush
            |   mov w0, #${ExitCodes.code(ExitCodes.RuntimeErrorCode)}
            |   bl exit""".stripMargin
}