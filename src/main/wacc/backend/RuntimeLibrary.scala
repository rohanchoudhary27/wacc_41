package wacc.backend

/**
 * Represents a snippet of AArch64 assembly that provides common functionality
 * Each widget contains its own assembly definition and may depend on other widgets
 */
abstract class Widget {
    /** The unique label for the subroutine */
    def label: String

    /** The actual assembly instructions for this widget */
    def source: String

    /** A text describing the purpose of this widget, is embedded as a comment line just before the
        widget source */
    def description: String

    /** Any other widgets this widget calls */
    def dependencies: Set[Widget] = Set.empty
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
     * Returns the full assembly string block for all required widgets
     * 
     * @return the assembly source of all used widgets
     */
    def getCollectedAssembly: String = {
        activeWidgets.map(_.source).mkString("\n\n")
    }
}


/* Runtime Library implemented as Widgets */

/**
 * AArch64 Widget to print a signed 32-bit integer.
 * Uses the C standard library 'printf'.
 */
object PrintIntWidget extends Widget {
    override val label: String = "_printInt"

    /* AArch64 requires 16-byte alignment before calling printf */
    override val source: String = ""

    override val description: String = "prints an integer value to stdout"
}

/**
 * AArch64 Widget to print a signed 32-bit integer.
 * Uses the C standard library 'printf'.
 */
object PrintStringWidget extends Widget {
    override val label: String = "_printInt"

    /* AArch64 requires 16-byte alignment before calling printf */
    override val source: String = ""

    override val description: String = "prints a string value to stdout"
}

/**
 * AArch64 Widget to print a signed 32-bit integer.
 * Uses the C standard library 'printf'.
 */
object PrintBoolWidget extends Widget {
    override val label: String = "_printInt"

    /* AArch64 requires 16-byte alignment before calling printf */
    override val source: String = ""

    override val description: String = "prints a boolean value to stdout"
}

/**
 * AArch64 Widget to print a signed 32-bit integer.
 * Uses the C standard library 'printf'.
 */
object PrintCharWidget extends Widget {
    override val label: String = "_printChar"

    /* AArch64 requires 16-byte alignment before calling printf */
    override val source: String = ""

    override val description: String = "prints a char value to stdout"
}
