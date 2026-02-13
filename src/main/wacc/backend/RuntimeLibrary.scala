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

    /** Any other widgets this widget calls. 
        e.g., _printBool might depend on _printStr for "true"/"false" 
     */
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
     * Returns the full assembly block for all required widgets
     * 
     * @return the assembly source of all used widgets
     */
    def getCollectedAssembly: String = {
        activeWidgets.map(_.source).mkString("\n\n")
    }
}