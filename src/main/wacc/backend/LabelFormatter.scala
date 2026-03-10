package wacc.backend

/**
  * String formatting for different label types in the generated assembly
  */
object LabelNameFormatter {
    val mainLabel = "main"
    val exitLabel = "exit"
    val elseLabel = ".L_else"
    val ifEndLabel = ".L_end"
    val whileBodyLabel = ".L_body"
    val whileCondLabel = ".L_cond"

    /** Start of a function (pre prologue) label name */
    def funcEntry(funcName: String) = s"f_${funcName}"

    /** Function body (post prologue) label name */
    def funcBody(funcLabelName: String) = s".L_${funcLabelName}_body"

    /** Function epilogue label name */
    def funcEpilogue(funcLabelName: String) = s".L_${funcLabelName}_epilogue"

    /** General label name in a function */
    def generalPurpose(ind: Int) = s".L$ind"

    /** Global string directive names */
    def stringName(strCount: Int) = s"string_${strCount}"
}

/**
  * Label names for CLib function calls
  */
object CLibLabels {
    val exitLabel = "exit"
    val putsLabel = "puts"
    val scanfLabel = "scanf"
    val mallocLabel = "malloc"
    val freeLabel = "free"
    val fflushLabel = "fflush"
    val printfLabel = "printf"
} 