package wacc

import scala.collection.mutable.ListBuffer

/** Represents an error that occurred during compilation. 
 * 
 * @param pos: A (row, column) tuple in the program where the error occurred
 * @param codeLine: The line of code where the error occurred
 * @param msg: A string error message
*/
sealed trait CompilerError {
    def pos: SourcePos
    def codeLine: String
    def msg: String
}

/** A syntax error from the parser.
  *
  * @param msg: The complete error message from the parser (via Parsley)
  */
case class SyntaxError(msg: String) extends CompilerError {
    def pos: SourcePos = (0, 0) /* Parsley error includes its own position */
    def codeLine: String = "" /* Parsley error includes its own code context */
}

/** A semantic error from the renamer or type checker.
  */
case class SemanticError(pos: SourcePos, msg: String, codeLine: String) extends CompilerError

/** A logger class for recording compilation errors.
  */
case class ErrorLogger(val codeLines: Vector[String] = Vector.empty) {

    private val errors = ListBuffer[CompilerError]()
    private val tabSize = 4
    private val indentTrackedLines = codeLines.map(indentAlign)


    /** Strip leading and trailing whitespace from a string and provide the number
      * of leading whitespace chars (indent) stripped.
      *
      * @param str: The string to be stripped
      * @return: A tuple with the stripped string and number of indent characters
      */
    private def indentAlign(str: String): (String, Int) = {
        val leadingChars = str.takeWhile(_.isWhitespace)
        val strippedStr = str.drop(leadingChars.length).stripTrailing()
        
        /* Tab characters count for 4 spaces and jump to the next
        tab column (which is how tabs work) */
        var visualIndent = 0
        for (c <- leadingChars) {
            if (c == '\t') {
                visualIndent += tabSize - (visualIndent % tabSize)
            } else {
                visualIndent += 1
            }
        }

        (strippedStr, visualIndent)
    }

    /* 
    private def indentAlign(str: String): (String, Int) = {
        val leftStrippedStr = str.stripLeading()
        (leftStrippedStr.stripTrailing(), str.length - leftStrippedStr.length)
    }
     */

    /** Log a syntax error from the parser.
      *
      * @param msg: The complete error message from the parser
      */
    def logSyntaxError(msg: String): Unit = {
        errors += SyntaxError(msg)
    }

    /** Log a semantic error (from renamer or type checker).
      *
      * @param pos: A (row, column) tuple in the program where the error occurred
      * @param msg: A string error message
      * @param extraInfo: Optional additional text for the error
      */
    def logSemanticError(pos: SourcePos, msg: String, extraInfo: String = ""): Unit = {
        val codeLine =
            if (indentTrackedLines.nonEmpty && pos(0) > 0 && pos(0) <= indentTrackedLines.length)
                val (line, _) = indentTrackedLines(pos(0) - 1)
                line
            else ""

        errors += SemanticError(pos, msg + extraInfo, codeLine)
    }

    /** Return the position of the first error logged.
      *
      * @return: The SourcePos of the first error, or (0, 0) if none
      */
    def firstErrorPos(): SourcePos = {
        if (errors.isEmpty) (0, 0)
        else errors.head.pos
    }

    /** Return a list of the the semantic errors logged.
      *
      * @return: A string containing all formatted errors
      */
    def retrieveAllErrors(): String = {
        val errorBuilder = new StringBuilder

        for (err <- errors) {
            err match {
                case SyntaxError(msg) =>
                    errorBuilder.append(msg)
                    errorBuilder.append("\n")

                case sem: SemanticError =>
                    errorBuilder.append(s"${sem.msg}\n\n")

                    /* Code where the error occurred and a pointer to the position in the line */
                    if (indentTrackedLines.nonEmpty && 
                        sem.pos(0) > 0 && 
                        sem.pos(0) <= indentTrackedLines.length) {

                        val (codeLine, indent) = indentTrackedLines(sem.pos(0) - 1)
                        val errorPointerLine = " " * (sem.pos(1) - indent - 1) + "^\n"
                        errorBuilder.append(s"${codeLine}\n")
                        errorBuilder.append(s"${errorPointerLine}")
                    }
            }
        }

        errorBuilder.toString()
    }

    /** Check whether any errors have been logged.
      *
      * @return: true if any errors have been found, false otherwise
      */
    def hasErrors(): Boolean = errors.nonEmpty
}
