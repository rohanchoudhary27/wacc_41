package wacc

import java.io.File
import scala.io.Source

/**
* Main function for WACC compiler.
*
* Stages:
* Parsing: Source text to a SyntacticAST, Exit code 100 on failure
* Renaming: Transforms SyntacticAST to use SemanticData, Log errors on failure
* Type Checker: Verifies Type correctness and, Log errors on failure
*
* Exit Codes:
*   0 - Success
*   100 - Syntactic Errors
*   200 - Semantic Errors
*   -1 - System Error
*
* @param args: Command Line arguments. Only 1 is expected.
*/
def main(args: Array[String]): Unit = {

    // Expect a singular argument
    if(args.length != 1) {
        println("Requires file.")
        sys.exit(-1)
    }

    // Put in file
    val file = new File(args(0))

    // Check file created succesfully
    if(!file.exists()) {
        println("File does not exist.")
        sys.exit(-1)
    }

    // Load file as String
    val source = Source.fromFile(file)
    val codeLines: Vector[String] =
        try source.getLines().toVector
        finally source.close()

    val sourceCode: String = codeLines.mkString("\n")


    // Parse the string
    val result = parser.parse(sourceCode)

    result match {
        // Syntax error from parsing - exit with 100
        case Left(msg) =>
            Console.err.println("Failure: 100 - Syntax error:")
            Console.err.println(msg)
            sys.exit(100)

        // Parsing complete successfully
        case Right(syntacticAST) =>
            // Create an errors object
            val semanticErrors = new SemanticErrorLogger(codeLines)

            // Rename the AST and return this AST, build the senamtic errors if there are any
            val renamedAST = Renamer.rename(syntacticAST,semanticErrors)

            // Type check the renamed AST, adding the errors if there are any
            val funcTable = renamedAST.info match {
                case SemanticData.TopLevel(ft) => ft
                case _ => Map.empty[String, SemanticData.Func]
            }
            TypeChecker.check(renamedAST, semanticErrors, funcTable)

            // If there were no errors then exit with 0, else 200
            semanticErrors.hasErrors() match {
                case false =>
                    println("Successfully compiled file with no errors.")
                    sys.exit(0)
                case true =>
                    Console.err.println("Failure: 200 - Semantic error:")
                    Console.err.println(semanticErrors.retrieveAllErrors())
                    sys.exit(200)
        }
    }
}
