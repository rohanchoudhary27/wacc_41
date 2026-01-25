package wacc

import java.io.File
import scala.io.Source

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
    val sourceCode = try source.mkString finally source.close()

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
            val semanticErrors = new SemanticErrorLogger()

            // Rename the AST and return this AST, build the senamtic errors if there are any
            val renamed = new Renamer()
            val renamedAST = renamed.rename(syntacticAST,semanticErrors)

            // Type check the renamed AST, adding the errors if there are any
            val typeChecker = new TypeChecker()
            val funcTable = renamedAST.info match {
                case SemanticData.TopLevel(ft) => ft
                case _ => Map.empty[String, SemanticData.Func]
            }
            typeChecker.check(renamedAST, semanticErrors, funcTable)

            // If there were no errors then exit with 0, else 200
            semanticErrors.hasErrors() match {
                case false =>
                    sys.exit(0)
                case true =>
                    Console.err.println("Failure: 200 - Semantic error:")
                    for(err <- semanticErrors.retrieveAllErrors()) {
                        Console.err.println(s"Error at: ${err.pos}: ${err.msg}")
                    }
                    sys.exit(200)
        }
    }
}