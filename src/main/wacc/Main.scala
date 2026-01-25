package wacc

import java.io.File
import scala.io.Source

def main(args: Array[String]): Unit = {

    if(args.length != 1) {
        println("Requires file.")
        sys.exit(-1)
    }

    val file = new File(args(0))

    if(!file.exists()) {
        println("File does not exist.")
        sys.exit(-1)
    }
    val source = Source.fromFile(file)
    val sourceCode = try source.mkString finally source.close()
    val result = parser.parse(sourceCode)

    result match {
        case Left(msg) =>
            Console.err.println("Failure: 100 - Syntax error:")
            Console.err.println(msg)
            sys.exit(100)

        case Right(syntacticAST) =>
            val semanticErrors = new SemanticErrorLogger()

            val renamed = new Renamer()
            val renamedAST = renamed.rename(syntacticAST,semanticErrors)

            val typeChecker = new TypeChecker()
            val funcTable = renamedAST.info match {
                case SemanticData.TopLevel(ft) => ft
                case _ => Map.empty[String, SemanticData.Func]
            }
            typeChecker.check(renamedAST, semanticErrors, funcTable)

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