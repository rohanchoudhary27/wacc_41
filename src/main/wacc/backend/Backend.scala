package wacc.backend

import wacc.ExitCodes._
import wacc.frontend.{Program, SemanticData}
import java.io.{File, PrintWriter}

case class BackendCompilationResult(
    exitCode: ExitCode,
    outputFile: Option[String] = None
)

object WaccBackend {

    /**
      * Runs the backend part of the compiler on the AST from the frontend
      *
      * @param ast: the AST produced from the frontend
      * @param outputFilename: .s file to write the Aarch64 assembly to
      * @return BackendCompilationResult: Compilation result
      */
    def run(ast: Program[SemanticData], outputFilename: String): BackendCompilationResult = {
        try {

            /* Phase 1 - Flatten*/
            val highLevelIR: RegProgram = Flattener.flatten(ast)

            /* Phase 2 - Register Allocation*/
            val registerAllocation: RegisterAllocator.AllocationResult = 
                RegisterAllocator.allocate(highLevelIR)

            /* Phase 3 - Code Generation */
            val (lowLevelIR, runtimeDispatcher) =
                CodeGenerator.generate(highLevelIR, registerAllocation)

            /* Phase 4 - Printer */
            val assemblyMain: String = Outputter.emit(lowLevelIR)
            val assemblyWidgets: String = runtimeDispatcher.getCollectedAssembly

            val assemblyFinal: String = s"$assemblyMain\n\n$assemblyWidgets\n"
            writeToFile(outputFilename, assemblyFinal)
            BackendCompilationResult(Success, Some(outputFilename))

        } catch {
            case e: Exception => Console.err.println(
                s"Internal compiler failure during backend code generation: ${e.getMessage}"
                )
            BackendCompilationResult(BackendFailure, None)
        }
    }

    /**
      * Helper function to write the generated assembly string to a file.
      * 
      * @param filename The output file path
      * @param content The string content to write
      */
    private def writeToFile(fileName: String, content: String): Unit = {
        val file = new File(fileName)
        val writer = new PrintWriter(file)
        try {
            writer.write(content)
        } finally {
            writer.close()
        }
    }
}