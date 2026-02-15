package wacc.backend

import wacc.ExitCodes._
import wacc.frontend.{Program, SemanticData}
import java.io.{File, PrintWriter}

case class BackendCompilationResult(
    exitCode: ExitCode,
    outputFile: Option[String] = None
)

object WaccBackend {
    // Flattener
    // Register Allocator
    // CodeGenerator
    // Printer

    def compile(ast: Program[SemanticData], outputFilename: String): BackendCompilationResult = {
        try {
            val highLevelIR: RegProgram = Flattener.flatten(ast)
            val registerAllocation: RegisterAllocator.AllocationResult = 
                RegisterAllocator.allocate(highLevelIR)
            val runtimeDispatcher = new RuntimeLibraryDispatcher()
            //val lowLevelIR: ArmProgram = CodeGenerator.generate(highLevelIR, allocation)
            //val assemblyMain: String = ArmPrinter.print(lowLevelIR)
            val assemblyWidgets: String = runtimeDispatcher.getCollectedAssembly
            //val assemblyFinal: String = s"$assemblyMain\n\n$assemblyWidgets"
            //writeToFile(outputFilename, assemblyFinal)
            BackendCompilationResult(Success, Some(outputFilename))
        } catch {
            case e: Exception => Console.err.println(s"Internal compiler failure during backend code generation: ${e.getMessage}")
            BackendCompilationResult(BackendFailure, None)
        }
    }

    /**
      * Helper function to write the generated assembly string to a file.
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