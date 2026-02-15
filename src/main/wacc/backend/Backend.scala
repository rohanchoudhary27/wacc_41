package wacc.backend

import wacc.frontend.{Program, SemanticData}
import java.io.{File, PrintWriter}

class WaccBackend {
    // Flattener
    // Register Allocator
    // CodeGenerator
    // Printer

    def compile(ast: Program[SemanticData], outputFileName: String): Unit = {
        val highLevelIR: RegProgram = Flattener.flatten(ast)
        val registerAllocation: RegisterAllocator.AllocationResult = RegisterAllocator.allocate(highLevelIR)
        //val lowLevelIR: ArmProgram = CodeGenerator.generate(highLevelIR, allocation)
        //val assemblyText: String = ArmPrinter.print(lowLevelIR)
        //writeToFile(outputFileName, assemblyText)
    }

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