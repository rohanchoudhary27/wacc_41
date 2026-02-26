package wacc.backend

import wacc.ExitCodes.*
import wacc.frontend.{Program, SemanticData}
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.util.{Try, Success, Failure}

/**
  * A result returned from WaccBackend.run()
  *
  * @param exitCode The exit code of the backend
  * @param outputFile The output file to write assembly to
  * @param error A backend error message
  */
case class BackendCompilationResult(
    exitCode: ExitCode,
    outputFile: Option[String] = None,
    error: Option[String] = None
)

/* The object running the compiler backend */
object WaccBackend:

    /**
      * Runs the backend part of the compiler on the AST from the frontend
      *
      * @param ast: the AST produced from the frontend
      * @param outputFilename: .s file to write the Aarch64 assembly to
      * @return BackendCompilationResult: Compilation result
      */
    def run(ast: Program[SemanticData], outputFilename: String): BackendCompilationResult = {
        val pipeline = for {

            /* Phase 1 - Flatten*/
            highLevelIR <- Try(Flattener.flatten(ast))

            /* Phase 2 - Register Allocation*/
            registerAllocation <- Try(RegisterAllocator.allocate(highLevelIR))

            /* Phase 3 - Code Generation */
            (lowLevelIR, runtimeDispatcher) <- Try(CodeGenerator.generate(highLevelIR, registerAllocation))

            /* Phase 4 - Printer */
            assemblyMain <- Try(Outputter.emit(lowLevelIR))
            assemblyWidgets <- Try(runtimeDispatcher.getCollectedAssembly)

            assemblyFinal = s"$assemblyMain\n\n$assemblyWidgets\n"
            _ <- writeToFile(outputFilename, assemblyFinal)

        } yield outputFilename

        pipeline match
            case Success(filename) =>
                BackendCompilationResult(CompilerSuccess, Some(filename))
            case Failure(e) =>
                Console.err.println(s"Backend Error: ${e.getMessage}")
                BackendCompilationResult(BackendFailure, None, Some(e.getMessage))
    }

    /**
      * Helper function to write the generated assembly string to a file.
      * 
      * @param filename The output file path
      * @param content The string content to write
      */
    private def writeToFile(path: String, content: String): Try[Unit] = Try {
        val filePath = Paths.get(path)
        Option(filePath.getParent).foreach(Files.createDirectories(_))
        Files.writeString(
            filePath,
            content,
            StandardOpenOption.CREATE,
            StandardOpenOption.TRUNCATE_EXISTING
        )
    }
end WaccBackend