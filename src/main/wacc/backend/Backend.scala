package wacc.backend

import wacc.ExitCodes.*
import wacc.TopLevelError
import wacc.frontend.{Program, SemanticData}
import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure, Using}

/**
  * A result returned from WaccBackend.run()
  *
  * @param exitCode The exit code of the backend
  * @param outputFile The output file to write assembly to
  * @param error A backend error message
  */
private case class BackendCompilationResult(
    exitCode: ExitCode,
    outputFile: Option[String] = None,
    error: Option[String] = None
)

/* The object running the compiler backend */
object WaccBackend {

    /**
      * Runs the backend part of the compiler on the AST from the frontend
      *
      * @param ast: the AST produced from the frontend
      * @param outputFilename: .s file to write the Aarch64 assembly to
      * @return BackendCompilationResult: Compilation result
      */
    def run(ast: Program[SemanticData], outputFilename: String): BackendCompilationResult = {

        compileToFile(ast, outputFilename) match
            case Success(_) =>
                BackendCompilationResult(CompilerSuccess, Some(outputFilename))

            case Failure(e: java.io.IOException) =>
                Console.err.println(TopLevelError.FileSystemError(e.getMessage).outputFormat)
                BackendCompilationResult(CompilerFailure, outputFile=None, error=Some(e.getMessage))

            /* Internal backend error */
            case Failure(e) =>
                Console.err.println(TopLevelError.BackendPipelineError(e.getMessage).outputFormat)
                BackendCompilationResult(BackendFailure, outputFile=None, error=Some(e.getMessage))
    }

    /**
      * Run the full backend pipeline and write assembly directly to a file
      *
      * @param ast The input semantically typed AST
      * @param outputFilename The output .s file path
      * @return Success(()) on success, Failure on any error
      */
    private def compileToFile(ast: Program[SemanticData], outputFilename: String): Try[Unit] = for {

        /* Phase 1 - Flatten */
        highLevelIR <- Try(Flattener.flatten(ast))

        /* Phase 2 - Register Allocation */
        registerAllocation <- Try(RegisterAllocator.allocate(highLevelIR))

        /* Phase 3 - Code Generation */
        (lowLevelIR, runtimeDispatcher) <- Try(CodeGenerator.generate(highLevelIR, registerAllocation))

        /* Phase 4 - Write assembly to file */
        _ <- {
            val filePath = Paths.get(outputFilename)
            Option(filePath.getParent).foreach(Files.createDirectories(_))

            Using(BufferedWriter(FileWriter(filePath.toFile))) { bw =>
                Outputter.emit(lowLevelIR, bw)
                bw.write("\n\n")
                runtimeDispatcher.writeCollectedAssembly(bw)
                bw.write("\n")
            }
        }

    } yield ()
}