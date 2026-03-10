package wacc.frontend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import scala.jdk.CollectionConverters._
import scala.util.Using
import wacc.ExitCodes._

/** Integration tests for the WACC import resolver wired into the full compilation pipeline. */
class ImportIntegrationSpec extends AnyFlatSpec with Matchers {

    private val importsBaseDir = Paths.get("AllIntegrationTests/custom/ExtensionsMilestone/Imports")
    private val testLibDir     = importsBaseDir.resolve("testLib")
    private val validDir       = importsBaseDir.resolve("valid")
    private val invalidDir     = importsBaseDir.resolve("invalid")

    private def mkTempDir(): Path = {
        val dir = Files.createTempDirectory("wacc-import-integration-")
        dir.toFile.deleteOnExit()
        dir
    }

    /** Runs a WACC file inside a sandboxed temp directory that also
      * includes a copy of its sibling test files and the shared `testLib`. */
    private def runSandboxed(sourceFile: Path): Unit = {
        val tempDir = mkTempDir()
        val sourceDir = sourceFile.getParent
        val mainFile = sourceFile.getFileName.toString

        // 1. Copy all files from the source directory
        if (Files.exists(sourceDir)) {
            Using.resource(Files.list(sourceDir)) { stream =>
                stream.forEach { p =>
                    if (Files.isRegularFile(p)) {
                        Files.copy(p, tempDir.resolve(p.getFileName), REPLACE_EXISTING)
                    }
                }
            }
        }

        // 2. Copy the `testLib` directory into the temp directory
        if (Files.exists(testLibDir)) {
            val tempLib = tempDir.resolve("testLib")
            Files.createDirectories(tempLib)

            Using.resource(Files.list(testLibDir)) { stream =>
                stream.forEach { p =>
                    if (Files.isRegularFile(p)) {
                        Files.copy(p, tempLib.resolve(p.getFileName), REPLACE_EXISTING)
                    }
                }
            }
        }

        // 3. Parse metadata from the original file
        val metadata = ImportTestParser.parseMetadata(sourceFile)

        // 4. Compile the main file within the sandbox
        val entryPoint = tempDir.resolve(mainFile)
        val res = wacc.frontend.WaccFrontend.run(entryPoint)
        val actualExit = code(res.exitCode)

        val expectedExit = metadata.expectedExitCode.getOrElse(0)

        // Check exit code
        if (actualExit != expectedExit) {
            println(s"FAILED $mainFile (Expected exit $expectedExit, got $actualExit)")
            res.errors.foreach(e => println(e.retrieveAllErrors()))
        }

        withClue(s"File: $mainFile\n") {
            actualExit shouldBe expectedExit
        }

        // Check output and error messages
        val actualOutput = res.errors.map(_.retrieveAllErrors()).mkString("\n").trim

        if (metadata.expectedError.nonEmpty) {
            val expectedLines =
                metadata.expectedError
                    .split("\n")
                    .map(_.trim)
                    .filter(_.nonEmpty)

            for (errLine <- expectedLines) {
                if (!actualOutput.contains(errLine)) {
                    println(
                        s"FAILED $mainFile:\n  Expected error containing: $errLine\n  Actual output: $actualOutput"
                    )
                    actualOutput should include(errLine)
                }
            }
        }
    }

    private def registerTests(dir: Path, testMsgPrefix: String): Unit = {
        if (Files.exists(dir)) {
            val waccFiles = Files.list(dir).iterator().asScala
                .filter(_.toString.endsWith(".wacc"))
                .toList
                .sortBy(_.getFileName.toString)

            for (file <- waccFiles) {
                val name = file.getFileName.toString
                it should s"$testMsgPrefix $name" in {
                    runSandboxed(file)
                }
            }
        }
    }

    behavior of "Import Integration tests (Valid)"
    registerTests(validDir, "compile successfully for valid test")

    behavior of "Import Integration tests (Invalid)"
    registerTests(invalidDir, "return status 100 and match error output for invalid test")
}