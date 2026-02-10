package wacc.frontend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*
import wacc.frontend.WaccFrontend

final class FrontendIntegrationSpec extends AnyFlatSpec with Matchers {

    /* File location and path info */
    private val scopeConfigPath: Path = Paths.get("config", "TestScope.txt")
    private val examplesRoot: Path = Paths.get("examples")
    private val programExtension: String = "wacc"
    private val enabledScopes: Set[String] = readScopes(scopeConfigPath)

    /**
     * Determines the expected exit code for a WACC file based on its path.
     * examples/valid/  => 0 (all valid programs should return 0)
     * examples/invalid/syntaxErr/ => 100 (programs with syntax errors should return 100)
     * examples/invalid/semanticErr/ => 200 (programs with semantic errors should return 200)
     * examples/invalid/whack/ => 100 (the prologue questions all have syntax errors in them) 
     * otherwise, returns a general compiler failure code.
     * 
     * @param file The path to the WACC source file
     * 
     * @return: the integer exit code defined in WaccFrontend
     */
    private def expectedStatus(file: Path): Int = {

        /* Make the file relative to examples root to get the scope */
        val rel = examplesRoot.relativize(file).toString 

        if (rel.startsWith("valid/")) WaccFrontend.successCode 
        else if (rel.startsWith("invalid/syntaxErr/") ||
                 rel.startsWith("invalid/whack/")) WaccFrontend.syntacticErrorCode
        else if (rel.startsWith("invalid/semanticErr/")) WaccFrontend.semanticErrorCode
        else WaccFrontend.compilerFailure
    }

    private val validDir = examplesRoot.resolve("valid")
    private val invalidDir = examplesRoot.resolve("invalid")

    /* Extract all the example WACC programs in the examples dir */
    private val allPrograms: Seq[Path] =
        getWaccProgramsUnder(validDir) ++ getWaccProgramsUnder(invalidDir)

    /* Display which tests have been enabled and disabled */
    private val (enabledPrograms, disabledPrograms) = allPrograms.partition(isEnabled)
    info(s"enabled programs: ${enabledPrograms.size}, disabled programs: ${disabledPrograms.size}")
    info(s"enabled scopes (${enabledScopes.size}): ${enabledScopes.toList.sorted.mkString(", ")}")

    /* Group test file paths based on test categories */
    private val grouped: Map[String, Seq[Path]] =
        allPrograms.groupBy { f =>

            /* Turn examples/{valid, invalid}/... into {valid, invalid}/... */
            val rel = examplesRoot.relativize(f).toString 

            /* Get the scope name (if the file is "invalid/syntaxError/basic/skpErr.wacc", 
               this gets "invalid/syntaxError") to be the groupBy key */
            rel.split('/').take(2).mkString("/") 
        }.map { case (k, v) => k -> v.sorted }

    /* Register each file as a scalatest test case */
    grouped.toSeq.sortBy(_._1).foreach { (groupName, files) =>
        behavior of s"wacc examples: $groupName"

        files.foreach { file =>

            /* Make the integration test expectation and title */
            val expected = expectedStatus(file)
            val testDecl = it should s"return status $expected for ${file.toString()}"

            /* Disable test or compile to perform test */
            if (!isEnabled(file)) {
                testDecl ignore { }
            } else {
                testDecl in {
                    val actual = WaccFrontend.run(file).exitCode
                    actual shouldBe expected
                }
            }
        }
    }

    /**
    * Checks if a file should be enabled for an integration test.
    * 
    * @param file: The path to the WACC file
    * @return: True if the file belongs to an enabled scope, false otherwise
    */ 
    private def isEnabled(file: Path): Boolean = {
        if (enabledScopes.isEmpty) false
        else {
            val fileRel = examplesRoot.relativize(file).toString
            val scopeKey = scopeFor(file)
            enabledScopes.exists { s =>
                val scope = s.stripSuffix("/")
                fileRel == scope || scopeKey == scope || scopeKey.startsWith(scope + "/")
            }
        }
    }

    /**
    * Extracts the parent directory of a file relative to the examples root.
    * Example: "examples/invalid/syntaxErr/basic/skpErr.wacc" -> "invalid/syntaxErr/basic"
    *
    * @param file: The path to the WACC file
    * 
    * @return: A string representation of the parent directory path
    */
    private def scopeFor(file: Path): String = {
        Option(file.getParent) match {
            case Some(parent) => examplesRoot.relativize(parent).toString
            case None => ""
        }
    }

    /**
    * Parses the scope config file to find the tests to run.
    * Filters commented lines (beginning with a '#')
    * 
    * @param path: The path to the TestScope.txt config file
    * 
    * @return A set of strings representing the active test scopes
    */
    private def readScopes(path: Path): Set[String] = {
        if (!Files.exists(path)) Set.empty
        else Files.readAllLines(path).asScala
            .map(s => s.takeWhile(_ != '#')) /* Treat '#' as a comment */
            .map(_.trim)
            .filter(s => s.nonEmpty)
            .toSet
    }

    /**
    * Returns all WACC files under a directory
    * @param dir: The root directory to start the search
    * 
    * @return: A sequence of paths pointing to WACC files
    */
    private def getWaccProgramsUnder(dir: Path): Seq[Path] = {
        if (!Files.isDirectory(dir)) Seq.empty
        else Files.walk(dir).toScala(List)
            .filter(p => Files.isRegularFile(p)) 
            .filter(p => extensionOf(p).contains(programExtension))
    }

    /**
    * Extracts the file extension from a path
    * @param p: The path to extract from
    * @return: An Option containing the extension string or None if no extension exists
    */
    private def extensionOf(p: Path): Option[String] = {
        val name = p.getFileName.toString
        val idx = name.lastIndexOf('.')
        if (idx < 0 || idx == name.length - 1) None else Some(name.substring(idx + 1))
    }
}
