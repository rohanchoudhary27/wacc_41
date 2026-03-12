package wacc.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*

import java.nio.file.{Files, Path, Paths}
import wacc.ExitCodes._


abstract class WaccIntegrationTestBase(
    protected val configurationPath: Path
) extends AnyFlatSpec with Matchers {
    /* File location and path info */
    private val rootTestPath = Paths.get("AllIntegrationTests")
    private val programExtension = "wacc"
    
     /* Valid and invalid test scopes */
    private val validDir = rootTestPath.resolve("examples/valid")
    private val invalidDir = rootTestPath.resolve("examples/invalid")
    private val customDir = rootTestPath.resolve("custom")

    /* Directories under custom/ that have their own dedicated test specs */
    private val customExcludedDirs: Set[Path] = Set(
        rootTestPath.resolve("custom/ExtensionsMilestone/Imports"),
        rootTestPath.resolve("custom/ExtensionsMilestone/Stdlib")
    )
      
    /* All the example WACC programs in the examples dir */
    private val allPrograms: Seq[Path] =
        getWaccProgramsUnder(validDir) ++
        getWaccProgramsUnder(invalidDir) ++
        getWaccProgramsUnder(customDir, customExcludedDirs)

    protected val enabledScopes: Set[String]
        = readScopes(configurationPath)

     /* Display which tests have been enabled and disabled */
    private val (enabledPrograms, disabledPrograms) = allPrograms.partition(isEnabled)
    info(s"enabled programs: ${enabledPrograms.size}, disabled programs: ${disabledPrograms.size}")
    info(s"enabled scopes (${enabledScopes.size}): ${enabledScopes.toList.sorted.mkString(", ")}")

    /* Group test file paths based on test categories and extract metadata */
    protected def groupTestsBy[A](
        action: (String, Seq[Path]) => (String, Seq[A])
    ): Map[String, Seq[A]] =
        allPrograms.groupBy { f =>

            /* Turn examples/{valid, invalid}/... into {valid, invalid}/... */
            val rel = rootTestPath.relativize(f).toString 

            /* Get the scope name (if the file is "invalid/syntaxError/basic/skpErr.wacc", 
               this gets "invalid/syntaxError") to be the groupBy key */
            rel.split('/').take(3).mkString("/") 
        }.map { (k, v) => { action(k, v) } }
        .toMap

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
    protected def expectedStatus(file: Path): Int = {

        /* Make the file relative to examples root to get the scope */
        val rel = rootTestPath.relativize(file).toString 

        if (rel.startsWith("examples/invalid/syntaxErr/") ||
            rel.startsWith("examples/invalid/whack/")) code(SyntaxError)
        else if (rel.startsWith("examples/invalid/semanticErr/")) code(SemanticError)
        else if (rel.startsWith("examples/valid/")) code(CompilerSuccess)
        /* Custom tests: infer from valid/ or invalid/ in path */
        else if (rel.startsWith("custom/") && rel.contains("/invalid/")) code(SemanticError)
        else if (rel.startsWith("custom/")) code(CompilerSuccess)
        else code(CompilerFailure)
    }

    /**
     * Checks if a file should be enabled for an integration test.
     * 
     * @param file: The path to the WACC file
     * @return: True if the file belongs to an enabled scope, false otherwise
     */ 
    protected def isEnabled(file: Path): Boolean = {

        val fileRel = rootTestPath.relativize(file).toString

        /* Don't run advanced tests on pipeline */
        if (fileRel.startsWith("examples/valid/advanced/")) {
            false
        } else {

            /* Enable if the scope is enabled */
            if (enabledScopes.isEmpty) false
            else {
                val scopeKey = scopeFor(file)
                enabledScopes.exists { s =>
                    val scope = s.stripSuffix("/")
                    fileRel == scope || scopeKey == scope || scopeKey.startsWith(scope + "/")
                }
            }
        }
    }

    /**
    * Parses the scope config file to find the tests to run.
    * Filters commented lines (beginning with a '#')
    * 
    * @param path: The path to the FrontendTestScope.txt config file
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
     * Extracts the parent directory of a file relative to the examples root.
     * Example: "examples/invalid/syntaxErr/basic/skpErr.wacc" -> "invalid/syntaxErr/basic"
     *
     * @param file: The path to the WACC file
     * 
     * @return: A string representation of the parent directory path
     */
    private def scopeFor(file: Path): String = {
        Option(file.getParent) match {
            case Some(parent) => rootTestPath.relativize(parent).toString
            case None => ""
        }
    }

    /**
     * Returns all WACC files under a directory
     * @param dir: The root directory to start the search
     * 
     * @return: A sequence of paths pointing to WACC files
     */
    private def getWaccProgramsUnder(dir: Path, excludeDirs: Set[Path] = Set.empty): Seq[Path] = {
        if (!Files.isDirectory(dir)) Seq.empty
        else Files.walk(dir).toScala(List)
            .filter(p => Files.isRegularFile(p))
            .filter(p => !excludeDirs.exists(ex => p.startsWith(ex)))
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
