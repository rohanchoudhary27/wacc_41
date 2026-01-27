package wacc.frontend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.StreamConverters.*
import scala.jdk.CollectionConverters.*
import scala.io.Source
import wacc.parser
import wacc.TypeChecker
import wacc.Renamer
import wacc.SemanticErrorLogger

final class FrontendIntegrationSpec extends AnyFlatSpec with Matchers {
    private val scopeConfigPath: Path = Paths.get("config", "test-scope.txt")

    private val examplesRoot: Path = Paths.get("examples")
    
    private val programExtension: String = "wacc"

    private val enabledScopes: Set[String] = readScopes(scopeConfigPath)

    // Runs the frontend pipeline against an integration test
    private object FrontendCompilerDriver {
        def frontendCompile(path: Path): Int = {

            val source = Source.fromFile(path.toFile)
            val sourceCode = try source.mkString finally source.close()

            val result = parser.parse(sourceCode)

            result match {
                case Left(_) => 100
                
                case Right(syntacticAST) =>
                    val semanticErrors = new SemanticErrorLogger()
                    val renamer = new Renamer()
                    val renamedAST = renamer.rename(syntacticAST, semanticErrors)

                    val funcTable = renamedAST.info match {
                        case wacc.SemanticData.TopLevel(ft) => ft
                        case _ => Map.empty[String, wacc.SemanticData.Func]
                    }

                    val typeChecker = new TypeChecker()
                    typeChecker.check(renamedAST, semanticErrors, funcTable)

                    if (semanticErrors.hasErrors()) 200 // Semantic Error
                    else 0                              // Success
            }
        }
    }

    // expected status codes:
    //  examples/valid/**  => 0 (all valid programs should return 0)
    //  examples/invalid/syntaxErr/** => 100 (all programs with syntax errors should return 100)
    //  examples/invalid/semanticErr/** => 200 (all programs with semantic errors should return 200)
    private def expectedStatus(file: Path): Int = {
        // make the file relative to examples root to get the scope
        val rel = examplesRoot.relativize(file).toString 

        if (rel.startsWith("valid/")) 0
        else if (rel.startsWith("invalid/syntaxErr/")) 100
        else if (rel.startsWith("invalid/semanticErr/")) 200
        else 1
    }

    private val validDir = examplesRoot.resolve("valid")
    private val invalidDir = examplesRoot.resolve("invalid")

    // extract all the example WACC programs in the examples dir
    private val allPrograms: Seq[Path] =
        getWaccProgramsUnder(validDir) ++ getWaccProgramsUnder(invalidDir)

    private val (enabledPrograms, disabledPrograms) = allPrograms.partition(isEnabled)
    info(s"enabled programs: ${enabledPrograms.size}, disabled programs: ${disabledPrograms.size}")
    info(s"enabled scopes (${enabledScopes.size}): ${enabledScopes.toList.sorted.mkString(", ")}")

    private val grouped: Map[String, Seq[Path]] =
        allPrograms.groupBy { f =>
            // turn examples/{valid, invalid}/... into {valid, invalid}/...
            val rel = examplesRoot.relativize(f).toString 
            // get the scope name (if the file is "invalid/syntaxError/basic/skpErr.wacc", 
            // this gets "invalid/syntaxError")
            rel.split('/').take(2).mkString("/") 
        }.map { case (k, v) => k -> v.sorted }

    grouped.toSeq.sortBy(_._1).foreach { (groupName, files) =>
        behavior of s"wacc examples: $groupName"

        files.foreach { file =>
            val expected = expectedStatus(file)
            val testDecl = it should s"return status $expected for ${file.toString()}"

            if (!isEnabled(file)) {
                testDecl ignore { }
            } else {
                testDecl in {
                    val actual = FrontendCompilerDriver.frontendCompile(file)
                    actual shouldBe expected
                }
            }
        }
    }

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

    // scope key is the parent directory relative to examples
    // e.g. examples/invalid/syntaxErr/basic/skpErr.wacc -> "invalid/syntaxErr/basic"
    private def scopeFor(file: Path): String = {
        Option(file.getParent) match {
            case Some(parent) => examplesRoot.relativize(parent).toString
            case None => ""
        }
    }

    // read the scopes in the configuration file
    private def readScopes(path: Path): Set[String] = {
        if (!Files.exists(path)) Set.empty
        else Files.readAllLines(path).asScala
            .map(s => s.takeWhile(_ != '#')) // treat '#' as a comment
            .map(_.trim)
            .filter(s => s.nonEmpty)
            .toSet
    }

    // return all .wacc files under dir
    private def getWaccProgramsUnder(dir: Path): Seq[Path] = {
        if (!Files.isDirectory(dir)) Seq.empty
        else Files.walk(dir).toScala(List)
            .filter(p => Files.isRegularFile(p)) 
            .filter(p => extensionOf(p).contains(programExtension))
    }

    private def extensionOf(p: Path): Option[String] = {
        val name = p.getFileName.toString
        val idx = name.lastIndexOf('.')
        if (idx < 0 || idx == name.length - 1) None else Some(name.substring(idx + 1))
    }
}
