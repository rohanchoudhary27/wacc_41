package wacc.frontend.integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import wacc.ExitCodes._

/** Integration tests for the WACC standard library modules. */
class StdlibIntegrationSpec extends AnyFlatSpec with Matchers {

    private val stdlibTestDir = Paths.get("AllIntegrationTests/custom/ExtensionsMilestone/Stdlib")

    private def registerTests(dir: Path): Unit = {
        if (Files.exists(dir)) {
            val waccFiles = Files.list(dir).iterator().asScala
                .filter(_.toString.endsWith(".wacc"))
                .toList
                .sortBy(_.getFileName.toString)

            for (file <- waccFiles) {
                val name = file.getFileName.toString
                val metadata = ImportTestParser.parseMetadata(file)
                val expectedExit = metadata.expectedExitCode.getOrElse(0)

                it should s"compile successfully for stdlib test $name" in {
                    val res = wacc.frontend.WaccFrontend.run(file)
                    val actualExit = code(res.exitCode)

                    if (actualExit != expectedExit) {
                        println(s"FAILED $name (Expected exit $expectedExit, got $actualExit)")
                        res.errors.foreach(e => println(e.retrieveAllErrors()))
                    }

                    withClue(s"File: $name\n") {
                        actualExit shouldBe expectedExit
                    }
                }
            }
        }
    }

    behavior of "Stdlib Integration tests"
    registerTests(stdlibTestDir)
}
