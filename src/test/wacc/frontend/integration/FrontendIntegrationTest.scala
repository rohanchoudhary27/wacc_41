package wacc.frontend.integration

import java.nio.file.{Path, Paths}
import wacc.frontend.WaccFrontend
import wacc.ExitCodes._
import wacc.integration.WaccIntegrationTestBase

private val FRONTEND_INTEGRATION_TEST_CONFIGURATION: Path = Paths.get("config", "FrontendTestScope.txt")

final class FrontendIntegrationSpec extends WaccIntegrationTestBase(
    FRONTEND_INTEGRATION_TEST_CONFIGURATION
) {

    /* Group test file paths based on test categories */
    private val grouped: Map[String, Seq[Path]] = groupTestsBy { (k, v) => k -> v.sorted }

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
                    val actual = code(WaccFrontend.run(file).exitCode)
                    actual shouldBe expected
                }
            }
        }
    }
}
