package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.SemanticErrorLogger

class SemanticErrorTests extends AnyFlatSpec with Matchers {

    "A SemanticErrorLogger" should "store errors passed to it" in {
      val logger = SemanticErrorLogger()

      val msgOne = "some error message"
      val posOne = (2,4)

      val msgTwo = "what? another error message?"
      val posTwo = (5,2)

      logger.logError(posOne, msgOne)
      logger.logError(posTwo, msgTwo)

      logger.hasErrors() shouldBe true
      val retrievedErrors = logger.retrieveAllErrors()

      retrievedErrors(0).msg shouldBe msgOne
      retrievedErrors(0).pos shouldBe posOne
      retrievedErrors(1).msg shouldBe msgTwo
      retrievedErrors(1).pos shouldBe posTwo
    }

    it should "start off emtpy" in {
      val logger = SemanticErrorLogger()
      logger.hasErrors() shouldBe (false)
      logger.retrieveAllErrors() shouldBe List()
    }
}
