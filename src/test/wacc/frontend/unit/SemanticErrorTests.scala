package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.SemanticErrorLogger

class SemanticErrorTests extends AnyFlatSpec with Matchers {

    "A SemanticErrorLogger" should "store and format errors passed to it" in {

      val logger = new SemanticErrorLogger()

      val msgOne = "some error message"
      val posOne = (2, 4)

      val msgTwo = "what? another error message?"
      val posTwo = (5, 2)

      logger.logError(posOne, msgOne)
      logger.logError(posTwo, msgTwo)

      logger.hasErrors() shouldBe true
      val retrievedErrors = logger.retrieveAllErrors()

      retrievedErrors should include (msgOne)
      retrievedErrors should include (msgTwo)

      retrievedErrors should include ("2") 
      retrievedErrors should include ("4")
      retrievedErrors should include ("5")
    }

    it should "start off empty" in {
      val logger = new SemanticErrorLogger()
      logger.hasErrors() shouldBe false
      
      // Check for empty string instead of empty List
      logger.retrieveAllErrors() shouldBe "" 
    }
}