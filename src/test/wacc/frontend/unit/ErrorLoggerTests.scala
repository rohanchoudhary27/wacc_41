package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend.ErrorLogger

class ErrorLoggerTests extends AnyFlatSpec with Matchers {

    "A ErrorLogger" should "store and format semantic errors passed to it" in {

        val logger = new ErrorLogger()

        val msgOne = "some error message"
        val posOne = (2, 4)

        val msgTwo = "what? another error message?"
        val posTwo = (5, 2)

        logger.logSemanticError(posOne, msgOne)
        logger.logSemanticError(posTwo, msgTwo)

        logger.hasErrors() shouldBe true
        
        /* First error position should be accessible */
        logger.firstErrorPos() shouldBe (2, 4)

        /* Message content should include error messages */
        val allErrors = logger.retrieveAllErrors()
        allErrors should include (msgOne)
        allErrors should include (msgTwo)
    }

    it should "store and format syntax errors passed to it" in {
        val logger = new ErrorLogger()
        val syntaxMsg = "unexpected end of input"

        logger.logSyntaxError(syntaxMsg)

        logger.hasErrors() shouldBe true
        val retrievedErrors = logger.retrieveAllErrors()

        retrievedErrors should include (syntaxMsg)
    }

    it should "store and format mixed syntax and semantic errors" in {
        val logger = new ErrorLogger()
        val syntaxMsg = "syntax error here"
        val semanticMsg = "semantic error there"
        val pos = (1, 1)

        logger.logSyntaxError(syntaxMsg)
        logger.logSemanticError(pos, semanticMsg)

        logger.hasErrors() shouldBe true
        val retrievedErrors = logger.retrieveAllErrors()

        retrievedErrors should include (syntaxMsg)
        retrievedErrors should include (semanticMsg)
    }

    it should "start off empty" in {
        val logger = new ErrorLogger()
        logger.hasErrors() shouldBe false
        
        // Check for empty string instead of empty List
        logger.retrieveAllErrors() shouldBe "" 
    }
}