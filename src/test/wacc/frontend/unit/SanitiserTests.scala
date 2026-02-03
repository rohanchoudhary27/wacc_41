import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc._

class SanitiserTests extends AnyFlatSpec with Matchers {

    "The Sanitiser Pass" should "allow program strings with all ASCII chars" in {
        val progStr = "begin int x = 0; if x > 5 then print 1 else print 2 fi end"
        val errorLogger = ErrorLogger()

        Sanitiser.sanitise(progStr, errorLogger)

        errorLogger.hasErrors() shouldBe false
    }

    it should "emit a syntax error on a non-ASCII char in the program string" in {
        val progStr = "begin ínt x = 5; print x end"
        val errorLogger = ErrorLogger()

        Sanitiser.sanitise(progStr, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Syntax error")
    }
}
