package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.parser

class ExpressionUnitTest extends AnyFlatSpec {
    "parser.parse" should "correctly parse add/subtract expressions" in {
        parser.parse("3 + (4 - 2)").get shouldBe 5
    }
}
