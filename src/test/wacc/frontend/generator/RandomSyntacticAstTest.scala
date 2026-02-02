package wacc.frontend.generator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary
import wacc.frontend.generator.AstGenerator.*
import wacc.frontend.generator._
import wacc._

class RandomSyntacticAstTest extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

    "Parser" should "correctly parse syntactically valid random ASTs" in {
        forAll("syntactically valid AST") { (p: Program[SyntacticData]) =>
            parser.parse(Pretty.program(p)) match {
                case Left(msg) =>
                    fail(msg)
                case Right(syntacticAST) =>
                    withClue("\nINPUT:\n" + Pretty.program(p) + "\n") {
                        // programEq(syntacticAST, p)
                        Pretty.program(syntacticAST) shouldBe Pretty.program(p)
                    }
            }
        }
    }
}

given Arbitrary[Program[SyntacticData]] = Arbitrary(genProgramSyntactic(5))