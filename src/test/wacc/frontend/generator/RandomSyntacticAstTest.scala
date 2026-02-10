package wacc.frontend.generator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary
import wacc.frontend.generator.AstGenerator.*
import wacc.frontend.generator.SyntacticASTMatchers._
import wacc.frontend.generator._
import wacc.frontend._

class RandomSyntacticAstTest extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

    "Parser" should "correctly parse syntactically valid random ASTs" in {
        forAll("syntactically valid AST") { (generatedAST: Program[SyntacticData]) =>
            parser.parse(Pretty.program(generatedAST)) match {
                case Left(msg) =>
                    fail(msg)
                case Right(parserAST) => {
                    withClue("\nINPUT:\n" + Pretty.program(generatedAST) + "\n") {
                        parserAST should matchProgram (generatedAST)
                    }
                }
            }
        }
    }
}

given Arbitrary[Program[SyntacticData]] = Arbitrary(genProgramSyntactic(5))