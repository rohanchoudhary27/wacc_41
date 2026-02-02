package wacc.frontend.generator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Shrink, Shrink.shrink
import org.scalacheck.Arbitrary
import wacc.frontend.generator.AstGenerator.*
import wacc._


class RandomSyntacticAstTest extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "Parser" should "correctly parse syntactically valid random ASTs" in {
    forAll("syntacticly valid AST") { (p: Program[SyntacticData]) =>
      parser.parse(Pretty.program(p)) == p
    }
  }

}

given Arbitrary[Program[SyntacticData]] = Arbitrary(genProgramSyntactic(5))

// given Shrink[Program[SyntacticData]] = Shrink(programShrink)

// def programShrink(p: Program[SyntacticData]): Stream[Program[SyntacticData]] = ???