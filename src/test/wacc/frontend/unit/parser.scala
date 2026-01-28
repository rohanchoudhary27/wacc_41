package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.parser
import wacc.Stmt

class ParserTests extends AnyFlatSpec with Matchers {

    "The parser" should "parse a program with a single declaration" in {
      pending // Parser implementation not yet complete
    }

    "The read statement parser" should "parse read with an identifier" in {
      val input = "begin int x = 0; read x end"
      val result = parser.parse(input)
      result.isRight shouldBe true
      
      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 2
          stmts(1) match {
            case Stmt.Read(_, _) => succeed
            case _ => fail("Second statement should be Read")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse multiple read statements in sequence" in {
      val input = "begin int x = 0; char y = 'a'; read x; read y end"
      val result = parser.parse(input)
      result.isRight shouldBe true
      
      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 4
          stmts(2) shouldBe a[Stmt.Read[?]]
          stmts(3) shouldBe a[Stmt.Read[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail when read has no lvalue" in {
      val input = "begin read end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail when read is followed by an expression instead of lvalue" in {
      val input = "begin read 5 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
}
