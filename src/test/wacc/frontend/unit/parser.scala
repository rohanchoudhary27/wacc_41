package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.parser
import wacc.Stmt

class ParserTests extends AnyFlatSpec with Matchers {

    "The parser" should "parse a program with a single declaration" in {
      pending // Parser implementation not yet complete
    }

    /* ==================== {READ} ==================== */
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
    /* ==================== {/READ} ==================== */

    /* ==================== {DECLARATION} ==================== */
    "The declaration statement parser" should "parse a simple integer variable declaration" in {
      val input = "begin int x = 1 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 1
          stmts(0) shouldBe a[Stmt.Decl[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse a variable declaration with expression" in {
      val input = "begin int x = 5 + 3 * 2 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 1
          stmts(0) shouldBe a[Stmt.Decl[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse multiple declarations in a sequence" in {
      val input = "begin int x = 1; bool y = true; char c = 'a' end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 3
          stmts(0) shouldBe a[Stmt.Decl[?]]
          stmts(1) shouldBe a[Stmt.Decl[?]]
          stmts(2) shouldBe a[Stmt.Decl[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail to parse declaration statement when the type is invalid" in {
      val input = "begin jamie x = 1; end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/DECLARATION} ==================== */

    /* ==================== {ASSIGNMENT} ==================== */
    "The assignment statement parser" should "parse simple assignment to an int variable" in {
      val input = "begin int x = 10; x = 20 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 2
          stmts(0) shouldBe a[Stmt.Decl[?]]
          stmts(1) shouldBe a[Stmt.Asgn[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse multiple assignments in a sequence" in {
      val input = "begin int x = 10; x = 20; x = 30 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 3
          stmts(0) shouldBe a[Stmt.Decl[?]]
          stmts(1) shouldBe a[Stmt.Asgn[?]]
          stmts(2) shouldBe a[Stmt.Asgn[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail to parse assignment statement when the value is invalid" in {
      val input = "begin int x = 10; x = 2A3 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/ASSIGNMENT} ==================== */

    /* ==================== {PRINT(LN)} ==================== */
    "The print/println statement parser" should "parse print/println with an identifier" in {
      val input = "begin int x = 0; print x; println x end"
      val result = parser.parse(input)
      result.isRight shouldBe true
      
      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 3
          stmts(1) match {
            case Stmt.Print(_, _) => succeed
            case _ => fail("Second statement should be Print")
          }
          stmts(2) match {
            case Stmt.Println(_, _) => succeed
            case _ => fail("Second statement should be Println")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail when print has no expression" in {
      val input = "begin print end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail when println has no expression" in {
      val input = "begin println end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail when print is followed by a statement instead of an expression" in {
      val input = "begin print x = 4 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail when println is followed by a statement instead of an expression" in {
      val input = "begin println x = 4 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/PRINT(LN)} ==================== */

    /* ==================== {IF} ==================== */
    /* ==================== {/IF} ==================== */
    
    /* ==================== {WHILE} ==================== */
    /* ==================== {/WHILE} ==================== */

    /* ==================== {UNARY EXPRESSIONS} ==================== */
    /* ==================== {/UNARY EXPRESSIONS} ==================== */

    /* ==================== {BINARY EXPRESSIONS} ==================== */
    /* ==================== {/BINARY EXPRESSIONS} ==================== */


}
