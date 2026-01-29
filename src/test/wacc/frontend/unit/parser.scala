package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.parser
import wacc.Stmt
import wacc.{BinaryExpr, BinaryOperator, UnaryExpr, UnaryOperator, Ident, BoolLiter, StrLiter, IntLiter, RValue}

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
     "The if statement parser" should "parse if statements with a valid condition expression and body statement" in {
      val input = "begin int x = 0; if true then x = 4+4 else x = 5+5 fi end"
      val result = parser.parse(input)
      result.isRight shouldBe true
      
      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 2
          stmts(1) shouldBe a[Stmt.If[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail to parse if statements with a statement check instead of an expression check" in {
      val input = "begin int x = 0; if println x then x = 4+4 else x = 5+5 fi end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail to parse if statements with an expression body instead of a statement" in {
      val input = "begin int x = 0; if true then 4+4 else 5+5 fi end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail to parse if statements without else" in {
      val input = "begin int x = 0; if true then x = 4+4 fi end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

     it should "fail to parse if statements without fi" in {
      val input = "begin int x = 0; if true then x = 4+4 else x = 5+5 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/IF} ==================== */
    
    /* ==================== {WHILE} ==================== */
     "The while statement parser" should "parse while statements with a valid condition expression and body statement" in {
      val input = "begin int x = 0; while true do x = 4+4 done end"
      val result = parser.parse(input)
      result.isRight shouldBe true
      
      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 2
          stmts(1) shouldBe a[Stmt.While[?]]
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail to parse while statements with a statement condition instead of an expression condition" in {
      val input = "begin int x = 0; while println x do x = 4+4 done end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail to parse while statements with an expression body instead of a statement" in {
      val input = "begin int x = 0; while true do 4+4 done end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

    it should "fail to parse while statements without do" in {
      val input = "begin int x = 0; while true x = 4+4 done end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }

     it should "fail to parse while statements without done" in {
      val input = "begin int x = 0; while true do x = 4+4 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/WHILE} ==================== */

    /* ==================== {UNARY EXPRESSIONS} ==================== */
    "The unary operation expression parser" should "parse integer negation" in {
      val input = "begin int x = -5 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              alert("parser now handles -5 as an integer literal, do we want it to behave like that or treat `-` as a unary operator?")
              rval match {
                case UnaryExpr(UnaryOperator.Neg, IntLiter(5, _), _) => succeed
                case IntLiter(-5, info) => succeed
                case other => fail(s"Expected Neg(5), got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse boolean negation" in {
      val input = "begin bool b = !true end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case UnaryExpr(UnaryOperator.Not, BoolLiter(true, _), _) => succeed
                case other => fail(s"Expected Not(true), got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse len on a string literal" in {
      val input = """begin int n = len "hello" end"""
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case UnaryExpr(UnaryOperator.Len, StrLiter("hello", _), _) => succeed
                case other => fail(s"""Expected Len("hello"), got: $other""")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "bind unary stronger than binary" in {
      val input = "begin int x = -5 + 3 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case BinaryExpr(BinaryOperator.Add, left, IntLiter(3, _), _) =>
                  alert("parser now handles -5 as an integer literal, do we want it to behave like that or treat `-` as a unary operator?")
                  left match {
                    case UnaryExpr(UnaryOperator.Neg, IntLiter(5, _), _) => succeed
                    case IntLiter(-5, info) => succeed
                    case other => fail(s"Expected Neg(5) on LHS, got: $other")
                  }
                case other => fail(s"Expected Add(Neg(5),3), got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail when a unary operator is missing its operand" in {
      val input = "begin int x = - end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/UNARY EXPRESSIONS} ==================== */

    /* ==================== {BINARY EXPRESSIONS} ==================== */
    "The binary operation expression parser" should "parse a simple addition expression" in {
      val input = "begin int x = 4 + 4 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 1
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case BinaryExpr(BinaryOperator.Add, IntLiter(4, _), IntLiter(4, _), _) => succeed
                case other => fail(s"Expected Add(4,4), got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "respect precedence: * binds stronger than +" in {
      val input = "begin int x = 5 + 3 * 2 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case BinaryExpr(BinaryOperator.Add, IntLiter(5, _), rhs, _) =>
                  rhs match {
                    case BinaryExpr(BinaryOperator.Mul, IntLiter(3, _), IntLiter(2, _), _) => succeed
                    case other => fail(s"Expected Mul(3,2) on RHS, got: $other")
                  }
                case other => fail(s"Expected Add(5, Mul(3,2)), got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "respect left associativity for -" in {
      val input = "begin int x = 10 - 3 - 2 end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case BinaryExpr(BinaryOperator.Sub, left, IntLiter(2, _), _) =>
                  left match {
                    case BinaryExpr(BinaryOperator.Sub, IntLiter(10, _), IntLiter(3, _), _) => succeed
                    case other => fail(s"Expected (10 - 3) on LHS, got: $other")
                  }
                case other => fail(s"Expected (10 - 3) - 2, got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "parse a boolean binary operator (&&)" in {
      val input = "begin bool b = true && false end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts(0) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case BinaryExpr(BinaryOperator.And, BoolLiter(true, _), BoolLiter(false, _), _) => succeed
                case other => fail(s"Expected true && false, got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail when a binary operator is missing a right operand" in {
      val input = "begin int x = 4 + end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/BINARY EXPRESSIONS} ==================== */

    /* ==================== {IDENTIFIERS} ==================== */
    "The identifier parser" should "parse a valid identifier usage in an expression" in {
      val input = "begin int x = 5; int y = x end"
      val result = parser.parse(input)
      result.isRight shouldBe true

      val program = result.toOption.get
      program.stmt match {
        case Stmt.Block(stmts, _) =>
          stmts.length shouldBe 2
          stmts(1) match {
            case Stmt.Decl(_, _, rval, _) =>
              rval match {
                case Ident("x", _) => succeed
                case other => fail(s"Expected identifier x, got: $other")
              }
            case other => fail(s"Expected Decl, got: $other")
          }
        case _ => fail("Expected Block statement")
      }
    }

    it should "fail when parsing an invalid identifier" in {
      val input = "begin int 1x = 5 end"
      val result = parser.parse(input)
      result.isLeft shouldBe true
    }
    /* ==================== {/IDENTIFIERS} ==================== */
}
