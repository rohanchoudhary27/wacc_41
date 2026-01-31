package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.parser
import wacc.{Stmt, Program}
import wacc.{BinaryExpr, BinaryOperator, UnaryExpr, UnaryOperator, Ident, BoolLiter, StrLiter, IntLiter, RValue, LValue}

class ParserTests extends AnyFlatSpec with Matchers {

  /** parse MUST succeed or fail the test with a message */
  private def parseOk(input: String): Program[?] =
    parser.parse(input) match {
      case Right(p) => p
      case Left(e)  => fail(s"Expected parse success but got error: $e\nInput:\n$input")
    }

  /** Parse MUST fail or fail the test with a useful message */
  private def parseFail(input: String): Unit =
    parser.parse(input) match {
      case Left(_)  => ()
      case Right(p) => fail(s"Expected parse failure but got: $p\nInput:\n$input")
    }

  /** Extract top block statement list or fail if the program isn't a block */
  private def stmtsOf(program: Program[?]): Seq[Stmt[?]] =
    program.stmt match {
      case Stmt.Block(stmts, _) => stmts
      case other                => fail(s"Expected Block statement but got: $other")
    }

  /** Parse and return the indexed statement in the statements list of top level block */
  private def stmtAt(input: String, i: Int): Stmt[?] = {
    val stmts = stmtsOf(parseOk(input))
    if (i < 0 || i >= stmts.length)
      fail(s"Expected at least ${i + 1} statements but got ${stmts.length}\nInput:\n$input")
    stmts(i)
  }

  /** Parse and require the indexed statement to be a Decl and return its rvalue */
  private def declRValueAt(input: String, i: Int): RValue[?] =
    stmtAt(input, i) match {
      case Stmt.Decl(_, _, rval, _) => rval
      case other                    => fail(s"Expected Decl at index $i but got: $other")
    }
  
  /** Parse and require the indexed statement to be a read and return its rvalue */
  private def readAt(input: String, i: Int): LValue[?] =
    stmtAt(input, i) match {
      case Stmt.Read(lval, _) => lval
      case other              => fail(s"Expected Read at index $i but got: $other")
    }
  
  /** Parse and require the indexed statement to be a assignment and return it */
  private def asgnAt(input: String, i: Int): Stmt.Asgn[?] =
    stmtAt(input, i) match {
      case i @ Stmt.Asgn(_, _, _) => i
      case other              => fail(s"Expected Asgn at index $i but got: $other")
    }

   /** Parse and require the indexed statement to be print or println and return it */
  private def printOrPrintlnAt(input: String, i: Int): Either[Stmt.Print[?], Stmt.Println[?]] =
    stmtAt(input, i) match {
      case i @ Stmt.Print(_, _) => Left(i)
      case j @ Stmt.Println(_, _) => Right(j)
      case other              => fail(s"Expected Print/Println at index $i but got: $other")
    }

  /** Parse and require the indexed statement to be if and return it */
  private def ifAt(input: String, i: Int): Stmt.If[?] =
    stmtAt(input, i) match {
      case i @ Stmt.If(_, _, _, _) => i
      case other              => fail(s"Expected if at index $i but got: $other")
    }

  /** Parse and require the indexed statement to be while and return it */
  private def whileAt(input: String, i: Int): Stmt.While[?] =
    stmtAt(input, i) match {
      case i @ Stmt.While(_, _, _) => i
      case other              => fail(s"Expected while at index $i but got: $other")
    }

  /** Parse and require the top block to have n statements */
  private def expectBlockLen(input: String, n: Int): Seq[Stmt[?]] = {
    val stmts = stmtsOf(parseOk(input))
    stmts.length shouldBe n
    stmts
  }

    /* ==================== {READ} ==================== */
    "The read statement parser" should "parse read with an identifier" in {
      val input = "begin int x = 0; read x end"
      readAt(input, 1)
    }

    it should "parse multiple read statements in sequence" in {
      val input = "begin int x = 0; char y = 'a'; read x; read y end"
      readAt(input, 2)
      readAt(input, 3)
    }

    it should "fail when read has no lvalue" in {
      val input = "begin read end"
      parseFail(input)
    }

    it should "fail when read is followed by an expression instead of lvalue" in {
      val input = "begin read 5 end"
      parseFail(input)
    }
    /* ==================== {/READ} ==================== */

    /* ==================== {DECLARATION} ==================== */
    "The declaration statement parser" should "parse a simple integer variable declaration" in {
      val input = "begin int x = 1 end"
      declRValueAt(input, 0)
    }

    it should "parse a variable declaration with expression" in {
      val input = "begin int x = 5 + 3 * 2 end"
      declRValueAt(input, 0)
    }

    it should "parse multiple declarations in a sequence" in {
      val input = "begin int x = 1; bool y = true; char c = 'a' end"
      declRValueAt(input, 0)
      declRValueAt(input, 1)
      declRValueAt(input, 2)
    }

    it should "fail to parse declaration statement when the type is invalid" in {
      val input = "begin jamie x = 1; end"
      parseFail(input)
    }
    /* ==================== {/DECLARATION} ==================== */

    /* ==================== {ASSIGNMENT} ==================== */
    "The assignment statement parser" should "parse simple assignment to an int variable" in {
      val input = "begin int x = 10; x = 20 end"
      declRValueAt(input, 0)
      asgnAt(input, 1)
    }

    it should "parse multiple assignments in a sequence" in {
      val input = "begin int x = 10; x = 20; x = 30 end"
      declRValueAt(input, 0)
      asgnAt(input, 1)
      asgnAt(input, 2)
    }

    it should "fail to parse assignment statement when the value is invalid" in {
      val input = "begin int x = 10; x = 2A3 end"
      parseFail(input)
    }
    /* ==================== {/ASSIGNMENT} ==================== */

    /* ==================== {PRINT(LN)} ==================== */
    "The print/println statement parser" should "parse print/println with an identifier" in {
      val input = "begin int x = 0; print x; println x end"
      val stmts = expectBlockLen(input, 3);

      stmts(1) match {
        case Stmt.Print(_, _) => succeed
        case other            => fail(s"Second statement should be Print, got: $other")
      }

      stmts(2) match {
        case Stmt.Println(_, _) => succeed
        case other              => fail(s"Third statement should be Println, got: $other")
      }
    }

    it should "fail when print has no expression" in {
      val input = "begin print end"
      parseFail(input)
    }

    it should "fail when println has no expression" in {
      val input = "begin println end"
      parseFail(input)
    }

    it should "fail when print is followed by a statement instead of an expression" in {
      val input = "begin print x = 4 end"
      parseFail(input)
    }

    it should "fail when println is followed by a statement instead of an expression" in {
      val input = "begin println x = 4 end"
      parseFail(input)
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
