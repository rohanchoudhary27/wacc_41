package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.parser
import wacc._

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
    private def expectDeclAt(input: String, i: Int): RValue[?] =
        stmtAt(input, i) match {
            case Stmt.Decl(_, _, rval, _) => rval
            case other                    => fail(s"Expected Decl at index $i but got: $other")
        }

     /** Parse and require the indexed statement to be a Decl; return the full Decl parts. */
    private def expectDeclStmtAt(input: String, i: Int): (SyntaxType, Ident[?], RValue[?]) =
        stmtAt(input, i) match {
            case Stmt.Decl(typ, ident, rval, _) => (typ, ident, rval)
            case other => fail(s"Expected Decl at index $i but got: $other")
        }
    
    /** Parse and require the indexed statement to be a read and return its rvalue */
    private def expectReadAt(input: String, i: Int): LValue[?] =
        stmtAt(input, i) match {
            case Stmt.Read(lval, _) => lval
            case other              => fail(s"Expected Read at index $i but got: $other")
        }
    
    /** Parse and require the indexed statement to be a assignment and return it */
    private def expectAsgnAt(input: String, i: Int): Stmt.Asgn[?] =
        stmtAt(input, i) match {
            case i @ Stmt.Asgn(_, _, _) => i
            case other              => fail(s"Expected Asgn at index $i but got: $other")
        }

    /** Parse and require the indexed statement to be print or println and return it */
    private def expectPrintOrPrintlnAt(input: String,
                                       i: Int): Either[Stmt.Print[?], Stmt.Println[?]] =
        stmtAt(input, i) match {
            case i @ Stmt.Print(_, _) => Left(i)
            case j @ Stmt.Println(_, _) => Right(j)
            case other              => fail(s"Expected Print/Println at index $i but got: $other")
        }

    /** Parse and require the indexed statement to be if and return it */
    private def expectIfAt(input: String, i: Int): Stmt.If[?] =
        stmtAt(input, i) match {
            case i @ Stmt.If(_, _, _, _) => i
            case other              => fail(s"Expected if at index $i but got: $other")
        }

    /** Parse and require the indexed statement to be while and return it */
    private def expectWhileAt(input: String, i: Int): Stmt.While[?] =
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

    /** Require rvalue to be a NewPair and return (e1, e2). */
    private def expectNewPair(rval: RValue[?]): (Expr[?], Expr[?]) =
        rval match {
            case NewPair(e1, e2, _) => (e1, e2)
            case other => fail(s"Expected NewPair(...) but got: $other")
        }

    /** Require expr to be a Pair literal `null`. */
    private def expectPairNull(expr: Expr[?]): PairLiter[?] =
        expr match {
            case p @ PairLiter(_) => p
            case other => fail(s"Expected PairLiter(null) but got: $other")
        }

    /** Require lvalue to be Ident(name, ...). */
    private def expectIdentLValue(lval: LValue[?], name: String): Ident[?] =
        lval match {
            case i @ Ident(n, _) if n == name => i
            case other => fail(s"Expected Ident lvalue $name but got: $other")
        }

    /** Require rvalue to be a PairElem (fst/snd) and return (isFst, inner lvalue). */
    private def expectPairElem(rval: RValue[?]): (Boolean, LValue[?]) =
        rval match {
            case PairElem(isFst, lval, _) => (isFst, lval)
            case other => fail(s"Expected PairElem(fst/snd ...) but got: $other")
        }

    /** Require lvalue to be a PairElem and return (isFst, inner lvalue). Useful for nesting. */
    private def expectPairElemLValue(lval: LValue[?]): (Boolean, LValue[?]) =
        lval match {
            case PairElem(isFst, inner, _) => (isFst, inner)
            case other => fail(s"Expected PairElem lvalue (fst/snd ...) but got: $other")
        }

    /* ==================== {READ} ==================== */
    "The read statement parser" should "parse read with an identifier" in {
        val input = "begin int x = 0; read x end"
        expectReadAt(input, 1)
    }

    it should "parse multiple read statements in sequence" in {
        val input = "begin int x = 0; char y = 'a'; read x; read y end"
        expectReadAt(input, 2)
        expectReadAt(input, 3)
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
        expectDeclAt(input, 0)
    }

    it should "parse a variable declaration with expression" in {
        val input = "begin int x = 5 + 3 * 2 end"
        expectDeclAt(input, 0)
    }

    it should "parse multiple declarations in a sequence" in {
        val input = "begin int x = 1; bool y = true; char c = 'a' end"
        expectDeclAt(input, 0)
        expectDeclAt(input, 1)
        expectDeclAt(input, 2)
    }

    it should "fail to parse declaration statement when the type is invalid" in {
        val input = "begin jamie x = 1; end"
        parseFail(input)
    }
    /* ==================== {/DECLARATION} ==================== */

    /* ==================== {ASSIGNMENT} ==================== */
    "The assignment statement parser" should "parse simple assignment to an int variable" in {
        val input = "begin int x = 10; x = 20 end"
        expectDeclAt(input, 0)
        expectAsgnAt(input, 1)
    }

    it should "parse multiple assignments in a sequence" in {
        val input = "begin int x = 10; x = 20; x = 30 end"
        expectDeclAt(input, 0)
        expectAsgnAt(input, 1)
        expectAsgnAt(input, 2)
    }

    it should "fail to parse assignment statement when the value is invalid" in {
        val input = "begin int x = 10; x = 2A3 end"
        parseFail(input)
    }
    /* ==================== {/ASSIGNMENT} ==================== */

    /* ==================== {PRINT(LN)} ==================== */
    "The print/println statement parser" should "parse print/println with an identifier" in {
        val input = "begin int x = 0; print x; println x end"
        expectPrintOrPrintlnAt(input, 1)
        expectPrintOrPrintlnAt(input, 2)
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
    "The if statement parser" should "parse if statements with valid cond and body statement" in {
        val input = "begin int x = 0; if true then x = 4+4 else x = 5+5 fi end"
        expectIfAt(input, 1)
    }
    

    it should "fail to parse if statements with a statement instead of an expression check" in {
        val input = "begin int x = 0; if println x then x = 4+4 else x = 5+5 fi end"
        parseFail(input)
    }

    it should "fail to parse if statements with an expression body instead of a statement" in {
        val input = "begin int x = 0; if true then 4+4 else 5+5 fi end"
        parseFail(input)
    }

    it should "fail to parse if statements without else" in {
        val input = "begin int x = 0; if true then x = 4+4 fi end"
        parseFail(input)
    }

     it should "fail to parse if statements without fi" in {
        val input = "begin int x = 0; if true then x = 4+4 else x = 5+5 end"
        parseFail(input)
    }
    /* ==================== {/IF} ==================== */
    
    /* ==================== {WHILE} ==================== */
    "The while statement parser" should "parse while statements with a valid cond and body" in {
        val input = "begin int x = 0; while true do x = 4+4 done end"
        expectWhileAt(input, 1)
    }

    it should "fail to parse while statements with a statement cond instead of an expression " in {
        val input = "begin int x = 0; while println x do x = 4+4 done end"
        parseFail(input)
    }

    it should "fail to parse while statements with an expression body instead of a statement" in {
        val input = "begin int x = 0; while true do 4+4 done end"
        parseFail(input)
    }

    it should "fail to parse while statements without do" in {
        val input = "begin int x = 0; while true x = 4+4 done end"
        parseFail(input)
    }

    it should "fail to parse while statements without done" in {
        val input = "begin int x = 0; while true do x = 4+4 end"
        parseFail(input)
    }
    /* ==================== {/WHILE} ==================== */

    /* ==================== {UNARY EXPRESSIONS} ==================== */
    "The unary operation expression parser" should "parse integer negation" in {
        val input = "begin int x = -5 end"
        val rval = expectDeclAt(input, 0)

        rval match {
            case UnaryExpr(UnaryOperator.Neg, IntLiter(5, _), _) => succeed
            case IntLiter(-5, _) => succeed
            case other => fail(s"Expected IntLiter(-5), got: $other")
        }
    }

    it should "parse boolean negation" in {
        val input = "begin bool b = !true end"
        val rval  = expectDeclAt(input, 0)
        rval match {
            case UnaryExpr(UnaryOperator.Not, BoolLiter(true, _), _) => succeed
            case other => fail(s"Expected Not(true), got: $other")
        }
    }

    it should "parse len on a string literal" in {
        val input = """begin int n = len "hello" end"""
        val rval  = expectDeclAt(input, 0)

        rval match {
            case UnaryExpr(UnaryOperator.Len, StrLiter("hello", _), _) => succeed
            case other => fail(s"""Expected Len("hello"), got: $other""")
        }
    }

    it should "bind unary stronger than binary" in {
        val input = "begin int x = -5 + 3 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Add, left, IntLiter(3, _), _) =>
            left match {
                case UnaryExpr(UnaryOperator.Neg, IntLiter(5, _), _) => succeed
                case IntLiter(-5, _)                                => succeed
                case other => fail(s"Expected Neg(5) or IntLiter(-5) on LHS, got: $other")
            }
            case other => fail(s"Expected Add(_, 3), got: $other")
        }
    }

    it should "fail when a unary operator is missing its operand" in {
        val input = "begin int x = - end"
        parseFail(input)
    }
    /* ==================== {/UNARY EXPRESSIONS} ==================== */

    /* ==================== {BINARY EXPRESSIONS} ==================== */
    "The binary operation expression parser" should "parse a simple addition expression" in {
        val input = "begin int x = 4 + 4 end"

        expectBlockLen(input, 1)
        val rval = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Add, IntLiter(4, _), IntLiter(4, _), _) => succeed
            case other => fail(s"Expected Add(4,4), got: $other")
        }
    }

    it should "respect precedence: * binds stronger than +" in {
        val input = "begin int x = 5 + 3 * 2 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Add, IntLiter(5, _), rhs, _) =>
            rhs match {
                case BinaryExpr(BinaryOperator.Mul, IntLiter(3, _), IntLiter(2, _), _) => succeed
                case other => fail(s"Expected Mul(3,2) on RHS, got: $other")
            }
            case other => fail(s"Expected Add(5, Mul(3,2)), got: $other")
        }
    }

    it should "respect left associativity for -" in {
        val input = "begin int x = 10 - 3 - 2 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Sub, left, IntLiter(2, _), _) =>
            left match {
                case BinaryExpr(BinaryOperator.Sub, IntLiter(10, _), IntLiter(3, _), _) => succeed
                case other => fail(s"Expected (10 - 3) on LHS, got: $other")
            }
            case other => fail(s"Expected (10 - 3) - 2, got: $other")
        }
    }

    it should "parse a boolean binary operator (&&)" in {
        val input = "begin bool b = true && false end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.And, BoolLiter(true, _),
                            BoolLiter(false, _), _) => succeed
            case other => fail(s"Expected true && false, got: $other")
        }
    }

    it should "fail when a binary operator is missing a right operand" in {
        val input = "begin int x = 4 + end"
        parseFail(input)
    }
    /* ==================== {/BINARY EXPRESSIONS} ==================== */

    /* ==================== {IDENTIFIERS} ==================== */
    "The identifier parser" should "parse a valid identifier usage in an expression" in {
        val input = "begin int x = 5; int y = x end"

        expectBlockLen(input, 2)
        val rval = expectDeclAt(input, 1)

        rval match {
            case Ident("x", _) => succeed
            case other         => fail(s"Expected identifier x, got: $other")
        }
    }

    it should "fail when parsing an invalid identifier" in {
        val input = "begin int 1x = 5 end"
        parseFail(input)
    }
    /* ==================== {/IDENTIFIERS} ==================== */

    /* ==================== {PAIRS} ==================== */

    "The pair parser" should "handle nested pairs and erased pair types" in {
        val input = "begin pair(int, pair) p = newpair(10, null) end"
        
        val (typ, _, rval) = expectDeclStmtAt(input, 0)

        typ shouldBe SyntaxType.Pair(SyntaxType.Int, SyntaxType.ErasedPair)

        val (_, e2) = expectNewPair(rval)
        expectPairNull(e2)
    }

    it should "parse nested pair extractions" in {
        val input = "begin pair(pair, int) p = null; int q = fst fst p end"

        val (typ, _, rval) = expectDeclStmtAt(input, 1)
        typ shouldBe SyntaxType.Int

        // rval = fst (fst p)
        val (isFstOuter, lvalOuter) = expectPairElem(rval)
        isFstOuter shouldBe true

        val (isFstInner, lvalInner) = expectPairElemLValue(lvalOuter)
        isFstInner shouldBe true

        expectIdentLValue(lvalInner, "p")
    }

    it should "parse newpair with complex expressions as arguments" in {
        val input = "begin pair(int, int) p = newpair(1 + 2, 3 * 4) end"
        val (typ, _, rval) = expectDeclStmtAt(input, 0)
        typ shouldBe SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int)

        expectNewPair(rval)
    }

    it should "fail to parse fst/snd followed by a non-lvalue" in {
        val input = "begin pair(int, int) p = fst 10 end"
        parseFail(input)
    }

    it should "fail to parse non-erased nested pairs" in {
        val input = "begin pair(pair(int, int), int) p = null"
        parseFail(input)
    }
    /* ==================== {/PAIRS} ==================== */
}
