package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.frontend.parser
import wacc.frontend._

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

    /* ==================== {FREE} ==================== */

    "The free statement parser" should "parse free with an identifier" in {
        val input = "begin free x end"
        stmtAt(input, 0) match {
            case Stmt.Free(Ident("x", _), _) => succeed
            case other => fail(s"Expected Free(Ident(x)) but got: $other")
        }
    }

    it should "parse free with an array element" in {
        val input = "begin free arr[0] end"
        stmtAt(input, 0) match {
            case Stmt.Free(ArrayElem(Ident("arr", _), List(IntLiter(0, _)), _), _) => succeed
            case other => fail(s"Expected Free(ArrayElem(arr[0])) but got: $other")
        }
    }

    it should "parse free with expression (syntactically valid)" in {
        val input = "begin free x + 5 end"
        stmtAt(input, 0) match {
            case Stmt.Free(BinaryExpr(_, _, _, _), _) => succeed
            case other => fail(s"Expected Free(BinaryExpr(...)) but got: $other")
        }
    }

    it should "fail when free is missing an expression" in {
        val input = "begin free end"
        parseFail(input)
    }

    it should "fail when free is followed by a statement" in {
        val input = "begin free int x = 5 end"
        parseFail(input)
    }

    /* ==================== {/FREE} ==================== */

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

    it should "allow parentheses to override standard precedence" in {
        val input = "begin int x = (5 + 3) * 2 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Mul, left, IntLiter(2, _), _) =>
                left match {
                    case BinaryExpr(BinaryOperator.Add, IntLiter(5, _), IntLiter(3, _), _) =>
                        succeed
                    case other => fail(s"Expected (5+3) on LHS, got: $other")
                }
            case other => fail(s"Expected Mul((5+3), 2), got: $other")
        }
    }

    it should "bind arithmetic operators stronger than comparison operators" in {
        val input = "begin bool b = 5 + 2 > 6 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.GT, left, IntLiter(6, _), _) =>
                left match {
                    case BinaryExpr(BinaryOperator.Add, IntLiter(5, _), IntLiter(2, _), _) =>
                        succeed
                    case other => fail(s"Expected (5+2) on LHS, got: $other")
                }
            case other => fail(s"Expected GT((5+2), 6), got: $other")
        }
    }

    it should "bind && (AND) stronger than || (OR)" in {
        val input = "begin bool b = true || false && true end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Or, BoolLiter(true, _), rhs, _) =>
                rhs match {
                    case BinaryExpr(BinaryOperator.And, BoolLiter(false, _), BoolLiter(true, _), _)
                        => succeed
                    case other => fail(s"Expected (false && true) on RHS, got: $other")
                }
            case other => fail(s"Expected Or(true, ...), got: $other")
        }
    }

    it should "parse division and modulo with correct left associativity" in {
        val input = "begin int x = 10 / 2 % 3 end"
        val rval  = expectDeclAt(input, 0)

        rval match {
            case BinaryExpr(BinaryOperator.Mod, left, IntLiter(3, _), _) =>
                left match {
                    case BinaryExpr(BinaryOperator.Div, IntLiter(10, _), IntLiter(2, _), _) =>
                        succeed
                    case other => fail(s"Expected (10 / 2) on LHS, got: $other")
                }
            case other => fail(s"Expected Mod(..., 3), got: $other")
        }
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

    /* ==================== {FUNCTIONS} ==================== */

    "The function parser" should "parse valid function definitions with params" in {
        val input = "begin int f(int x, char y) is return x + 1 end println \"hello\" end"
        parseOk(input) match {
            case Program(_, _, Func(typ, ident, params, stmt, _) :: tail, _, _) =>
                tail.length shouldBe 0

                typ shouldBe SyntaxType.Int
                ident.name shouldBe "f"

                params.length shouldBe 2
                params(0).ident.name shouldBe "x"
                params(0).typ shouldBe SyntaxType.Int
                params(1).ident.name shouldBe "y"
                params(1).typ shouldBe SyntaxType.Char

            case other => fail("Expected a single function in the list of program functions" +
                               s"but got ${other}")
        }
    }

    it should "parse valid function calls with arguments" in {
        val input = "begin int f(int x) is return x + 1 end bool g() is return true end" +
                    " int y = call f(3) end"
                    
        parseOk(input) match {
            case Program(_, _, _, Stmt.Block(List(Stmt.Decl(_, _, Call(ident, argList, _), _)), _), _) =>
                ident.name shouldBe "f"
                argList.length shouldBe 1
                argList(0) match {
                    case IntLiter(n, _) => n shouldBe 3
                    case other          => fail(s"Expected IntLiter(3) but got: $other")
                }
            
            case other => fail("Expected a single call with `3` to function f" +
                               s"but got ${other}")
        }
    }

    it should "not allow a function with a non-returning path" in {
        val input = "begin int f(int x) is if x > 2 then return x + 1 else skip fi end" +
                    "int y = call f(3); println(y) end"
        parseFail(input)
    }
    
    it should "only allow functions to be defined at the top of the main block" in {
       val input = "begin bool f() is return true end bool x = call f(); bool g() is false end end" 
       parseFail(input)
    }

    /* ==================== {/FUNCTIONS} ==================== */

    /* ==================== {SCOPE} ==================== */
    "Scope parsing" should "allow scopes to be nested" in {
        val input = "begin begin int x = 1 end end"
        parseOk(input) match {
            case Program(_, _, funcs, Stmt.Block(List(Stmt.Block(innerStmts, _)), _), _) =>
                innerStmts.length shouldBe 1
            
            case other => fail("Expected a scope containing another scope" +
                               s" but got ${other}")
        }
    }

    it should "allow scopes inside of statements" in {
        val input = "begin while true do begin skip end done end"
        parseOk(input) match {
            case Program(_, _, _,
                        Stmt.Block(List(Stmt.While(_,
                                                   Stmt.Block(List(Stmt.Block(stmts, _)), _),
                                                    _)), _), _) =>
                stmts.length shouldBe 1
                stmts(0) match {
                    case Stmt.Skip(_) => true
                    case other          => fail("Expected skip statement in inner block but got" +
                                                s"${other}")
                }
            case other => fail("Expected a scope containing another scope" +
                               s" but got ${other}")

        }    
    }
    /* ==================== {/SCOPE} ==================== */

    /* ==================== {STRINGS} ==================== */
    "String parsing" should "parse escaped characters" in {
        val input = """begin string x = "\thel\flo\n" end"""
        parseOk(input) match {
            case Program(_, _, _, Stmt.Block(List(Stmt.Decl(_, _, StrLiter(strValue, _), _)), _), _) =>
                strValue shouldBe "\thel\flo\n"
                
            case other => fail("Expected a string with escape characters" +
                               s" but got ${other}")
        }
    }

    it should "not allow un-escaped escape characters" in {
        val input = "begin skip 'end"
        parseFail(input)
    }

    /* ==================== {/STRINGS} ==================== */

    /* ==================== {ARRAYS} ==================== */

    "The array type parser" should "parse simple array types" in {
        val input = "begin int[] arr = [] end"
        val (typ, _, _) = expectDeclStmtAt(input, 0)
        typ shouldBe SyntaxType.Array(SyntaxType.Int)
    }

    it should "parse nested array types" in {
        val input = "begin int[][] arr = [] end"
        val (typ, _, _) = expectDeclStmtAt(input, 0)
        typ shouldBe SyntaxType.Array(SyntaxType.Array(SyntaxType.Int))
    }

    it should "parse array of pairs" in {
        val input = "begin pair(int, int)[] arr = [] end"
        val (typ, _, _) = expectDeclStmtAt(input, 0)
        typ shouldBe SyntaxType.Array(SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int))
    }

    it should "parse pair containing array element type" in {
        val input = "begin pair(int[], bool) p = null end"
        val (typ, _, _) = expectDeclStmtAt(input, 0)
        typ shouldBe SyntaxType.Pair(SyntaxType.Array(SyntaxType.Int), SyntaxType.Bool)
    }

    "The array literal parser" should "parse an empty array literal" in {
        val input = "begin int[] arr = [] end"
        val rval = expectDeclAt(input, 0)
        rval match {
            case ArrayLiter(elems, _) => elems shouldBe empty
            case other => fail(s"Expected ArrayLiter([]) but got: $other")
        }
    }

    it should "parse a single-element array literal" in {
        val input = "begin int[] arr = [1] end"
        val rval = expectDeclAt(input, 0)
        rval match {
            case ArrayLiter(elems, _) => elems.length shouldBe 1
            case other => fail(s"Expected ArrayLiter([1]) but got: $other")
        }
    }

    it should "parse a multi-element array literal" in {
        val input = "begin int[] arr = [1, 2, 3] end"
        val rval = expectDeclAt(input, 0)
        rval match {
            case ArrayLiter(elems, _) => elems.length shouldBe 3
            case other => fail(s"Expected ArrayLiter([1,2,3]) but got: $other")
        }
    }

    "The array element parser" should "parse array element access in expressions" in {
        val input = "begin int[] arr = [1]; int x = arr[0] end"
        val rval = expectDeclAt(input, 1)
        rval match {
            case ArrayElem(Ident("arr", _), indices, _) => indices.length shouldBe 1
            case other => fail(s"Expected ArrayElem(arr, [0]) but got: $other")
        }
    }

    it should "parse nested array element access" in {
        val input = "begin int[][] arr = []; int x = arr[0][1] end"
        val rval = expectDeclAt(input, 1)
        rval match {
            case ArrayElem(Ident("arr", _), indices, _) => indices.length shouldBe 2
            case other => fail(s"Expected ArrayElem(arr, [0][1]) but got: $other")
        }
    }

    it should "parse array element as lvalue in assignment" in {
        val input = "begin int[] arr = [1]; arr[0] = 5 end"
        val asgn = expectAsgnAt(input, 1)
        asgn.lval match {
            case ArrayElem(Ident("arr", _), indices, _) => indices.length shouldBe 1
            case other => fail(s"Expected ArrayElem lvalue but got: $other")
        }
    }

    it should "fail when array index is missing" in {
        val input = "begin int[] arr = [1]; int x = arr[] end"
        parseFail(input)
    }

    /* ==================== {/ARRAYS} ==================== */

    /* ==================== {VERIFIED ERRORS} ==================== */
    "Verified error messages for unclosed blocks" should "detect unclosed if at EOF" in {
        val input = "begin if true then skip else skip"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed if statement")
            case Right(_) => fail("Expected parse failure for unclosed if")
        }
    }

    it should "detect unclosed if with wrong closing keyword (end)" in {
        val input = "begin if true then skip else skip end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed if statement")
            case Right(_) => fail("Expected parse failure for unclosed if")
        }
    }

    it should "detect unclosed if with wrong closing keyword (done)" in {
        val input = "begin if true then skip else skip done end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed if statement")
            case Right(_) => fail("Expected parse failure for unclosed if")
        }
    }

    it should "detect unclosed while at EOF" in {
        val input = "begin while true do skip"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed while loop")
            case Right(_) => fail("Expected parse failure for unclosed while")
        }
    }

    it should "detect unclosed while with wrong closing keyword (end)" in {
        val input = "begin while true do skip end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed while loop")
            case Right(_) => fail("Expected parse failure for unclosed while")
        }
    }

    it should "detect unclosed while with wrong closing keyword (fi)" in {
        val input = "begin while true do skip fi end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed while loop")
            case Right(_) => fail("Expected parse failure for unclosed while")
        }
    }

    it should "detect unclosed begin block at EOF" in {
        val input = "begin skip"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed begin block")
            case Right(_) => fail("Expected parse failure for unclosed begin")
        }
    }

    it should "detect unclosed begin block with wrong closing keyword (fi)" in {
        val input = "begin skip fi"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed begin block")
            case Right(_) => fail("Expected parse failure for unclosed begin")
        }
    }

    it should "detect unclosed begin block with wrong closing keyword (done)" in {
        val input = "begin skip done"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed begin block")
            case Right(_) => fail("Expected parse failure for unclosed begin")
        }
    }

    it should "detect unclosed nested scope at EOF" in {
        val input = "begin begin skip end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("unclosed begin block")
            case Right(_) => fail("Expected parse failure for unclosed nested scope")
        }
    }


    it should "successfully parse correctly closed if statement" in {
        val input = "begin if true then skip else skip fi end"
        parseOk(input)
    }

    it should "successfully parse correctly closed while loop" in {
        val input = "begin while true do skip done end"
        parseOk(input)
    }

    it should "successfully parse correctly closed nested scopes" in {
        val input = "begin begin skip end end"
        parseOk(input)
    }
    /* ==================== {/VERIFIED ERRORS} ==================== */

    /* ==================== {PREVENTATIVE ERRORS} ==================== */
    "Preventative error messages" should "detect missing else branch in if statement" in {
        val input = "begin if true then skip fi end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("if statements require an 'else' branch")
            case Right(_) => fail("Expected parse failure for missing else")
        }
    }

    it should "detect missing else branch with nested if" in {
        val input = "begin if x > 0 then int y = 5 fi end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("if statements require an 'else' branch")
            case Right(_) => fail("Expected parse failure for missing else")
        }
    }

    it should "detect missing 'then' keyword in if statement" in {
        val input = "begin if true skip else skip fi end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("if statement expects 'then' after condition")
            case Right(_) => fail("Expected parse failure for missing then")
        }
    }

    it should "detect missing 'then' with complex condition" in {
        val input = "begin if x > 5 && y < 10 skip else skip fi end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("if statement expects 'then' after condition")
            case Right(_) => fail("Expected parse failure for missing then")
        }
    }

    it should "detect missing 'do' keyword in while loop" in {
        val input = "begin while true skip done end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("while loop expects 'do' after condition")
            case Right(_) => fail("Expected parse failure for missing do")
        }
    }

    it should "detect missing 'do' with complex condition" in {
        val input = "begin while x < 100 skip done end"
        val result = parser.parse(input)
        result match {
            case Left(err) => err should include("while loop expects 'do' after condition")
            case Right(_) => fail("Expected parse failure for missing do")
        }
    }

    it should "allow valid semicolons between statements" in {
        val input = "begin int x = 1; int y = 2; skip end"
        parseOk(input)
    }

    /* ==================== {/PREVENTATIVE ERRORS} ==================== */
}
