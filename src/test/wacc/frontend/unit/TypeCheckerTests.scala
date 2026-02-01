package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc._

class TypeCheckerTests extends AnyFlatSpec with Matchers {

    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos

    "The type checker for read statements" should "accept read with int variable" in {
        val input = "begin int x = 0; read x end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }
        val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept read with char variable" in {
        val input = "begin char c = 'a'; read c end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }
        val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "reject read with bool variable" in {
        val input = "begin bool b = true; read b end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type mismatch")
    }

    it should "reject read with string variable" in {
        val input = "begin string s = \"hello\"; read s end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }

        TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type mismatch")
    }

    "Type-checking functions" should "correctly type-check a valid function call" in {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val callExpr = Call(
            Ident("f", funcSignature),
            List(IntLiter(10, SemanticData.Expr(synPos.pos, KnownType.Int))),
            semPos
        )

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("y", SemanticData.Var(synPos.pos, 1, KnownType.Int)), 
            callExpr,
            semPos
        )

        val inputAST =
            Program(List(), Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        val errorLogger = new ErrorLogger()

        TypeChecker.check(inputAST, errorLogger, funcTable)

        if (errorLogger.hasErrors()) fail(errorLogger.retrieveAllErrors())
    }

    "The type checker for pairs" should "accept null asgn to a pair and extracting fst/snd" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // pair(int, int) p = null;
        val pVar = Ident(
            "p",
            SemanticData.Var(synPos.pos, 1, KnownType.Pair(KnownType.Int, KnownType.Int))
        )

        val declP =
            Stmt.Decl(
                SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
                pVar,
                PairLiter(SemanticData.Expr(synPos.pos, Null)), // null
                semPos
            )

        // int x = fst p;
        val xVar = Ident("x", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val fstP =
            PairElem(
                isFst = true,
                Ident("p", SemanticData.Var(synPos.pos,
                                            1,
                                            KnownType.Pair(KnownType.Int, KnownType.Int)
                                            )
                ),
                semPos
            )
        val declX = Stmt.Decl(SyntaxType.Int, xVar, fstP, semPos)

        // int y = snd p;
        val yVar = Ident("y", SemanticData.Var(synPos.pos, 3, KnownType.Int))
        val sndP =
            PairElem(
                isFst = false,
                Ident("p",
                      SemanticData.Var(synPos.pos, 1, KnownType.Pair(KnownType.Int, KnownType.Int))
                ),
                semPos
            )
        val declY = Stmt.Decl(SyntaxType.Int, yVar, sndP, semPos)

        val inputAST =
            Program(Nil,
                    Stmt.Block(List(declP, declX, declY), semPos),
                    SemanticData.TopLevel(funcTable)
            )

        val typeCheckResult = TypeChecker.check(inputAST, errorLogger, funcTable)

        if (errorLogger.hasErrors()) fail(errorLogger.retrieveAllErrors())
        typeCheckResult.isDefined shouldBe true
    }

    it should "reject newpair when its arguments do not match the declared pair type" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // pair(int, char) p = newpair(true, 'a') 
        val pVar =
            Ident("p",
                  SemanticData.Var(synPos.pos, 1, KnownType.Pair(KnownType.Int, KnownType.Char))
            )

        val badNewPair =
            NewPair(
                BoolLiter(true, SemanticData.Expr(synPos.pos, KnownType.Bool)),
                CharLiter('a', SemanticData.Expr(synPos.pos, KnownType.Char)),
                semPos
            )

        val declP =
            Stmt.Decl(
                SyntaxType.Pair(SyntaxType.Int, SyntaxType.Char),
                pVar,
                badNewPair,
                semPos
            )

        val inputAST =
            Program(Nil, Stmt.Block(List(declP), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type")
    }

    it should "reject fst/snd applied to a non-pair value" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int x = 5;
        val xVar = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val declX = Stmt.Decl(
            SyntaxType.Int,
            xVar,
            IntLiter(5, SemanticData.Expr(synPos.pos, KnownType.Int)),
            semPos
        )

        // int y = fst x;  
        val yVar = Ident("y", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val fstX =
            PairElem(
                isFst = true,
                Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int)),
                semPos
            )
        val declY = Stmt.Decl(SyntaxType.Int, yVar, fstX, semPos)

        val inputAST =
            Program(Nil, Stmt.Block(List(declX, declY), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type")
    }

    it should "reject assignments where both sides are ambiguous erased pairs" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // pair(pair, int) p = null
        val pVar =
            Ident("p", SemanticData.Var(synPos.pos, 1, KnownType.Pair(PairAny, KnownType.Int)))

        val declP =
            Stmt.Decl(
                SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
                pVar,
                PairLiter(SemanticData.Expr(synPos.pos, Null)),
                semPos
            )

        // pair(pair, int) q = null
        val qVar =
            Ident("q", SemanticData.Var(synPos.pos, 2,KnownType.Pair(PairAny, KnownType.Int)))

        val declQ =
            Stmt.Decl(
                SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
                qVar,
                PairLiter(SemanticData.Expr(synPos.pos, PairAny)),
                semPos
            )

        // fst fst p
        val fstFstP =
            PairElem(
                isFst = true,
                PairElem(
                    isFst = true,
                    pVar,
                    semPos
                ),
                semPos
            )

        // fst fst q
        val fstFstQ =
            PairElem(
                isFst = true,
                PairElem(
                    isFst = true,
                    qVar,
                    semPos
                ),
                semPos
            )

        // fst fst p = fst fst q
        val asgn = Stmt.Asgn(fstFstP, fstFstQ, semPos)

        val inputAST = Program(
            Nil,
            Stmt.Block(List(declP, declQ, asgn), semPos),
            SemanticData.TopLevel(funcTable)
        )

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true

        val errors = errorLogger.retrieveAllErrors()
        errors should include ("ambiguous")
    }
}
