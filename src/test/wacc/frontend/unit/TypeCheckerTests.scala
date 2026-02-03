package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc._

class TypeCheckerTests extends AnyFlatSpec with Matchers {

    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos

    "Type checking declarations" should "allow a validly typed variable declaration" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val ident = Ident("a", SemanticData.Var(synPos.pos, 1, KnownType.Char))
        val rval = CharLiter('c', SemanticData.Expr(synPos.pos, KnownType.Char))
        val decl = Stmt.Decl(SyntaxType.Char, ident, rval, semPos)

        val inputAST =
            Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "error on a type mismatch when declaring" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val ident = Ident("a", SemanticData.Var(synPos.pos, 1, KnownType.Char))
        val rval = CharLiter('c', SemanticData.Expr(synPos.pos, KnownType.Char))
        val decl = Stmt.Decl(SyntaxType.String, ident, rval, semPos)

        val inputAST =
            Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("expected string")
        errorLogger.retrieveAllErrors() should include ("unexpected char")
    }


    "Type checking assignments" should "allow a validly typed assignment" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val varInfo = SemanticData.Var(synPos.pos, 1, KnownType.Char)

        val identDecl = Ident("a", varInfo)
        val rvalDecl = CharLiter('c', SemanticData.Expr(synPos.pos, KnownType.Char))
        val decl = Stmt.Decl(SyntaxType.Char, identDecl, rvalDecl, semPos)
        val identAsgn = Ident("a", varInfo) 
        val rvalAsgn = CharLiter('d', SemanticData.Expr(synPos.pos, KnownType.Char))
        
        val asgn = Stmt.Asgn(identAsgn, rvalAsgn, semPos)

        val inputAST =
            Program(Nil, Stmt.Block(List(decl, asgn), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "allow assigning a variable to another variable of the same type" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val varInfo = SemanticData.Var(synPos.pos, 1, KnownType.Int)
        val declY = Stmt.Decl(
            SyntaxType.Int,
            Ident("y", varInfo),
            IntLiter(10, SemanticData.Expr(synPos.pos, KnownType.Int)),
            semPos
        )

        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", varInfo),
            IntLiter(0, SemanticData.Expr(synPos.pos, KnownType.Int)),
            semPos
        )

        val asgn = Stmt.Asgn(
            Ident("x", varInfo),
            Ident("y", varInfo),
            semPos
        )

        val inputAST = Program(
            Nil, 
            Stmt.Block(List(declY, declX, asgn), semPos), 
            SemanticData.TopLevel(funcTable)
        )

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "error on an assignment type mismatch" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val varInfo = SemanticData.Var(synPos.pos, 1, KnownType.Char)
        val decl = Stmt.Decl(
            SyntaxType.Char, 
            Ident("a", varInfo), 
            CharLiter('c', SemanticData.Expr(synPos.pos, KnownType.Char)), 
            semPos
        )

        val identAsgn = Ident("a", varInfo) 
        val rvalAsgn = IntLiter(2, SemanticData.Expr(synPos.pos, KnownType.Int))
        val asgn = Stmt.Asgn(identAsgn, rvalAsgn, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl, asgn), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("unexpected int")
        errors should include ("expected char")
    }

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

    it should "semantic error on a call argument type mismatch" in {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Bool))
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

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("expected bool")
        errors should include ("unexpected int")
    }

    it should "semantic error on a return type mismatch in definition" in {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List())
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val returnExpr = StrLiter("hello", SemanticData.Expr(synPos.pos, KnownType.String))
        val returnStmt = Stmt.Return(returnExpr, semPos)
        val func = Func(SyntaxType.Int, Ident("f", semPos), List(), returnStmt, semPos)

        val inputAST =
            Program(List(func),
                    Stmt.Block(List(Stmt.Skip(semPos)), semPos),
                    SemanticData.TopLevel(funcTable))
        val errorLogger = new ErrorLogger()

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("expected int")
        errors should include ("unexpected string")
    }

    "The type checker for if" should "error on if statements with a non bool condition" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val badCondition = IntLiter(10, SemanticData.Expr(synPos.pos, KnownType.Int))
        
        val thenBranch: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val elseBranch: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)

        val ifStmt = Stmt.If(badCondition, thenBranch, elseBranch, semPos)
        val inputAST = Program(Nil,
                               Stmt.Block(List(ifStmt), semPos),
                               SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors().toLowerCase
        errors should include ("expected bool")
        errors should include ("unexpected int")
    }

    "The type checker for while" should "error on while statements with a non bool condition" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val badCondition = StrLiter("loop", SemanticData.Expr(synPos.pos, KnownType.String))
        
        val body: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)

        val whileStmt = Stmt.While(badCondition, body, semPos)
        val inputAST = Program(Nil,
                               Stmt.Block(List(whileStmt), semPos),
                               SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors().toLowerCase
        errors should include ("expected bool")
        errors should include ("unexpected string")
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

    "The Unary Expression type checker" should "expect the operand for NOT to be a boolean" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val pos = SyntacticData.Pos(0,0)

        // bool b = !true;
        val expr = UnaryExpr(UnaryOperator.Not, BoolLiter(true, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Bool, Ident("b", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow NOT with a non boolean operand" in {
        // bool b = !5;
        val pos = SyntacticData.Pos(0,0)
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val expr = UnaryExpr(UnaryOperator.Not, IntLiter(5, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Bool, Ident("b", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Type mismatch")
        errorLogger.retrieveAllErrors() should include ("expected bool")
    }

    it should "expect the operand for NEG to be an int" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val pos = SyntacticData.Pos(0,0)

        // int i = -5;
        val expr = UnaryExpr(UnaryOperator.Neg, IntLiter(5, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("i", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow NEG with a non int operand" in {
        // int i = -true;
        val pos = SyntacticData.Pos(0,0)
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val expr = UnaryExpr(UnaryOperator.Neg, BoolLiter(true, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("i", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Type mismatch")
        errorLogger.retrieveAllErrors() should include ("expected int")
    }

    it should "expect the operand for ORD to be a char" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val pos = SyntacticData.Pos(0,0)

        // int c = ord 'a';
        val expr = UnaryExpr(UnaryOperator.Ord, CharLiter('a', pos), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("c", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow ORD with a non char operand" in {
        // int i = ord true;
        val pos = SyntacticData.Pos(0,0)
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val expr = UnaryExpr(UnaryOperator.Ord, BoolLiter(true, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("c", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Type mismatch")
        errorLogger.retrieveAllErrors() should include ("expected char")
    }

    it should "expect the operand for CHR to be an int" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val pos = SyntacticData.Pos(0,0)

        // char c = chr 97;
        val expr = UnaryExpr(UnaryOperator.Chr, IntLiter(97, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Char, Ident("c", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow CHR with a non int operand" in {
        // int c = chr true;
        val pos = SyntacticData.Pos(0,0)
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()
        val expr = UnaryExpr(UnaryOperator.Chr, BoolLiter(true, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Char, Ident("c", pos), expr, pos)
        
        val ast = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val renamedAST = Renamer.rename(ast, errorLogger)
        
        TypeChecker.check(renamedAST, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Type mismatch")
        errorLogger.retrieveAllErrors() should include ("expected int")
    }
}
