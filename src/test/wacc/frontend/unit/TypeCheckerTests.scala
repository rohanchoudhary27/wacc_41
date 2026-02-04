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
        val rval = CharLiter('c', SemanticData.Typed(synPos.pos, KnownType.Char))
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
        val rval = CharLiter('c', SemanticData.Typed(synPos.pos, KnownType.Char))
        val decl = Stmt.Decl(SyntaxType.String, ident, rval, semPos)

        val inputAST =
            Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Expected 'string'")
        errorLogger.retrieveAllErrors() should include ("but got 'char'")
    }


    "Type checking assignments" should "allow a validly typed assignment" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val varInfo = SemanticData.Var(synPos.pos, 1, KnownType.Char)

        val identDecl = Ident("a", varInfo)
        val rvalDecl = CharLiter('c', SemanticData.Typed(synPos.pos, KnownType.Char))
        val decl = Stmt.Decl(SyntaxType.Char, identDecl, rvalDecl, semPos)
        val identAsgn = Ident("a", varInfo) 
        val rvalAsgn = CharLiter('d', SemanticData.Typed(synPos.pos, KnownType.Char))
        
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
            IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )

        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", varInfo),
            IntLiter(0, SemanticData.Typed(synPos.pos, KnownType.Int)),
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
            CharLiter('c', SemanticData.Typed(synPos.pos, KnownType.Char)), 
            semPos
        )

        val identAsgn = Ident("a", varInfo) 
        val rvalAsgn = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int))
        val asgn = Stmt.Asgn(identAsgn, rvalAsgn, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl, asgn), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Expected 'char'")
        errors should include ("but got 'int'")
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
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

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
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

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
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
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
            case _                              => Map.empty
        }

        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
    }

    "Type-checking functions" should "correctly type-check a valid function call" in {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val callExpr = Call(
            Ident("f", funcSignature),
            List(IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))),
            semPos
        )

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("y", SemanticData.Var(synPos.pos, 1, KnownType.Int)),
            callExpr,
            semPos
        )

        val inputAST =
        Program(
            List(),
            Stmt.Block(List(decl), semPos),
            SemanticData.TopLevel(funcTable)
        )
        val errorLogger = new ErrorLogger()

        TypeChecker.check(inputAST, errorLogger, funcTable)

        if (errorLogger.hasErrors()) fail(errorLogger.retrieveAllErrors())
    }

    it should "handle a missing function signature in the map on a return outside a function" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty

        val typedLiteral = IntLiter(2, SemanticData.Typed(semPos.pos, KnownType.Int))
        val returnStmt = Stmt.Return(typedLiteral, semPos)

        val inputAST = Program(
            List(),
            Stmt.Block(List(returnStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        
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
            List(IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))),
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
        errors should include ("Expected 'bool'")
        errors should include ("but got 'int'")
    }

    it should "semantic error on a return type mismatch in definition" in {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List())
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val returnExpr = StrLiter("hello", SemanticData.Typed(synPos.pos, KnownType.String))
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
        errors should include ("Expected 'int'")
        errors should include ("but got 'string'")
    }

    "The type checker for if" should "error on if statements with a non bool condition" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val badCondition = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        
        val thenBranch: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val elseBranch: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)

        val ifStmt = Stmt.If(badCondition, thenBranch, elseBranch, semPos)
        val inputAST = Program(Nil,
                               Stmt.Block(List(ifStmt), semPos),
                               SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors().toLowerCase
        errors should include ("'bool'")
        errors should include ("but got 'int'")
    }

    "The type checker for while" should "error on while statements with a non bool condition" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val badCondition = StrLiter("loop", SemanticData.Typed(synPos.pos, KnownType.String))
        
        val body: Stmt.Block[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)

        val whileStmt = Stmt.While(badCondition, body, semPos)
        val inputAST = Program(Nil,
                               Stmt.Block(List(whileStmt), semPos),
                               SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors().toLowerCase
        errors should include ("'bool'")
        errors should include ("but got 'string'")
    }

    "The type checker for pairs" should "accept null asgn to a pair and extracting fst/snd" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // pair(int, int) p = null;
        val pVar = Ident(
        "p",
        SemanticData.Var(
            synPos.pos,
            1,
            KnownType.Pair(KnownType.Int, KnownType.Int)
        )
        )

        val declP =
        Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pVar,
            PairLiter(SemanticData.Typed(synPos.pos, Null)), // null
            semPos
        )

        // int x = fst p;
        val xVar = Ident("x", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val fstP =
        PairElem(
            isFst = true,
            Ident(
                "p",
                SemanticData
                    .Var(synPos.pos, 1, KnownType.Pair(KnownType.Int, KnownType.Int))
            ),
            semPos
        )
        val declX = Stmt.Decl(SyntaxType.Int, xVar, fstP, semPos)

        // int y = snd p;
        val yVar = Ident("y", SemanticData.Var(synPos.pos, 3, KnownType.Int))
        val sndP =
        PairElem(
            isFst = false,
            Ident(
                "p",
                SemanticData
                    .Var(synPos.pos, 1, KnownType.Pair(KnownType.Int, KnownType.Int))
            ),
            semPos
        )
        val declY = Stmt.Decl(SyntaxType.Int, yVar, sndP, semPos)

        val inputAST =
        Program(
            Nil,
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
        Ident(
            "p",
            SemanticData.Var(
                synPos.pos,
                1,
                KnownType.Pair(KnownType.Int, KnownType.Char)
            )
        )

        val badNewPair =
        NewPair(
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)),
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
        Program(
            Nil,
            Stmt.Block(List(declP), semPos),
            SemanticData.TopLevel(funcTable)
        )

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type")
    }

    it should "reject fst/snd applied to a non-pair value" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int x = 5;
        val xVar = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val declX = Stmt.Decl(
            SyntaxType.Int,
            xVar,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
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
        Program(
            Nil,
            Stmt.Block(List(declX, declY), semPos),
            SemanticData.TopLevel(funcTable)
        )

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type")
    }

    it should "reject assignments where both sides are ambiguous erased pairs" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // pair(pair, int) p = null
        val pVar =
        Ident(
            "p",
            SemanticData.Var(synPos.pos, 1, KnownType.Pair(PairAny, KnownType.Int))
        )

        val declP =
        Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            pVar,
            PairLiter(SemanticData.Typed(synPos.pos, Null)),
            semPos
        )

        // pair(pair, int) q = null
        val qVar =
        Ident(
            "q",
            SemanticData.Var(synPos.pos, 2, KnownType.Pair(PairAny, KnownType.Int))
        )

        val declQ =
        Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            qVar,
            PairLiter(SemanticData.Typed(synPos.pos, PairAny)),
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
        errors should include("ambiguous")
    }

    it should "allow `newpair` to be used on an erased inner pair assignment" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty

        val pairSemType = KnownType.Pair(PairAny, KnownType.Int)
        val pairVarInfo = SemanticData.Var(semPos.pos, 1, pairSemType)
        val pairIdent = Ident("p", pairVarInfo)

        val newPair = NewPair(
            IntLiter(10, SemanticData.Typed(semPos.pos, KnownType.Int)),
            CharLiter('b', SemanticData.Typed(semPos.pos, KnownType.Char)),
            semPos
        )

        val pairSynType = SyntaxType.Pair(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.ErasedPair),
            SyntaxType.Int
        )
        val nullRVal = PairLiter(SemanticData.Typed(semPos.pos, Null))

        val pairDecl = Stmt.Decl(pairSynType, pairIdent, nullRVal, semPos)

        val pairElem = PairElem(isFst = true, pairIdent, semPos)
        val pairElemAsgn = Stmt.Asgn(pairElem, newPair, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(pairDecl, pairElemAsgn), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow pair covariance" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val pos = semPos.pos 

        val charArrPairType = KnownType.Pair(
            KnownType.Array(KnownType.Char), 
            KnownType.Array(KnownType.Char)
        )
        
        val stringPairType = KnownType.Pair(
            KnownType.String, 
            KnownType.String
        )

        val decl = Stmt.Decl(SyntaxType.Pair(SyntaxType.String, SyntaxType.String),
                             Ident("pair2", SemanticData.Var(pos, 2, stringPairType)),
                             Ident("pair1", SemanticData.Var(pos, 1, charArrPairType)), semPos)

        val errorLogger = new ErrorLogger(Vector.empty)
        val inputAST = Program(
            Nil,
            Stmt.Block(List(decl), semPos),
            SemanticData.TopLevel(funcTable)
        )
        
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Expected 'pair(string, string)'") 
        errors should include ("got 'pair(char[], char[])'")
    }

    "The type checker for arrays" should "accept simple array literal declaration" in {
        val input = "begin int[] arr = [1, 2, 3]; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept string weakening for char[] literals" in {
        val input = "begin string s = ['h', 'e', 'l', 'l', 'o']; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept string weakening for char[] variables" in {
        val input = "begin char[] arr = ['a', 'b']; string s = arr; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept mixed char[] and string in string[] array literal" in {
        val input =
        "begin char[] arr = ['a', 'b']; string[] mixed = [arr, \"hello\"]; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept array element access" in {
        val input = "begin int[] arr = [1, 2, 3]; int x = arr[0]; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "accept array element assignment" in {
        val input = "begin int[] arr = [1, 2, 3]; arr[0] = 10; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        val typeCheckResult =
            TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
        typeCheckResult.isDefined shouldBe true
    }

    it should "reject array literal with incompatible element types" in {
        val input = "begin int[] arr = [1, true, 3]; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("array literal mismatch")
    }

    it should "reject string assigned to char[] (no reverse weakening)" in {
        val input = "begin char[] arr = \"hello\"; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
    }

    it should "reject char[][] assigned to string[] (array invariance)" in {
        val input =
        "begin char[] a = ['a']; char[] b = ['b']; char[][] arr = [a, b]; string[] s = arr; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
    }

    it should "reject non-int array index" in {
        val input = "begin int[] arr = [1, 2, 3]; int x = arr[true]; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
    }

    it should "reject wrong element type in array element assignment" in {
        val input = "begin int[] arr = [1, 2, 3]; arr[0] = true; skip end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true

        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _                              => Map.empty
        }
        TypeChecker.check(renamedProgram, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type error")
    }

    "The Unary Expression type checker" should "expect the operand for NOT to be a boolean" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // bool b = !true
        val bVar = (Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool)))
        val notExpr = UnaryExpr(UnaryOperator.Not, BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)), semPos)
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, notExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow LEN to be used with string type" in {
        // begin 
        //    println len "hello" 
        // end
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        
        val strLiter: Expr[SemanticData] = 
            StrLiter("hello", SemanticData.Typed(semPos.pos, KnownType.String))
        val unaryLenExprWithString: Expr[SemanticData] =
            UnaryExpr(UnaryOperator.Len, strLiter, semPos)
        val printStmt = Stmt.Println(unaryLenExprWithString, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(printStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include("Type")
    }


    it should "not allow NOT with a non boolean operand" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // bool b = !5
        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))
        val notExpr = UnaryExpr(
            UnaryOperator.Not, 
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)), 
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, notExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
    }

    it should "expect the operand for NEG to be an int" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int i = -5;
        val iVar = (Ident("i", SemanticData.Var(synPos.pos, 1, KnownType.Int)))
        val notExpr = UnaryExpr(UnaryOperator.Neg, IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)
        val decl = Stmt.Decl(SyntaxType.Int, iVar, notExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow NEG with a non int operand" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int i = -true
        val iVar = Ident("i", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val negExpr = UnaryExpr(
            UnaryOperator.Neg, 
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)), 
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, iVar, negExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
    }

    it should "expect the operand for ORD to be a char" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int i = -5;
        val iVar = (Ident("i", SemanticData.Var(synPos.pos, 1, KnownType.Int)))
        val ordExpr = UnaryExpr(UnaryOperator.Neg, IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)
        val decl = Stmt.Decl(SyntaxType.Int, iVar, ordExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow ORD with a non char operand" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // int i = ord true
        val iVar = Ident("i", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val ordExpr = UnaryExpr(
            UnaryOperator.Ord, 
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)), 
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, iVar, ordExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
    }

    it should "expect the operand for CHR to be an int" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // char c = chr 100;
        val cVar = (Ident("c", SemanticData.Var(synPos.pos, 1, KnownType.Char)))
        val chrExpr = UnaryExpr(UnaryOperator.Chr, IntLiter(100, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)
        val decl = Stmt.Decl(SyntaxType.Char, cVar, chrExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow CHR with a non int operand" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        // char c = chr true;
        val cVar = Ident("i", SemanticData.Var(synPos.pos, 1, KnownType.Char))
        val chrExpr = UnaryExpr(
            UnaryOperator.Chr, 
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)), 
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, cVar, chrExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
    }

    "The Binary Expression type checker" should "allow arithmetic operators with int operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val xVar = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))

        val addExpr = BinaryExpr(
            BinaryOperator.Add,
            IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int)),
            IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )

        // int x = 10 + 20
        val decl = Stmt.Decl(SyntaxType.Int, xVar, addExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow arithmetic operators with non int operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val xVar = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))

        val addExpr = BinaryExpr(
            BinaryOperator.Add,
            IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int)),
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            semPos
        )

        // int x = 10 + true
        val decl = Stmt.Decl(SyntaxType.Int, xVar, addExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("Expected 'int'")
        errors should include ("but got 'bool'")
    }

    it should "allow comparison operators with int operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val gtExpr = BinaryExpr(
            BinaryOperator.GT,
            IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int)),
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )

        // bool b = 10 > 5
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, gtExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "allow comparison operators with char operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val ltExpr = BinaryExpr(
            BinaryOperator.LT,
            CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)),
            CharLiter('z', SemanticData.Typed(synPos.pos, KnownType.Char)),
            semPos
        )

        // bool b = 'a' < 'z'
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, ltExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow comparison between incompatible types" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val gtExpr = BinaryExpr(
            BinaryOperator.GT,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)),
            semPos
        )

        // bool b = 5 > 'a'
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, gtExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("but got 'char'")
    }

    it should "allow equality checks between matching types" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val eqExpr = BinaryExpr(
            BinaryOperator.EQ,
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            semPos
        )

        // bool b = true == false
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, eqExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow equality checks between mismatched types" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val eqExpr = BinaryExpr(
            BinaryOperator.EQ,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            semPos
        )

        // bool b = 5 == true
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, eqExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("Expected 'int'")
        errors should include ("but got 'bool'")
    }

    it should "accept logical operators with bool operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val andExpr = BinaryExpr(
            BinaryOperator.And,
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            semPos
        )

        // bool b = true && false
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, andExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "not allow logical operators with non bool operands" in {
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        val errorLogger = new ErrorLogger()

        val bVar = Ident("b", SemanticData.Var(synPos.pos, 1, KnownType.Bool))

        val andExpr = BinaryExpr(
            BinaryOperator.And,
            BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool)),
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )

        // bool b = true && 5
        val decl = Stmt.Decl(SyntaxType.Bool, bVar, andExpr, semPos)

        val inputAST = Program(Nil, Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))

        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("Expected 'bool'")
        errors should include ("but got 'int'")
    }

    

    "The type checker for free statement" should "accept free statements with pair type" in {
        // begin 
        //    pair(int, int) p = null;
        //    free p
        // end
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        
        val p = 
            Ident(
                "p", 
                SemanticData.Var(semPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int))
            )
        // pair(int, int) p = null;
        val declStmt = Stmt.Decl(
                SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), 
                p,
                PairLiter(SemanticData.Typed(semPos.pos, Null)),
                semPos
            )
        // free p
        val freeStmt = Stmt.Free(p, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(declStmt, freeStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    "The type checker for free statement" should "accept free statements with array type" in {
        // begin 
        //    array[int] p = [];
        //    free p
        // end
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        
        val p = 
            Ident(
                "p", 
                SemanticData.Var(semPos.pos, 0, KnownType.Array(KnownType.Int))
            )
        // pair(int, int) p = null;
        val declStmt = Stmt.Decl(
                SyntaxType.Array(SyntaxType.Int), 
                p,
                ArrayLiter(List(), SemanticData.Typed(semPos.pos, EmptyArray)),
                semPos
            )
        // free p
        val freeStmt = Stmt.Free(p, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(declStmt, freeStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe false
    }

    it should "reject free statements with literal type" in {
        // begin 
        //    int p = 10;
        //    free p
        // end
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        
        val p = 
            Ident(
                "p", 
                SemanticData.Var(semPos.pos, 0, KnownType.Int)
            )
        // pair(int, int) p = null;
        val declStmt = Stmt.Decl(
                SyntaxType.Int, 
                p,
                IntLiter(10, SemanticData.Typed(semPos.pos, KnownType.Int)),
                semPos
            )
        // free p
        val freeStmt = Stmt.Free(p, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(declStmt, freeStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("| pair")
        errors should include ("but got 'int'")
    }

    it should "reject free statements with binary expressions" in {
        // begin 
        //    free 3 * 3
        // end
        val funcTable: Map[String, SemanticData.Func] = Map.empty
        
        val binExpr = BinaryExpr(
                BinaryOperator.Mul, 
                IntLiter(3, SemanticData.Typed(semPos.pos, KnownType.Int)), 
                IntLiter(3, SemanticData.Typed(semPos.pos, KnownType.Int)), 
                semPos
            )
        
        // free p
        val freeStmt = Stmt.Free(binExpr, semPos)

        val errorLogger = new ErrorLogger()
        val inputAST = Program(
            Nil,
            Stmt.Block(List(freeStmt), semPos),
            SemanticData.TopLevel(funcTable)
        )
        TypeChecker.check(inputAST, errorLogger, funcTable)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type error")
        errors should include ("| pair")
        errors should include ("but got 'int'")
    }
}
