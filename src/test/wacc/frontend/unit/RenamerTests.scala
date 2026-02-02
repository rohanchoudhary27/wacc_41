package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc._

/* Get the renamedID of a renamed variable */
def extractId(ident: Ident[SemanticData]): Option[Int] = ident.info match {
    case SemanticData.Var(_, id, _) => Some(id)
    case _ => None
}

class RenamerSpec extends AnyFlatSpec with Matchers {

    val pos = SyntacticData.Pos(0, 0)

    "The Renamer" should "assign different rename IDs to shadowed variables" in {

        val innerDecl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", SyntacticData.Pos(0,0)),
            IntLiter(0, SyntacticData.Pos(0,0)),
            SyntacticData.Pos(0,0))

        val outerDecl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", SyntacticData.Pos(0,0)), 
            IntLiter(0, SyntacticData.Pos(0,0)),
            SyntacticData.Pos(0,0))

        val testBlock = Stmt.Block(
            List(
            outerDecl,
            Stmt.Block(List(innerDecl), SyntacticData.Pos(0,0))
            ), 
            SyntacticData.Pos(0,0)
        )

        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val mainStmt = renamedAST.stmt

        val Stmt.Block(
            List(
            Stmt.Decl(_, outerIdent, _, _),
            Stmt.Block(
                List(Stmt.Decl(_, innerIdent, _, _)), _
            )
            ), 
            _
        ) = mainStmt: @unchecked

        val outerId = extractId(outerIdent)
        val innerId = extractId(innerIdent)

        if (outerId.isEmpty) {
            fail("Error in variable shadowing test - outer var not tagged with `Var` semantic data")
        }

        if (innerId.isEmpty) {
            fail("Error in variable shadowing test - inner var not tagged with `Var` semantic data")
        }

        outerId should not equal innerId
    }

    it should "maintain the lvalue in a read statement after renaming" in {

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", SyntacticData.Pos(0,0)),
            IntLiter(5, SyntacticData.Pos(0,0)),
            SyntacticData.Pos(0,0))

        val readStmt = Stmt.Read(Ident("x", SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))
        
        val testBlock = Stmt.Block(List(decl, readStmt), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))
        
        val errorLogger = ErrorLogger()
        val renamedAST = Renamer.rename(inputAST, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val Stmt.Block(
            List(
                Stmt.Decl(_, declIdent, _, _),
                Stmt.Read(readIdent: Ident[SemanticData], _)
            ), 
            _
        ) = renamedAST.stmt: @unchecked
        
        val declId = extractId(declIdent)
        val readId = extractId(readIdent)
        
        declId.isDefined shouldBe true
        readId.isDefined shouldBe true
        declId shouldEqual readId
    }

    it should "suggest a similar variable name when a typo occurs in the same scope" in {

        val decl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("myVariable", SyntacticData.Pos(0,0)), 
            IntLiter(5, SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )

        val printStmt = Stmt.Print(
            Ident("myVarble", SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )

        val testBlock = Stmt.Block(List(decl, printStmt), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val errorLogger = new ErrorLogger() 
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Variable 'myVarble' is not defined")
        errors should include ("Did you mean 'myVariable'?")
    }

    it should "suggest a similar variable name from a parent scope" in {

        val outerDecl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("jamie", SyntacticData.Pos(0,0)), 
            IntLiter(10, SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )
        
        val innerPrint = Stmt.Print(
            Ident("jame", SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )
        val innerBlock = Stmt.Block(List(innerPrint), SyntacticData.Pos(0,0))

        val testBlock = Stmt.Block(List(outerDecl, innerBlock), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'jame' is not defined")
        errors should include ("Did you mean 'jamie'?")
    }

    it should "not suggest anything if no variable is similar enough" in {
        val decl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", SyntacticData.Pos(0,0)), 
            IntLiter(5, SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )

        val printStmt = Stmt.Print(
            Ident("konstantinos", SyntacticData.Pos(0,0)), 
            SyntacticData.Pos(0,0)
        )

        val testBlock = Stmt.Block(List(decl, printStmt), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'konstantinos' is not defined")
        errors should not include ("Did you mean")
    }

    it should "output the declaration site location on a redeclaration error" in {

        val decl1 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", SyntacticData.Pos(10, 5)),
            IntLiter(5, SyntacticData.Pos(10, 10)), 
            SyntacticData.Pos(10, 1)
        )

        val decl2 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", SyntacticData.Pos(20, 5)),
            IntLiter(5, SyntacticData.Pos(20, 10)), 
            SyntacticData.Pos(20, 1)
        )

        val testBlock = Stmt.Block(List(decl1, decl2), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'number' is already declared")
        
        errors should include ("10") 
        errors should include ("1")
    }

    "The Renamer in for read" should "maintain the lvalue in a read statement after renaming" in {

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", SyntacticData.Pos(0,0)),
            IntLiter(5, SyntacticData.Pos(0,0)),
            SyntacticData.Pos(0,0))

        val readStmt = Stmt.Read(Ident("x", SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))
        
        val testBlock = Stmt.Block(List(decl, readStmt), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))
        
        val errorLogger = ErrorLogger()
        val renamedAST = Renamer.rename(inputAST, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val Stmt.Block(
            List(
                Stmt.Decl(_, declIdent, _, _),
                Stmt.Read(readIdent: Ident[SemanticData], _)
            ), 
            _
        ) = renamedAST.stmt: @unchecked
        
        val declId = extractId(declIdent)
        val readId = extractId(readIdent)
        
        declId.isDefined shouldBe true
        readId.isDefined shouldBe true
        declId shouldEqual readId
    }

    "Renaming functions" should "detect duplicate function definitions" in {

        val func1 = Func(SyntaxType.Int, Ident("f", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)
        
        val func2 = Func(SyntaxType.Int, Ident("f", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(1, pos), pos)), pos), pos)

        val inputAST = Program(List(func1, func2), Stmt.Skip(pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true

        val errors = errorLogger.retrieveAllErrors()
        errors should include ("'f' is already defined")
    }

    it should "let function parameters to be used in the function body" in {

        val paramX = Param(SyntaxType.Int, Ident("x", pos), pos)
        val returnStmt = Stmt.Return(Ident("x", pos), pos)
        
        val func = Func(SyntaxType.Int, Ident("f", pos), List(paramX), 
                       Stmt.Block(List(returnStmt), pos), pos)

        val inputAST = Program(List(func), Stmt.Skip(pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val renamedFunc = renamedAST.funcs.head
        val Stmt.Block(List(Stmt.Return(retIdent: Ident[SemanticData], _)), _) =
            renamedFunc.stmt: @unchecked
        val Param(_, paramIdent, _) = renamedFunc.params.head

        extractId(retIdent) shouldBe extractId(paramIdent)
    }

    it should "allow functions to call others defined later" in {
        
        val funcG = Func(SyntaxType.Int, Ident("g", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)

        val callG = Call(Ident("g", pos), List(), pos)
        val bodyF = Stmt.Block(List(Stmt.Decl(SyntaxType.Int, Ident("u", pos), callG, pos)), pos)
        val funcF = Func(SyntaxType.Int, Ident("f", pos), List(), bodyF, pos)

        val inputAST = Program(List(funcF, funcG), Stmt.Skip(pos), pos)

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe false
    }

    it should "allow call to pass the same renamed arguments to the function" in {
    
        val declX = Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(5, pos), pos)

        val callF = Call(Ident("f", pos), List(Ident("x", pos)), pos)
        val declResult = Stmt.Decl(SyntaxType.Int, Ident("res", pos), callF, pos)

        val funcF = Func(SyntaxType.Int, Ident("f", pos), List(Param(SyntaxType.Int, Ident("p", pos), pos)), 
                        Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)

        val inputAST = Program(List(funcF), Stmt.Block(List(declX, declResult), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredX, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, Call(_, args, _), _) = stmts(1): @unchecked
        
        val usedX = args match {
            case (id: Ident[SemanticData]) :: _ => id
            case _ => fail("Expected an Identifier as the first argument")
        }

        extractId(usedX) shouldBe extractId(declaredX)
    }

    it should "report an error when calling an undefined function" in {

        val callExpr = Call(Ident("imaginaryFunc", pos), List(), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", pos), callExpr, pos)
        
        val inputAST = Program(List(), Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Function 'imaginaryFunc' is not defined")
    }

    it should "report an error on an incorrect number of arguments in a Call" in {

        val returnStmt = Stmt.Return(StrLiter("bye", pos), pos)
        val func = Func(SyntaxType.String, Ident("f", pos), List(), returnStmt, pos)
        val call = Call(Ident("f", pos), List(StrLiter("hello", pos)), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", pos), call, pos)

        val inputAST = Program(List(func), Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include
            ("Function 'f' takes 0 arguments but 1 were given")
    }

    "Renaming pairs" should "let a declared pair identifier be used inside fst/snd" in {

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            PairElem(isFst = true, Ident("p", pos), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declP, declX), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, pIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, usedP: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedP) shouldBe extractId(pIdent)
    }

    it should "rename nested pair extractions" in {

        // pair(pair, int) p = null;
        // int q = fst fst p
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        // fst fst p  ==> PairElem(fst, PairElem(fst, Ident(p)))
        val nested =
            PairElem(isFst = true, PairElem(isFst = true, Ident("p", pos), pos),pos)

        val declQ = Stmt.Decl(SyntaxType.Int, Ident("q", pos), nested, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declP, declQ), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, pIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, innerLval, _), _) = stmts(1): @unchecked

        // innerLval is PairElem(fst, Ident(p))
        val PairElem(_, usedP: Ident[SemanticData], _) = innerLval: @unchecked
        extractId(usedP) shouldBe extractId(pIdent)
    }

    it should "report an error when fst/snd refers to an out-of-scope identifier" in {
        // int x = fst p  (p is never declared)
        val declX = Stmt.Decl(
            SyntaxType.Int, Ident("x", pos), 
            PairElem(isFst = true,
            Ident("p", pos), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declX), pos), pos)
        val errorLogger = new ErrorLogger()

        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Scope error")
    }

    it should "resolve shadowing correctly for pair identifiers inside nested blocks" in {
        // begin
        //   pair(int, int) p = null;
        //   begin
        //     pair(int, int) p = null;
        //     int x = fst p;
        //   end
        // end
        val outerDeclP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val innerDeclP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val innerDeclX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            PairElem(isFst = true, Ident("p", pos), pos),
            pos
        )

        val innerBlock = Stmt.Block(List(innerDeclP, innerDeclX), pos)
        val inputAST = Program(Nil, Stmt.Block(List(outerDeclP, innerBlock), pos), pos)

        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(outerStmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, outerP: Ident[SemanticData], _, _) = outerStmts(0): @unchecked
        val Stmt.Block(innerStmts, _) = outerStmts(1): @unchecked

        val Stmt.Decl(_, innerP: Ident[SemanticData], _, _) = innerStmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, usedP: Ident[SemanticData], _), _) =
            innerStmts(1): @unchecked

        // The use in fst p should bind to the inner p, not the outer p
        extractId(usedP) shouldBe extractId(innerP)
        extractId(innerP) should not be extractId(outerP)
    }

}