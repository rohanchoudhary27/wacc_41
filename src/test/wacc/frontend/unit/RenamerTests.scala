import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc._

/* Get the renamedID of a renamed variable */
def extractId(ident: Ident[SemanticData]): Option[Int] = ident.info match {
    case SemanticData.Var(_, id, _) => Some(id)
    case _ => None
}

class RenamerSpec extends AnyFlatSpec with Matchers {

    "The Renamer" should "assign different rename IDs to shadowed variables" in {
        val innerDecl = Stmt.Decl(SyntaxType.Int, Ident("x", SyntacticData.Pos(0,0)), IntLiter(0, SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))
        val outerDecl = Stmt.Decl(SyntaxType.Int, Ident("x", SyntacticData.Pos(0,0)), IntLiter(0, SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))

        val testBlock = Stmt.Block(
            List(
            outerDecl,
            Stmt.Block(List(innerDecl), SyntacticData.Pos(0,0))
            ), 
            SyntacticData.Pos(0,0)
        )

        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))

        val renamedAST = Renamer.rename(inputAST, new SemanticErrorLogger())

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
            fail("Error in variable shadowing test - outer variable not tagged with `Var` semantic data")
        }

        if (innerId.isEmpty) {
            fail("Error in variable shadowing test - inner variable not tagged with `Var` semantic data")
        }

        outerId should not equal innerId
    }

    it should "maintain the lvalue in a read statement after renaming" in {
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", SyntacticData.Pos(0,0)), IntLiter(5, SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))
        val readStmt = Stmt.Read(Ident("x", SyntacticData.Pos(0,0)), SyntacticData.Pos(0,0))
        
        val testBlock = Stmt.Block(List(decl, readStmt), SyntacticData.Pos(0,0))
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, SyntacticData.Pos(0,0))
        
        val errorLogger = SemanticErrorLogger()
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

        val errorLogger = new SemanticErrorLogger() 
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

        val errorLogger = new SemanticErrorLogger()
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

        val errorLogger = new SemanticErrorLogger()
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

        val errorLogger = new SemanticErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'number' is already declared")
        
        errors should include ("10") 
        errors should include ("1")
    }
}