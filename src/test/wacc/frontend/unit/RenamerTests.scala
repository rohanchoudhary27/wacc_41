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
}