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

        val renamer = Renamer()
        val renamedAST = renamer.rename(inputAST, new SemanticErrorLogger())

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
}