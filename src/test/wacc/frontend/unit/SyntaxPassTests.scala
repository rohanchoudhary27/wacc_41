import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend._

class SyntaxPassTests extends AnyFlatSpec with Matchers {

    val pos: SyntacticData = SyntacticData.Pos((0, 0))

    "The Syntax Pass" should "report a syntax error when a non-returning path exists" in {

        val returnStmt = Stmt.Return(IntLiter(2, pos), pos)
        val ifStmt = Stmt.If(BoolLiter(false, pos), Stmt.Skip(pos), returnStmt, pos)
        val func = Func(SyntaxType.Int, Ident("f", pos), List(), ifStmt, pos)

        val inputAST = Program(Nil, List(func), Stmt.Skip(pos), pos)

        val errorLogger = ErrorLogger()
        SyntaxPass.syntaxCheck(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true

        errorLogger.retrieveAllErrors() should include ("'f' is not guaranteed to return")

    }
}
