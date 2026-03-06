package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend.*
import wacc.frontend.lsa.*

class StaticAnalyserTests extends AnyFlatSpec with Matchers {

    private val semPos = SemanticData.Pos((1, 1))
    private val typedInt = SemanticData.Typed((1, 1), KnownType.Int)
    private val typedBool = SemanticData.Typed((1, 1), KnownType.Bool)
    private val typedPair = SemanticData.Typed((1, 1), KnownType.Pair(KnownType.Int, KnownType.Int))

    private def programWith(stmt: Stmt[SemanticData]): Program[SemanticData] =
        Program(
            List(),
            Stmt.Block(List(stmt), semPos),
            SemanticData.TopLevel(Map.empty)
        )

    private def programWithStmts(stmts: List[Stmt[SemanticData]]): Program[SemanticData] =
        Program(
            List(),
            Stmt.Block(stmts, semPos),
            SemanticData.TopLevel(Map.empty)
        )

    private def varIdent(name: String, renameId: Int, semType: SemanticType = KnownType.Int): Ident[SemanticData] =
        Ident(name, SemanticData.Var((1, 1), renameId, semType))

    "StaticAnalyser on declarations" should "track integer literal assignments" in {
        val xIdent = varIdent("x", 0)
        val decl = Stmt.Decl(
            SyntaxType.Int,
            xIdent,
            IntLiter(5, typedInt),
            semPos
        )

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    it should "track boolean literal assignments" in {
        val bIdent = varIdent("b", 0, KnownType.Bool)
        val decl = Stmt.Decl(
            SyntaxType.Bool,
            bIdent,
            BoolLiter(true, typedBool),
            semPos
        )

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    it should "track null literal assignments" in {
        val pIdent = varIdent("p", 0, KnownType.Pair(KnownType.Int, KnownType.Int))
        val decl = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            PairLiter(typedPair),
            semPos
        )

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    it should "track newpair allocations as non-null" in {
        val pIdent = varIdent("p", 0, KnownType.Pair(KnownType.Int, KnownType.Int))
        val decl = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    it should "track array literal lengths" in {
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val typedArr = SemanticData.Typed((1, 1), KnownType.Array(KnownType.Int), Some(3))
        val decl = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            aIdent,
            ArrayLiter(
                List(IntLiter(1, typedInt), IntLiter(2, typedInt), IntLiter(3, typedInt)),
                typedArr
            ),
            semPos
        )

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on read" should "complete without errors on a simple read" in {
        val xIdent = varIdent("x", 0)
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)
        val read = Stmt.Read(xIdent, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(decl, read)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on free" should "complete without errors on a simple free" in {
        val pIdent = varIdent("p", 0, KnownType.Pair(KnownType.Int, KnownType.Int))
        val decl = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val free = Stmt.Free(pIdent, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(decl, free)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on if-else" should "complete without errors on a simple if-else" in {
        val xIdent = varIdent("x", 0)
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)

        val cond = BoolLiter(true, typedBool)
        val thenBlock = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val elseBlock = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val ifStmt = Stmt.If(cond, thenBlock, elseBlock, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(decl, ifStmt)))
        result.hasErrors shouldBe false
    }


    "StaticAnalyser on while" should "complete without errors on a simple while loop" in {
        val xIdent = varIdent("x", 0)
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)

        val cond = BoolLiter(false, typedBool)
        val body = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val whileStmt = Stmt.While(cond, body, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(decl, whileStmt)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on exit" should "complete without errors" in {
        val exitStmt = Stmt.Exit(IntLiter(0, typedInt), semPos)
        val result = StaticAnalyser.analyse(programWith(exitStmt))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on skip" should "not produce any diagnostics" in {
        val result = StaticAnalyser.analyse(programWith(Stmt.Skip(semPos)))
        result.diagnostics shouldBe empty
    }

    "StaticAnalyser on print" should "complete without errors" in {
        val printStmt = Stmt.Print(IntLiter(42, typedInt), semPos)
        val result = StaticAnalyser.analyse(programWith(printStmt))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser on functions" should "analyse a simple function body" in {
        val funcSignature: SemanticData.Func = SemanticData.Func((1, 1), "f", KnownType.Int, List(KnownType.Int))
        val funcTable = Map("f" -> funcSignature)

        val xIdent = varIdent("x", 0)
        val returnStmt = Stmt.Return(xIdent, semPos)
        val func = Func(
            SyntaxType.Int,
            Ident("f", funcSignature),
            List(Param(SyntaxType.Int, xIdent, semPos)),
            returnStmt,
            semPos
        )

        val ast = Program(
            List(func),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            SemanticData.TopLevel(funcTable)
        )

        val result = StaticAnalyser.analyse(ast)
        result.hasErrors shouldBe false
    }

    "AnalysisDiagnostic" should "format error diagnostics correctly" in {
        val diag = AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            (5, 10),
            "Division by zero: divisor 'x' is constant 0"
        )
        diag.format should include("Line 5")
        diag.format should include("column 10")
        diag.format should include("error")
        diag.format should include("Division by zero")
    }

    it should "format warning diagnostics correctly" in {
        val diag = AnalysisDiagnostic(
            DiagnosticSeverity.Warning,
            (3, 4),
            "Potential null dereference"
        )
        diag.format should include("warning")
    }

    it should "format info diagnostics correctly" in {
        val diag = AnalysisDiagnostic(
            DiagnosticSeverity.Info,
            (1, 1),
            "Null check is redundant"
        )
        diag.format should include("info")
    }

    it should "include detail when provided" in {
        val diag = AnalysisDiagnostic(
            DiagnosticSeverity.Error,
            (1, 1),
            "Some error",
            "extra detail"
        )
        diag.format should include("extra detail")
    }
}
