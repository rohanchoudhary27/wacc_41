package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend.*
import wacc.frontend.lsa.*

class NullCheckEliminationTests extends AnyFlatSpec with Matchers {

    private val semPos = SemanticData.Pos((1, 1))
    private val typedInt = SemanticData.Typed((1, 1), KnownType.Int)
    private val typedPair = SemanticData.Typed((1, 1), KnownType.Pair(KnownType.Int, KnownType.Int))

    private def programWithStmts(stmts: List[Stmt[SemanticData]]): Program[SemanticData] =
        Program(
            List(),
            List(),
            Stmt.Block(stmts, semPos),
            SemanticData.TopLevel(Map.empty)
        )

    private def varIdent(name: String, renameId: Int, semType: SemanticType = KnownType.Int): Ident[SemanticData] =
        Ident(name, SemanticData.Var((1, 1), renameId, semType))

    "Null check elimination" should "mark fst access as safe when pair is from newpair" in {
        /* pair(int, int) p = newpair(1, 2); int x = fst p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.hasErrors shouldBe false
        result.safePairAccesses should contain(fstAccess)
    }

    it should "mark snd access as safe when pair is from newpair" in {
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val sndAccess = PairElem(false, pIdent, SemanticData.Typed((2, 5), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, sndAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.hasErrors shouldBe false
        result.safePairAccesses should contain(sndAccess)
    }

    it should "not mark access as safe when pair is null" in {
        /* pair(int, int) p = null; int x = fst p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            PairLiter(typedPair),
            semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.hasErrors shouldBe true
        result.safePairAccesses should not contain fstAccess
    }

    it should "not mark access as safe after free" in {
        /* pair(int, int) p = newpair(1, 2); free p; int x = fst p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val freeP = Stmt.Free(pIdent, semPos)
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((4, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, freeP, declX)))
        result.hasErrors shouldBe true
        result.safePairAccesses should not contain fstAccess
    }

    it should "mark pair element lvalue assignment as safe when pointer is non-null" in {
        /* pair(int, int) p = newpair(1, 2); fst p = 5 */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val pairElemLval = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val asgn = Stmt.Asgn(pairElemLval, IntLiter(5, typedInt), semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, asgn)))
        result.hasErrors shouldBe false
        result.safePairAccesses should contain(pairElemLval)
    }

    it should "track multiple PairElem nodes independently" in {
        /* pair(int, int) p = newpair(1, 2); int x = fst p; int y = snd p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)
        val yIdent = varIdent("y", 2)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val sndAccess = PairElem(false, pIdent, SemanticData.Typed((3, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, sndAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX, declY)))
        result.hasErrors shouldBe false
        result.safePairAccesses should contain(fstAccess)
        result.safePairAccesses should contain(sndAccess)
        result.safePairAccesses should have size 2
    }

    "StaticAnalyser.run with null elimination" should "return safe PairElem nodes through run()" in {
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)
        val prog = programWithStmts(List(declP, declX))

        val errorLogger = new ErrorLogger()
        val result = StaticAnalyser.run(prog, errorLogger)

        result.safePairAccesses should contain(fstAccess)
        errorLogger.hasErrors() shouldBe false
    }
}