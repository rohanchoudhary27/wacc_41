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
    
    private def declArray(ident: Ident[SemanticData], elements: List[Expr[SemanticData]]): Stmt.Decl[SemanticData] = {
        val arrType = KnownType.Array(KnownType.Int)
        val typedArr = SemanticData.Typed((1, 1), arrType, Some(elements.length))
        Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            ident,
            ArrayLiter(elements, typedArr),
            semPos
        )
    }

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

    "StaticAnalyser division-by-zero" should "detect division by literal zero" in {
        /* int x = 10 / 0 */
        val xIdent = varIdent("x", 0)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            IntLiter(0, typedInt),
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Division by zero")) shouldBe true
    }

    it should "detect division by a variable known to be zero" in {
        /* int y = 0; int x = 10 / y */
        val yIdent = varIdent("y", 0)
        val xIdent = varIdent("x", 1)

        val declY = Stmt.Decl(SyntaxType.Int, yIdent, IntLiter(0, typedInt), semPos)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            yIdent,
            semPos
        )
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declY, declX)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Division by zero")) shouldBe true
    }

    it should "detect modulo by zero" in {
        /* int x = 10 % 0 */
        val xIdent = varIdent("x", 0)
        val modExpr = BinaryExpr(
            BinaryOperator.Mod,
            IntLiter(10, typedInt),
            IntLiter(0, typedInt),
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, modExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe true
    }

    it should "not flag division by a known non-zero constant" in {
        /* int x = 10 / 5 */
        val xIdent = varIdent("x", 0)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            IntLiter(5, typedInt),
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
    }

    it should "not flag division by an unknown variable" in {
        /* int y = read; int x = 10 / y */
        val yIdent = varIdent("y", 0)
        val xIdent = varIdent("x", 1)

        val declY = Stmt.Decl(SyntaxType.Int, yIdent, IntLiter(1, typedInt), semPos)
        val readY = Stmt.Read(yIdent, semPos)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            yIdent,
            semPos
        )
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declY, readY, declX)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser" should "propagate through chained arithmetic" in {
        /* int a = 5; int b = 3; int c = a + b; int d = c / 0 */
        val aIdent = varIdent("a", 0)
        val bIdent = varIdent("b", 1)
        val cIdent = varIdent("c", 2)
        val dIdent = varIdent("d", 3)

        val declA = Stmt.Decl(SyntaxType.Int, aIdent, IntLiter(5, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Int, bIdent, IntLiter(3, typedInt), semPos)
        val addExpr = BinaryExpr(BinaryOperator.Add, aIdent, bIdent, typedInt)
        val declC = Stmt.Decl(SyntaxType.Int, cIdent, addExpr, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, cIdent, IntLiter(0, typedInt), semPos)
        val declD = Stmt.Decl(SyntaxType.Int, dIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declA, declB, declC, declD))
        )
        result.hasErrors shouldBe true
    }

    it should "lose knowledge after read invalidation" in {
        /* int x = 0; read x; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)
        val readX = Stmt.Read(xIdent, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, readX, declY))
        )
        result.hasErrors shouldBe false
    }

    "StaticAnalyser unary expressions" should "propagate negation" in {
        /* int x = 5; int y = -x; int z = 10 / y */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val zIdent = varIdent("z", 2)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)
        val negExpr = UnaryExpr(UnaryOperator.Neg, xIdent, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, negExpr, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, semPos)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declY, declZ))
        )
        result.hasErrors shouldBe false
    }

    it should "detect division by negated zero" in {
        /* int x = 0; int y = -x; int z = 10 / y — y is -0 = 0, so error */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val zIdent = varIdent("z", 2)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)
        val negExpr = UnaryExpr(UnaryOperator.Neg, xIdent, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, negExpr, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, semPos)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declY, declZ))
        )
        result.hasErrors shouldBe true
    }

    it should "propagate logical NOT for dead branch detection" in {
        /* bool b = true; if !b then exit 0 else skip fi */
        val bIdent = varIdent("b", 0, KnownType.Bool)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)

        val notExpr = UnaryExpr(UnaryOperator.Not, bIdent, typedBool)
        val ifStmt = Stmt.If(
            notExpr,
            Stmt.Block(List(Stmt.Exit(IntLiter(0, typedInt), semPos)), semPos),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            semPos
        )

        val result = StaticAnalyser.analyse(programWithStmts(List(declB, ifStmt)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser len" should "propagate array length through len operator" in {
        /* int[] a = [1,2,3]; int n = len a; int x = 10 / n */
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val nIdent = varIdent("n", 1)
        val xIdent = varIdent("x", 2)

        val typedArr = SemanticData.Typed((1, 1), KnownType.Array(KnownType.Int), Some(3))
        val declA = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int), aIdent,
            ArrayLiter(
                List(IntLiter(1, typedInt), IntLiter(2, typedInt), IntLiter(3, typedInt)),
                typedArr
            ), semPos
        )
        val lenExpr = UnaryExpr(UnaryOperator.Len, aIdent, typedInt)
        val declN = Stmt.Decl(SyntaxType.Int, nIdent, lenExpr, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), nIdent, semPos)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declA, declN, declX))
        )
        result.hasErrors shouldBe false
    }

    "StaticAnalyser comparisons" should "evaluate known comparisons for dead branch detection" in {
        /* int x = 5; if x > 3 then skip else exit 1 fi */
        val xIdent = varIdent("x", 0)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)

        val cmpExpr = BinaryExpr(BinaryOperator.GT, xIdent, IntLiter(3, typedInt), typedBool)
        val ifStmt = Stmt.If(
            cmpExpr,
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            Stmt.Block(List(Stmt.Exit(IntLiter(1, typedInt), semPos)), semPos),
            semPos
        )

        val result = StaticAnalyser.analyse(programWithStmts(List(declX, ifStmt)))
        result.hasErrors shouldBe false
    }

    "StaticAnalyser multiplication" should "propagate zero absorbing property" in {
        /* int x = read; int y = x * 0; int z = 10 / y */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val zIdent = varIdent("z", 2)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(1, typedInt), semPos)
        val readX = Stmt.Read(xIdent, semPos)
        val mulExpr = BinaryExpr(BinaryOperator.Mul, xIdent, IntLiter(0, typedInt), typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, mulExpr, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, semPos)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, readX, declY, declZ))
        )
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Division by zero")) shouldBe true
    }

    "StaticAnalyser null dereference" should "detect access on a null pair" in {
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
        result.diagnostics.exists(_.message.contains("Null dereference")) shouldBe true
    }

    it should "not flag access on a newpair-allocated pair" in {
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
    }

    "StaticAnalyser use-after-free" should "detect access after free" in {
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
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((3, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, freeP, declX)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Use after free")) shouldBe true
    }

    it should "detect double free" in {
        /* pair(int, int) p = newpair(1, 2); free p; free p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair),
            semPos
        )
        val free1 = Stmt.Free(pIdent, semPos)
        val free2 = Stmt.Free(pIdent, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, free1, free2)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Use after free")) shouldBe true
    }

    it should "detect free of null" in {
        /* pair(int, int) p = null; free p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            PairLiter(typedPair),
            semPos
        )
        val freeP = Stmt.Free(pIdent, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, freeP)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Null dereference")) shouldBe true
    }

    "StaticAnalyser pair element lvalue" should "detect null dereference on fst assignment" in {
        /* pair(int, int) p = null; fst p = 5 */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            pIdent,
            PairLiter(typedPair),
            semPos
        )
        val pairElemLval = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val asgn = Stmt.Asgn(pairElemLval, IntLiter(5, typedInt), semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, asgn)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Null dereference")) shouldBe true
    }

    "StaticAnalyser.run" should "report errors to ErrorLogger" in {
        /* int x = 10 / 0 — should route error to ErrorLogger */
        val xIdent = varIdent("x", 0)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            IntLiter(0, typedInt),
            semPos
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)
        val prog = programWith(decl)

        val errorLogger = new ErrorLogger()
        val result = StaticAnalyser.run(prog, errorLogger)

        result.hasErrors shouldBe true
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include("Division by zero")
    }

    it should "not report errors to ErrorLogger for clean programs" in {
        val prog = programWith(Stmt.Skip(semPos))

        val errorLogger = new ErrorLogger()
        val result = StaticAnalyser.run(prog, errorLogger)

        result.hasErrors shouldBe false
        errorLogger.hasErrors() shouldBe false
    }

    it should "report null dereference errors to ErrorLogger" in {
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
        val prog = programWithStmts(List(declP, declX))

        val errorLogger = new ErrorLogger()
        val result = StaticAnalyser.run(prog, errorLogger)

        result.hasErrors shouldBe true
        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include("Null dereference")
    }

    "Static analyser for dead branch detection" should "emit warning when condition is always true" in {
        /* if true then skip else skip fi */
        val ifStmt = Stmt.If(
            BoolLiter(true, typedBool),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            Stmt.Block(List(Stmt.Skip(SemanticData.Pos((3, 1)))), SemanticData.Pos((3, 1))),
            semPos
        )
        val result = StaticAnalyser.analyse(programWithStmts(List(ifStmt)))
        result.hasErrors shouldBe false
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Dead branch")
        } shouldBe true
    }

    it should "emit warning when condition is always false" in {
        /* if false then skip else skip fi */
        val ifStmt = Stmt.If(
            BoolLiter(false, typedBool),
            Stmt.Block(List(Stmt.Skip(SemanticData.Pos((2, 1)))), SemanticData.Pos((2, 1))),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            semPos
        )
        val result = StaticAnalyser.analyse(programWithStmts(List(ifStmt)))
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Dead branch")
        } shouldBe true
    }

    it should "emit warning when variable based condition is provably constant" in {
        /* bool b = true; if b then skip else skip fi */
        val bIdent = varIdent("b", 0, KnownType.Bool)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val ifStmt = Stmt.If(
            bIdent,
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            Stmt.Block(List(Stmt.Skip(SemanticData.Pos((3, 1)))), SemanticData.Pos((3, 1))),
            semPos
        )
        val result = StaticAnalyser.analyse(programWithStmts(List(declB, ifStmt)))
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Dead branch")
        } shouldBe true
    }

    it should "not emit warning when condition is unknown" in {
        /* bool b = true; read b; if b then skip else skip fi */
        val bIdent = varIdent("b", 0, KnownType.Bool)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)
        val ifStmt = Stmt.If(
            bIdent,
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            semPos
        )
        val result = StaticAnalyser.analyse(programWithStmts(List(declB, readB, ifStmt)))
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Dead branch")
        } shouldBe false
    }

    "Static analyser for join across if-else" should "widen variable assigned differently in each branch" in {
        /* int x = 0; if cond then x = 3 else x = 7 fi; int y = 10 / x
           After join, x is unknown, so no division-by-zero error */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val thenBlock = Stmt.Block(List(
            Stmt.Asgn(xIdent, IntLiter(3, typedInt), semPos)
        ), semPos)
        val elseBlock = Stmt.Block(List(
            Stmt.Asgn(xIdent, IntLiter(7, typedInt), semPos)
        ), semPos)
        val ifStmt = Stmt.If(bIdent, thenBlock, elseBlock, semPos)

        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, ifStmt, declY))
        )
        /* x is widened to IntRange(3,7) which is nonzero, so no error */
        result.hasErrors shouldBe false
    }

    it should "preserve variable assigned the same value in both branches" in {
        /* int x = 0; if cond then x = 5 else x = 5 fi; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val thenBlock = Stmt.Block(List(
            Stmt.Asgn(xIdent, IntLiter(5, typedInt), semPos)
        ), semPos)
        val elseBlock = Stmt.Block(List(
            Stmt.Asgn(xIdent, IntLiter(5, typedInt), semPos)
        ), semPos)
        val ifStmt = Stmt.If(bIdent, thenBlock, elseBlock, semPos)

        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, ifStmt, declY))
        )
        result.hasErrors shouldBe false
    }

    it should "retain variable not modified in either branch" in {
        /* int x = 5; if cond then skip else skip fi; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val ifStmt = Stmt.If(
            bIdent,
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            semPos
        )
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, ifStmt, declY))
        )
        result.hasErrors shouldBe false
    }

    it should "use only the reachable branch state when one branch exits" in {
        /* int x = 0; if cond then x = 5 else exit 1 fi; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(0, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val thenBlock = Stmt.Block(List(
            Stmt.Asgn(xIdent, IntLiter(5, typedInt), semPos)
        ), semPos)
        val elseBlock = Stmt.Block(List(
            Stmt.Exit(IntLiter(1, typedInt), semPos)
        ), semPos)
        val ifStmt = Stmt.If(bIdent, thenBlock, elseBlock, semPos)

        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, semPos)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, ifStmt, declY))
        )
        result.hasErrors shouldBe false
    }

    "Static Analyser for condition refinement for equality" should "narrow variable to zero in then branch of x == 0" in {
        /* int x = read; if x == 0 then int y = 10 / x else skip fi */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(1, typedInt), semPos)
        val readX = Stmt.Read(xIdent, semPos)

        val cond = BinaryExpr(BinaryOperator.EQ, xIdent, IntLiter(0, typedInt), typedBool)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, typedInt)
        val thenBlock = Stmt.Block(List(
            Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)
        ), semPos)
        val elseBlock = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val ifStmt = Stmt.If(cond, thenBlock, elseBlock, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declX, readX, ifStmt)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Division by zero")) shouldBe true
    }

    it should "not flag division in else branch of x == 0" in {
        /* int x = read; if x == 0 then skip else int y = 10 / x fi */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(1, typedInt), semPos)
        val readX = Stmt.Read(xIdent, semPos)

        val cond = BinaryExpr(BinaryOperator.EQ, xIdent, IntLiter(0, typedInt), typedBool)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, typedInt)
        val thenBlock = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val elseBlock = Stmt.Block(List(
            Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)
        ), semPos)
        val ifStmt = Stmt.If(cond, thenBlock, elseBlock, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declX, readX, ifStmt)))
        result.hasErrors shouldBe false
    }

    "Static Analyser for condition refinement for null" should "narrow pair to Null in then-branch of p == null" in {
        /* pair(int,int) p = read; if p == null then fst p else skip fi */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair), semPos
        )
        /* Invalidate to simulate unknown nullity */
        val readP = Stmt.Read(pIdent, semPos)

        val cond = BinaryExpr(BinaryOperator.EQ, pIdent, PairLiter(typedPair), typedBool)
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((5, 1), KnownType.Int))
        val thenBlock = Stmt.Block(List(
            Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)
        ), semPos)
        val elseBlock = Stmt.Block(List(Stmt.Skip(semPos)), semPos)
        val ifStmt = Stmt.If(cond, thenBlock, elseBlock, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, readP, ifStmt)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("Null dereference")) shouldBe true
    }

    "Static analyser for while loop condition refinement" should "preserve knowledge when loop body doesn't modify variable" in {
        /* int x = 5; while cond do skip od; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val whileStmt = Stmt.While(
            bIdent,
            Stmt.Block(List(Stmt.Skip(semPos)), semPos),
            semPos
        )
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, whileStmt, declY))
        )
        result.hasErrors shouldBe false
    }

    it should "invalidate variable modified in loop body" in {
        /* int x = 5; while cond do read x od; int y = 10 / x */
        val xIdent = varIdent("x", 0)
        val yIdent = varIdent("y", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val whileStmt = Stmt.While(
            bIdent,
            Stmt.Block(List(Stmt.Read(xIdent, semPos)), semPos),
            semPos
        )
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), xIdent, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declX, declB, readB, whileStmt, declY))
        )
        result.hasErrors shouldBe false
    }

    "Static analyser for unreachable code detection" should "warn about code after exit" in {
        /* exit 0; int x = 5 */
        val exitStmt = Stmt.Exit(IntLiter(0, typedInt), semPos)
        val xIdent = varIdent("x", 0)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, IntLiter(5, typedInt), SemanticData.Pos((2, 1)))

        val result = StaticAnalyser.analyse(programWithStmts(List(exitStmt, declX)))
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Unreachable")
        } shouldBe true
    }

    it should "not warn when there is no unreachable code" in {
        val result = StaticAnalyser.analyse(
            programWithStmts(List(
                Stmt.Decl(SyntaxType.Int, varIdent("x", 0), IntLiter(5, typedInt), semPos),
                Stmt.Skip(semPos)
            ))
        )
        result.diagnostics.exists { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Unreachable")
        } shouldBe false
    }

    it should "only warn once per unreachable section" in {
        /* exit 0; int x = 5; int y = 10 — only one warning */
        val exitStmt = Stmt.Exit(IntLiter(0, typedInt), semPos)
        val declX = Stmt.Decl(SyntaxType.Int, varIdent("x", 0), IntLiter(5, typedInt), SemanticData.Pos((2, 1)))
        val declY = Stmt.Decl(SyntaxType.Int, varIdent("y", 1), IntLiter(10, typedInt), SemanticData.Pos((3, 1)))

        val result = StaticAnalyser.analyse(programWithStmts(List(exitStmt, declX, declY)))
        val unreachableWarnings = result.diagnostics.filter { d =>
            d.severity == DiagnosticSeverity.Info && d.message.contains("Unreachable")
        }
        unreachableWarnings should have size 1
    }

    "Static analysis for division check elimination" should "mark division as safe when divisor is a known non-zero constant" in {
        /* int x = 10 / 5 — divisor is 5, safe */
        val xIdent = varIdent("x", 0)
        val divExpr = BinaryExpr(
            BinaryOperator.Div,
            IntLiter(10, typedInt),
            IntLiter(5, typedInt),
            typedInt
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
        result.safeDivisions should contain(divExpr)
    }

    it should "mark modulo as safe when divisor is a known non-zero constant" in {
        /* int x = 10 % 3 — divisor is 3, safe */
        val xIdent = varIdent("x", 0)
        val modExpr = BinaryExpr(
            BinaryOperator.Mod,
            IntLiter(10, typedInt),
            IntLiter(3, typedInt),
            typedInt
        )
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, modExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe false
        result.safeDivisions should contain(modExpr)
    }

    it should "mark division as safe when divisor is a variable known to be non-zero" in {
        /* int y = 4; int x = 10 / y — y is 4, safe */
        val yIdent = varIdent("y", 0)
        val xIdent = varIdent("x", 1)

        val declY = Stmt.Decl(SyntaxType.Int, yIdent, IntLiter(4, typedInt), semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, typedInt)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declY, declX)))
        result.hasErrors shouldBe false
        result.safeDivisions should contain(divExpr)
    }

    it should "not mark division as safe when divisor is unknown" in {
        /* int y = read; int x = 10 / y — y is unknown */
        val yIdent = varIdent("y", 0)
        val xIdent = varIdent("x", 1)

        val declY = Stmt.Decl(SyntaxType.Int, yIdent, IntLiter(1, typedInt), semPos)
        val readY = Stmt.Read(yIdent, semPos)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, typedInt)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declY, readY, declX)))
        result.hasErrors shouldBe false
        result.safeDivisions should not contain divExpr
    }

    it should "not mark division as safe when divisor is known zero (error case)" in {
        /* int x = 10 / 0 — error, definitely not safe */
        val xIdent = varIdent("x", 0)
        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), IntLiter(0, typedInt), typedInt)
        val decl = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(programWith(decl))
        result.hasErrors shouldBe true
        result.safeDivisions should not contain divExpr
    }

    it should "mark division safe after if-else where both branches set non-zero" in {
        /* int y = 0; if cond then y = 3 else y = 7 fi; int x = 10 / y
           After join, y is IntRange(3,7) which is provably non-zero */
        val yIdent = varIdent("y", 0)
        val xIdent = varIdent("x", 1)
        val bIdent = varIdent("b", 2, KnownType.Bool)

        val declY = Stmt.Decl(SyntaxType.Int, yIdent, IntLiter(0, typedInt), semPos)
        val declB = Stmt.Decl(SyntaxType.Bool, bIdent, BoolLiter(true, typedBool), semPos)
        val readB = Stmt.Read(bIdent, semPos)

        val thenBlock = Stmt.Block(List(Stmt.Asgn(yIdent, IntLiter(3, typedInt), semPos)), semPos)
        val elseBlock = Stmt.Block(List(Stmt.Asgn(yIdent, IntLiter(7, typedInt), semPos)), semPos)
        val ifStmt = Stmt.If(bIdent, thenBlock, elseBlock, semPos)

        val divExpr = BinaryExpr(BinaryOperator.Div, IntLiter(10, typedInt), yIdent, typedInt)
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, divExpr, semPos)

        val result = StaticAnalyser.analyse(
            programWithStmts(List(declY, declB, readB, ifStmt, declX))
        )
        result.hasErrors shouldBe false
        result.safeDivisions should contain(divExpr)
    }

    "Static analysis for array bounds on empty array" should "detect any constant index as out of bounds" in {
        /* int[] a = []; int x = a[0]*/
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val xIdent = varIdent("x", 1)

        val declA = declArray(aIdent, List.empty)

        val arrayAccess = ArrayElem(aIdent, List(IntLiter(0, typedInt)), SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, arrayAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declA, declX)))
        result.hasErrors shouldBe true
        result.diagnostics.exists(_.message.contains("out of bounds")) shouldBe true
    }

    "Static analysis for array bounds with variable index" should "detect out of bounds when variable is known constant" in {
        /* int[] a = [1, 2, 3]; int i = 5; int x = a[i] */
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val iIdent = varIdent("i", 1)
        val xIdent = varIdent("x", 2)

        val declA = declArray(aIdent, List(IntLiter(1, typedInt), IntLiter(2, typedInt), IntLiter(3, typedInt)))
        val declI = Stmt.Decl(SyntaxType.Int, iIdent, IntLiter(5, typedInt), semPos)

        val arrayAccess = ArrayElem(aIdent, List(iIdent), SemanticData.Typed((3, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, arrayAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declA, declI, declX)))
        result.hasErrors shouldBe true
    }

    it should "mark safe when variable is known constant in bounds" in {
        /* int[] a = [1, 2, 3]; int i = 2; int x = a[i] */
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val iIdent = varIdent("i", 1)
        val xIdent = varIdent("x", 2)

        val declA = declArray(aIdent, List(IntLiter(1, typedInt), IntLiter(2, typedInt), IntLiter(3, typedInt)))
        val declI = Stmt.Decl(SyntaxType.Int, iIdent, IntLiter(2, typedInt), semPos)

        val arrayAccess = ArrayElem(aIdent, List(iIdent), SemanticData.Typed((3, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, arrayAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declA, declI, declX)))
        result.hasErrors shouldBe false
        result.safeArrayAccesses should contain(arrayAccess)
    }

    "Static analysis for array bounds with arithmetic index" should "mark safe for a + b where result is in bounds" in {
        /* int[] a = [1,2,3,4,5]; int x = a[1 + 2] */
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val xIdent = varIdent("x", 1)

        val elems = (1 to 5).map(n => IntLiter(n, typedInt)).toList
        val declA = declArray(aIdent, elems)

        val addExpr = BinaryExpr(BinaryOperator.Add, IntLiter(1, typedInt), IntLiter(2, typedInt), typedInt)
        val arrayAccess = ArrayElem(aIdent, List(addExpr), SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, arrayAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declA, declX)))
        result.hasErrors shouldBe false
        result.safeArrayAccesses should contain(arrayAccess)
    }

    it should "detect out of bounds for a + b where result exceeds length" in {
        /* int[] a = [1,2,3]; int x = a[2 + 3]*/
        val aIdent = varIdent("a", 0, KnownType.Array(KnownType.Int))
        val xIdent = varIdent("x", 1)

        val declA = declArray(aIdent, List(IntLiter(1, typedInt), IntLiter(2, typedInt), IntLiter(3, typedInt)))

        val addExpr = BinaryExpr(BinaryOperator.Add, IntLiter(2, typedInt), IntLiter(3, typedInt), typedInt)
        val arrayAccess = ArrayElem(aIdent, List(addExpr), SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, arrayAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declA, declX)))
        result.hasErrors shouldBe true
    }

    "Static analyser for pair inner type tracking" should "track inner types from newpair and not warn on matching access" in {
        /* pair(int, int) p = newpair(1, 2); int x = fst p */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)
        val typedPair = SemanticData.Typed((1, 1), pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair), semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.diagnostics.exists(_.message.contains("Unsound pair coercion")) shouldBe false
    }

    it should "warn when fst access expects different type than stored" in {
        /* pair(int, int) p = newpair(1, 2) */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1, KnownType.Bool)
        val typedPair = SemanticData.Typed((1, 1), pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair), semPos
        )

        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Bool))
        val declX = Stmt.Decl(SyntaxType.Bool, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.diagnostics.exists(_.message.contains("Unsound pair coercion")) shouldBe true
    }

    it should "still mark pair access as safe for null check elimination" in {
        /* PairInfo implies non-null, so fst access on a newpair should be safe */
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = varIdent("p", 0, pairType)
        val xIdent = varIdent("x", 1)
        val typedPair = SemanticData.Typed((1, 1), pairType)

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), pIdent,
            NewPair(IntLiter(1, typedInt), IntLiter(2, typedInt), typedPair), semPos
        )
        val fstAccess = PairElem(true, pIdent, SemanticData.Typed((2, 1), KnownType.Int))
        val declX = Stmt.Decl(SyntaxType.Int, xIdent, fstAccess, semPos)

        val result = StaticAnalyser.analyse(programWithStmts(List(declP, declX)))
        result.safePairAccesses should contain(fstAccess)
    }
}
