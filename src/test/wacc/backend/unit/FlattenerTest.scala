package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.{Flattener, Call as IRCall, Print as IRPrint, Label, Immediate, Mov, DataDirective,
                     PrintCharWidget, PrintIntWidget, PrintBoolWidget, PrintStringWidget, BranchCond,
                     BranchUncond,Cmp, SetCond, Not, And, Or, VirtualReg, Size, LabelDef, Condition,
                     Neg, Chr}
import wacc.frontend.*
import wacc.backend.VirtualReg
import wacc.backend.Size
import wacc.backend.HeapAlloc
import wacc.backend.PairStore
import wacc.backend.PairPos
import wacc.backend.NullCheck
import wacc.backend.PairLoad

/**
  * Unit tests for the Flattener
  */
class FlattenerTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal typed AST with no functions */
    private def programWith(stmt: Stmt[SemanticData]): Program[SemanticData] =
        Program(List.empty, stmt, SemanticData.Pos((0, 0)))

    private def programWith(stmts: List[Stmt[SemanticData]]): Program[SemanticData] =
        Program(List.empty, Stmt.Block(stmts, SemanticData.Pos(0, 0)), SemanticData.Pos((0, 0)))

    /** Dummy position annotation */
    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos

    "The flattener for exit" should "flatten with int literal to a Call instruction" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(7, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head shouldBe a [IRCall]

        result.body.head match {
            case call: IRCall =>
                call.label shouldBe Label("exit")
                call.args should have length 1
                call.args.head shouldBe Immediate(7)
                call.dest shouldBe None
            case _ => fail("Expected Call instruction")
        }
    }

    it should "flatten exit with zero exit code" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(0, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head match {
            case call: IRCall => call.args.head shouldBe Immediate(0)
            case _ => fail("Expected Call instruction")
        }
    }

    it should "flatten exit with negative exit code" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(-1, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head match {
            case call: IRCall => call.args.head shouldBe Immediate(-1)
            case _ => fail("Expected Call instruction")
        }
    }

    it should "flatten exit with large exit code" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(256, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head match {
            case call: IRCall => call.args.head shouldBe Immediate(256)
            case _ => fail("Expected Call instruction")
        }
    }

    it should "flatten a block containing an exit statement" in {
        val ast = programWith(
            Stmt.Block(List(
                Stmt.Exit(IntLiter(42, semPos), semPos)
            ), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head match {
            case call: IRCall =>
                call.label shouldBe Label("exit")
                call.args.head shouldBe Immediate(42)
            case _ => fail("Expected Call instruction")
        }
    }

    it should "flatten nested blocks with exit" in {
        val ast = programWith(
            Stmt.Block(List(
                Stmt.Block(List(
                    Stmt.Exit(IntLiter(99, semPos), semPos)
                ), semPos)
            ), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head match {
            case call: IRCall => call.args.head shouldBe Immediate(99)
            case _ => fail("Expected Call instruction")
        }
    }

    it should "produce no data directives for a simple exit" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(0, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.data shouldBe empty
    }

    it should "produce no function definitions for a simple exit" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(0, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.funcs shouldBe empty
    }

    "The flattener to a Call" should "pass the imm as a Call argument (not wrapping in Mov)" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(7, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        /* There should be exactly 1 instruction (the Call), no Mov instructions */
        result.body should have length 1
        result.body.head shouldBe a [IRCall]
        result.body.exists { case Mov(_, _) => true; case _ => false } shouldBe false
    }

    "The flattener for prints" should "flatten a simple print statement for a char" in {

        val printChar =
            Stmt.Print(CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)), semPos)

        val inputAST = programWith(printChar)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        val charVal = 97
        flatOutput.body shouldBe List(IRPrint(Immediate(charVal), PrintCharWidget, false))
    }

    it should "flatten a simple print statment for an int" in {

        val printInt =
            Stmt.Print(IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)

        val inputAST = programWith(printInt)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(IRPrint(Immediate(2), PrintIntWidget, false))
    }

    it should "flatten a simple print statment for a bool" in {

        val printBool =
            Stmt.Print(BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool)), semPos)

        val inputAST = programWith(printBool)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(IRPrint(Immediate(0), PrintBoolWidget, false))
    }

    it should "flatten a simple print statment for a string" in {

        val printStr =
            Stmt.Print(StrLiter("hello", SemanticData.Typed(synPos.pos, KnownType.String)), semPos)

        val inputAST = programWith(printStr)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe List(DataDirective(Label("string_0"), "hello"))
        flatOutput.funcs shouldBe Nil
        flatOutput.body shouldBe List(IRPrint(Label("string_0"), PrintStringWidget, false))
    }

    it should "flatten a simple println statment" in {
        val printlnInt =
            Stmt.Println(IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)

        val inputAST = programWith(printlnInt)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(IRPrint(Immediate(2), PrintIntWidget, true))
    }

    "The flattener for declarations" should "flatten a simple int declaration to a Mov" in {
        // int x = 5
        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int)),
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )
        
        val result = Flattener.flatten(programWith(decl))
        
        // Should generate: Mov(VirtualReg(0, W), Immediate(5))
        result.body should have length 1
        result.body.head shouldBe a [Mov]
        
        val mov: Mov = (result.body.head: @unchecked) match {
            case m: Mov => m
        }
        mov.src shouldBe Immediate(5)
        mov.reg should matchPattern { case VirtualReg(0, Size.W) => }
    }

    it should "flatten a variable usage correctly" in {
        // begin int x = 5; int y = x end

        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        
        val xDecl = Stmt.Decl(
            SyntaxType.Int,
            xIdent,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )
        
        val yDecl = Stmt.Decl(
            SyntaxType.Int,
            Ident("y", SemanticData.Var(synPos.pos, 0, KnownType.Int)),
            xIdent,
            semPos
        )

        val block = Stmt.Block(List(xDecl, yDecl), semPos)
        val result = Flattener.flatten(programWith(block))

        // Expect:
        // 1. Mov(v0, 5)
        // 2. Mov(v1, v0)
        result.body should have length 2
        
        val instr1: Mov = (result.body(0): @unchecked) match {
            case m: Mov => m
        }
        val instr2: Mov = (result.body(1): @unchecked) match {
            case m: Mov => m
        }

        instr1.src shouldBe Immediate(5)
        val xReg = instr1.reg

        instr2.reg should matchPattern { case VirtualReg(1, Size.W) => }
        instr2.src shouldBe xReg // The source of y's assignment should be x's register
    }

    "The flattener for assignments" should "update the existing register" in {
        // begin int x = 5; x = 10 end
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        
        val xDecl = Stmt.Decl(
            SyntaxType.Int,
            xIdent,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )
        
        val xAsgn = Stmt.Asgn(
            xIdent,
            IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )

        val block = Stmt.Block(List(xDecl, xAsgn), semPos)
        val result = Flattener.flatten(programWith(block))

        result.body should have length 2
        val declMov: Mov = (result.body(0): @unchecked) match {
            case m: Mov => m
        }
        val asgnMov: Mov = (result.body(1): @unchecked) match {
            case m: Mov => m
        }

        // Both instructions should target the same register
        declMov.reg shouldBe asgnMov.reg
        asgnMov.src shouldBe Immediate(10)
    }

    "The flattener for comparison operators" should "flatten greater than comparison" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val gtExpr = BinaryExpr(BinaryOperator.GT, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(gtExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        /* Should emit: Mov(left), Mov(right), Cmp, SetCond, Print */
        flatOutput.body.length should be >= 3
        
        /* Find the Cmp and SetCond instructions */
        val cmpInstrs = flatOutput.body.collect { case c: Cmp => c }
        val setCondInstrs = flatOutput.body.collect { case s: SetCond => s }
        
        cmpInstrs should have length 1
        setCondInstrs should have length 1
        setCondInstrs.head.cond shouldBe wacc.backend.Condition.GT
    }

    it should "flatten less than or equal comparison" in {
        val leftExpr = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val lteExpr = BinaryExpr(BinaryOperator.LTE, leftExpr, rightExpr,
                                 SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(lteExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val setCondInstrs = flatOutput.body.collect { case s: SetCond => s }
        setCondInstrs should have length 1
        setCondInstrs.head.cond shouldBe wacc.backend.Condition.LE
    }

    it should "flatten equality comparison" in {
        val leftExpr = IntLiter(7, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(7, SemanticData.Typed(synPos.pos, KnownType.Int))
        val eqExpr = BinaryExpr(BinaryOperator.EQ, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(eqExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val setCondInstrs = flatOutput.body.collect { case s: SetCond => s }
        setCondInstrs.head.cond shouldBe wacc.backend.Condition.EQ
    }

    it should "flatten not equal comparison" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val neqExpr = BinaryExpr(BinaryOperator.NEQ, leftExpr, rightExpr,
                                 SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(neqExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val setCondInstrs = flatOutput.body.collect { case s: SetCond => s }
        setCondInstrs.head.cond shouldBe wacc.backend.Condition.NE
    }

    it should "flatten comparison and emit Cmp instruction" in {
        val leftExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int))
        val ltExpr = BinaryExpr(BinaryOperator.LT, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(ltExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val cmpInstrs = flatOutput.body.collect { case c: Cmp => c }
        cmpInstrs should have length 1
        
        /* Both operands should be virtual registers */
        cmpInstrs.head.src1 shouldBe a [VirtualReg]
        cmpInstrs.head.src2 shouldBe a [VirtualReg]
    }

    "The flattener for boolean operators" should "flatten NOT operator" in {
        val boolExpr = BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val notExpr = UnaryExpr(UnaryOperator.Not, boolExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(notExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        /* Should contain Not instruction */
        val notInstrs = flatOutput.body.collect { case n: Not => n }
        notInstrs should have length 1
    }

    it should "flatten AND operator" in {
        val leftExpr = BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val rightExpr = BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val andExpr = BinaryExpr(BinaryOperator.And, leftExpr, rightExpr,
                                 SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(andExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        /* Should contain And instruction */
        val andInstrs = flatOutput.body.collect { case a: And => a }
        andInstrs should have length 1
    }

    it should "flatten OR operator" in {
        val leftExpr = BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val rightExpr = BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val orExpr = BinaryExpr(BinaryOperator.Or, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(orExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        /* Should contain Or instruction */
        val orInstrs = flatOutput.body.collect { case o: Or => o }
        orInstrs should have length 1
    }

    "The flattener for if statements" should "flatten an if statement" in {

        val blockTypes = SemanticData.Typed(synPos.pos, KnownType.Int)

        val cond = BinaryExpr(BinaryOperator.LT,
                              IntLiter(2, blockTypes),
                              IntLiter(5, blockTypes),
                              SemanticData.Typed(synPos.pos, KnownType.Bool))

        val ifBlock = Stmt.Print(StrLiter("hello!", blockTypes), semPos)
        val elseBlock = Stmt.Print(StrLiter("good bye!", blockTypes), semPos)

        val ifStmt = Stmt.If(cond, ifBlock, elseBlock, semPos)

        val inputAST = programWith(ifStmt)

        val instrs = Flattener.flatten(inputAST).body

        val elseBranch = instrs.collectFirst { case BranchCond(_, label) => label }
        elseBranch should be (defined)
        val elseLabel = elseBranch.get

        val endBranch = instrs.collectFirst { case BranchUncond(label) => label }
        endBranch should be (defined)
        val endLabel = endBranch.get

        /* Label defs should exist */
        instrs should contain (LabelDef(elseLabel))
        instrs should contain (LabelDef(endLabel))

        /* else label branching should be before its definition */
        instrs.indexOf(BranchCond(Condition.EQ, elseLabel)) should be < instrs.indexOf(LabelDef(elseLabel))
        
        /* end label branching should be before its definition */
        instrs.indexOf(BranchUncond(endLabel)) should be < instrs.indexOf(LabelDef(endLabel))
    }

    "The flattener for while statements" should "flatten a while statement" in {

        val blockTypes = SemanticData.Typed(synPos.pos, KnownType.Int)

        val cond = BinaryExpr(BinaryOperator.LT,
                              IntLiter(2, blockTypes),
                              IntLiter(5, blockTypes),
                              SemanticData.Typed(synPos.pos, KnownType.Bool))

        val body = Stmt.Print(IntLiter(1, blockTypes), semPos)
        val whileStmt = Stmt.While(cond, body, semPos)

        val inputAST = programWith(whileStmt)
        val instrs = Flattener.flatten(inputAST).body

        val condBranch = instrs.collectFirst { case BranchUncond(label) => label }
        condBranch should be (defined)
        val condLabel = condBranch.get

        val bodyBranch = instrs.collectFirst { case BranchCond(_, label) => label }
        bodyBranch should be (defined)
        val bodyLabel = bodyBranch.get

        /* Label defs should exist */
        instrs should contain (LabelDef(condLabel))
        instrs should contain (LabelDef(bodyLabel))

        /* cond label branching should be before its definition */
        instrs.indexOf(BranchUncond(condLabel)) should be < instrs.indexOf(LabelDef(condLabel))
        
        /* body label branching should be after its definition */
        instrs.indexOf(BranchCond(Condition.EQ, bodyLabel)) should be > instrs.indexOf(LabelDef(bodyLabel))
    }

    "The flattener for unary operators" should "flatten the neg operator" in {
        val intExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val negExpr = UnaryExpr(UnaryOperator.Neg, intExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(negExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val negInstrs = flatOutput.body.collect { case n: Neg => n }
        negInstrs should have length 1
        
        /* Check that dest and src are different */
        negInstrs.head match {
            case Neg(dest, src) => dest should not be src
        }
    }

    it should "flatten chr operator" in {
        val intExpr = IntLiter(97, SemanticData.Typed(synPos.pos, KnownType.Int))
        val chrExpr = UnaryExpr(UnaryOperator.Chr, intExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Char))

        val printStmt = Stmt.Print(chrExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val chrInstrs = flatOutput.body.collect { case c: Chr => c }
        chrInstrs should have length 1
        
        /* Needs the error widget */
        chrInstrs.head.widget should not be None
    }

    "The flattener for pairs" should "flatten a newpair expression into a HeapAlloc and PairStores" in {
        // pair(int, int) p = newpair(10, 20)
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(KnownType.Int, KnownType.Int)))
        
        val decl = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int))),
            newPair,
            semPos
        )
        
        val flatOutput = Flattener.flatten(programWith(decl))

        // should see a HeapAlloc of 16 bytes
        val allocs = flatOutput.body.collect { case h: HeapAlloc => h }
        allocs should have length 1
        allocs.head.size shouldBe Immediate(16) // PAIR_HEAP_SIZE_B

        // should see exactly two PairStores (for fst and snd)
        val stores = flatOutput.body.collect { case p: PairStore => p }
        stores should have length 2
        
        // Ensure Fst and Snd are both stored
        stores.map(_.index) should contain allOf (PairPos.Fst, PairPos.Snd)
    }

    it should "flatten reading from a pair (fst/snd) with a NullCheck" in {
        // pair(int, int) p = newpair(10, 20)
        // int x = fst p
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(KnownType.Int, KnownType.Int)))
        
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int))),
            newPair,
            semPos
        )
        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int)))
        val fstElem = PairElem(isFst = true, pairIdent, SemanticData.Typed(synPos.pos, KnownType.Int))
        
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int)), fstElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, decl)))

        // should explicitly emit a NullCheck before loading
        val nullChecks = flatOutput.body.collect { case n: NullCheck => n }
        nullChecks should have length 1

        // should emit a PairLoad with Fst
        val loads = flatOutput.body.collect { case p: PairLoad => p }
        loads should have length 1
        loads.head.index shouldBe PairPos.Fst
    }

    it should "flatten writing to a pair (fst p = 5) with a NullCheck" in {
        // pair(int, int) p = newpair(10, 20)
        // fst p = 5
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(KnownType.Int, KnownType.Int)))
        
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int))),
            newPair,
            semPos
        )

        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(KnownType.Int, KnownType.Int)))
        val fstElem = PairElem(isFst = true, pairIdent, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rhs = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        
        val asgn = Stmt.Asgn(fstElem, rhs, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, asgn)))

        // Should explicitly emit a NullCheck before storing
        val nullChecks = flatOutput.body.collect { case n: NullCheck => n }
        nullChecks should have length 1

        // Should emit a PairStore with Fst
        val stores = flatOutput.body.collect { case p: PairStore => p }
        stores should have length 3
        stores.head.index shouldBe PairPos.Fst
    }

}
