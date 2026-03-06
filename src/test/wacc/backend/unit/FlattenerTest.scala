package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.{Flattener}
import wacc.frontend.*
import wacc.testhelpers.*
import wacc.backend.TAC
import wacc.backend.*

/**
  * Unit tests for the Flattener
  */
class FlattenerTest extends AnyFlatSpec with Matchers with TailRecPrebuiltTrees {

    /** Helper to create a minimal typed AST with no functions */
    private def programWith(stmt: Stmt[SemanticData]): Program[SemanticData] =
        Program(List.empty, stmt, SemanticData.Pos((0, 0)))

    private def programWith(stmts: List[Stmt[SemanticData]]): Program[SemanticData] =
        Program(List.empty, Stmt.Block(stmts, SemanticData.Pos(0, 0)), SemanticData.Pos((0, 0)))

    "The flattener for exit" should "flatten with int literal to a Call instruction" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(7, semPos), semPos)
        )
        val result = Flattener.flatten(ast)

        result.body should have length 1
        result.body.head shouldBe a [TAC.Call]

        result.body.head match {
            case call: TAC.Call =>
                call.label shouldBe TAC.Label("exit")
                call.args should have length 1
                call.args.head shouldBe TAC.Immediate(7)
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
            case call: TAC.Call => call.args.head shouldBe TAC.Immediate(0)
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
            case call: TAC.Call => call.args.head shouldBe TAC.Immediate(-1)
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
            case call: TAC.Call => call.args.head shouldBe TAC.Immediate(256)
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
            case call: TAC.Call =>
                call.label shouldBe TAC.Label("exit")
                call.args.head shouldBe TAC.Immediate(42)
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
            case call: TAC.Call => call.args.head shouldBe TAC.Immediate(99)
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
        result.body.head shouldBe a [TAC.Call]
        result.body.exists { case TAC.Mov(_, _) => true; case _ => false } shouldBe false
    }

    "The flattener for prints" should "flatten a simple print statement for a char" in {

        val printChar =
            Stmt.Print(CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)), semPos)

        val inputAST = programWith(printChar)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        val charVal = 97
        flatOutput.body shouldBe List(TAC.Print(TAC.Immediate(charVal), PrintCharWidget, false))
    }

    it should "flatten a simple print statment for an int" in {

        val printInt =
            Stmt.Print(IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)

        val inputAST = programWith(printInt)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(TAC.Print(TAC.Immediate(2), PrintIntWidget, false))
    }

    it should "flatten a simple print statment for a bool" in {

        val printBool =
            Stmt.Print(BoolLiter(false, SemanticData.Typed(synPos.pos, KnownType.Bool)), semPos)

        val inputAST = programWith(printBool)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(TAC.Print(TAC.Immediate(0), PrintBoolWidget, false))
    }

    it should "flatten a simple print statment for a string" in {

        val printStr =
            Stmt.Print(StrLiter("hello", SemanticData.Typed(synPos.pos, KnownType.String)), semPos)

        val inputAST = programWith(printStr)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe List(TAC.DataDirective(TAC.Label("string_0"), "hello"))
        flatOutput.funcs shouldBe Nil
        flatOutput.body shouldBe List(TAC.Print(TAC.Label("string_0"), PrintStringWidget, false))
    }

    it should "flatten a simple println statment" in {
        val printlnInt =
            Stmt.Println(IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int)), semPos)

        val inputAST = programWith(printlnInt)

        val flatOutput = Flattener.flatten(inputAST) 

        flatOutput.data shouldBe Nil
        flatOutput.funcs shouldBe Nil

        flatOutput.body shouldBe List(TAC.Print(TAC.Immediate(2), PrintIntWidget, true))
    }

    "The flattener for read statements" should "flatten a read int statement" in {
        // begin int x = 0; read x end
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        
        val xDecl = Stmt.Decl(
            SyntaxType.Int,
            xIdent,
            IntLiter(0, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )
        
        val readStmt = Stmt.Read(xIdent, semPos)

        val block = Stmt.Block(List(xDecl, readStmt), semPos)
        val result = Flattener.flatten(programWith(block))

        // Should generate: Mov(VirtualReg(0, W), Immediate(0)), Read(VirtualReg(0, W), ReadIntegerWidget)
        result.body should have length 2
        
        val decl: TAC.Mov = (result.body(0): @unchecked) match {
            case m: TAC.Mov => m
        }
        val read: TAC.Read = (result.body(1): @unchecked) match {
            case r: TAC.Read => r
        }

        decl.reg should matchPattern { case TAC.VirtualReg(0, Size.W) => }
        read.dest shouldBe decl.reg
        read.widget shouldBe ReadIntegerWidget
    }

    it should "flatten a read char statement" in {
        // begin char c = 'a'; read c end
        val cIdent = Ident("c", SemanticData.Var(synPos.pos, 0, KnownType.Char))
        
        val cDecl = Stmt.Decl(
            SyntaxType.Char,
            cIdent,
            CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char)),
            semPos
        )
        
        val readStmt = Stmt.Read(cIdent, semPos)

        val block = Stmt.Block(List(cDecl, readStmt), semPos)
        val result = Flattener.flatten(programWith(block))

        // Should generate: Mov(VirtualReg(0, B), Immediate(97)), Read(VirtualReg(0, B), ReadCharWidget)
        result.body should have length 2
        
        val read: TAC.Read = (result.body(1): @unchecked) match {
            case r: TAC.Read => r
        }

        read.widget shouldBe ReadCharWidget
        read.dest should matchPattern { case TAC.VirtualReg(0, Size.B) => }
    }

    it should "flatten a read into pair fst element" in {
        // pair(int, int) p = newpair(10, 20)
        // read fst p
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
        val readStmt = Stmt.Read(fstElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, readStmt)))

        // Should explicitly emit a NullCheck before accessing
        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 1

        // Should emit a PairLoad to load current value, then Read, then PairStore
        val loads = flatOutput.body.collect { case p: TAC.PairLoad => p }
        loads should have length 1
        loads.head.index shouldBe PairPos.Fst
        
        val reads = flatOutput.body.collect { case r: TAC.Read => r }
        reads should have length 1
        reads.head.widget shouldBe ReadIntegerWidget
        
        val stores = flatOutput.body.collect { case p: TAC.PairStore => p }
        stores should have length 3  // 2 from newpair init + 1 from read
        stores.last.index shouldBe PairPos.Fst  // The read's store should be last
    }

    it should "flatten a read into pair snd element with char" in {
        // pair(int, char) p = newpair(10, 'a')
        // read snd p
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val e2 = CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char))
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Char)
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, pairType))
        
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Char),
            Ident("p", SemanticData.Var(synPos.pos, 0, pairType)),
            newPair,
            semPos
        )
        
        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, pairType))
        val sndElem = PairElem(isFst = false, pairIdent, SemanticData.Typed(synPos.pos, KnownType.Char))
        val readStmt = Stmt.Read(sndElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, readStmt)))

        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 1

        val loads = flatOutput.body.collect { case p: TAC.PairLoad => p }
        loads should have length 1
        loads.head.index shouldBe PairPos.Snd
        
        val reads = flatOutput.body.collect { case r: TAC.Read => r }
        reads should have length 1
        reads.head.widget shouldBe ReadCharWidget
        
        val stores = flatOutput.body.collect { case p: TAC.PairStore => p }
        stores should have length 3
        stores.last.index shouldBe PairPos.Snd
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
        result.body.head shouldBe a [TAC.Mov]
        
        val mov: TAC.Mov = (result.body.head: @unchecked) match {
            case m: TAC.Mov => m
        }
        mov.src shouldBe TAC.Immediate(5)
        mov.reg should matchPattern { case TAC.VirtualReg(0, Size.W) => }
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
        
        val instr1: TAC.Mov = (result.body(0): @unchecked) match {
            case m: TAC.Mov => m
        }
        val instr2: TAC.Mov = (result.body(1): @unchecked) match {
            case m: TAC.Mov => m
        }

        instr1.src shouldBe TAC.Immediate(5)
        val xReg = instr1.reg

        instr2.reg should matchPattern { case TAC.VirtualReg(1, Size.W) => }
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
        val declMov: TAC.Mov = (result.body(0): @unchecked) match {
            case m: TAC.Mov => m
        }
        val asgnMov: TAC.Mov = (result.body(1): @unchecked) match {
            case m: TAC.Mov => m
        }

        // Both instructions should target the same register
        declMov.reg shouldBe asgnMov.reg
        asgnMov.src shouldBe TAC.Immediate(10)
    }

    "The flattener for maths expressions" should "flatten addition" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val addExpr = BinaryExpr(BinaryOperator.Add, leftExpr, rightExpr, SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(addExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val addInstrs = flatOutput.body.collect { case a: TAC.Add => a }
        addInstrs should have length 1
        addInstrs.head.src1 shouldBe a [TAC.VirtualReg]
    }

    it should "flatten subtraction" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val subExpr = BinaryExpr(BinaryOperator.Sub, leftExpr, rightExpr, SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(subExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val subInstrs = flatOutput.body.collect { case s: TAC.Sub => s }
        subInstrs should have length 1
    }

    it should "flatten multiplication" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val mulExpr = BinaryExpr(BinaryOperator.Mul, leftExpr, rightExpr, SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(mulExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val mulInstrs = flatOutput.body.collect { case m: TAC.Mul => m }
        mulInstrs should have length 1
    }

    it should "flatten division" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val divExpr = BinaryExpr(BinaryOperator.Div, leftExpr, rightExpr, SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(divExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val divInstrs = flatOutput.body.collect { case x: TAC.Div => x }
        divInstrs should have length 1
    }

    it should "flatten modulo" in {
        val leftExpr = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val modExpr = BinaryExpr(BinaryOperator.Mod, leftExpr, rightExpr, SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(modExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val modInstrs = flatOutput.body.collect { case x: TAC.Mod => x }
        modInstrs should have length 1
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
        val cmpInstrs = flatOutput.body.collect { case c: TAC.Cmp => c }
        val setCondInstrs = flatOutput.body.collect { case s: TAC.SetCond => s }
        
        cmpInstrs should have length 1
        setCondInstrs should have length 1
        setCondInstrs.head.cond shouldBe Condition.GT
    }

    it should "flatten less than or equal comparison" in {
        val leftExpr = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val lteExpr = BinaryExpr(BinaryOperator.LTE, leftExpr, rightExpr,
                                 SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(lteExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val setCondInstrs = flatOutput.body.collect { case s: TAC.SetCond => s }
        setCondInstrs should have length 1
        setCondInstrs.head.cond shouldBe Condition.LE
    }

    it should "flatten equality comparison" in {
        val leftExpr = IntLiter(7, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(7, SemanticData.Typed(synPos.pos, KnownType.Int))
        val eqExpr = BinaryExpr(BinaryOperator.EQ, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(eqExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val setCondInstrs = flatOutput.body.collect { case s: TAC.SetCond => s }
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

        val setCondInstrs = flatOutput.body.collect { case s: TAC.SetCond => s }
        setCondInstrs.head.cond shouldBe Condition.NE
    }

    it should "flatten comparison and emit Cmp instruction" in {
        val leftExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val rightExpr = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int))
        val ltExpr = BinaryExpr(BinaryOperator.LT, leftExpr, rightExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(ltExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val cmpInstrs = flatOutput.body.collect { case c: TAC.Cmp => c }
        cmpInstrs should have length 1
        
        /* Both operands should be virtual registers */
        cmpInstrs.head.src1 shouldBe a [TAC.VirtualReg]
        cmpInstrs.head.src2 shouldBe a [TAC.VirtualReg]
    }

    "The flattener for boolean operators" should "flatten NOT operator" in {
        val boolExpr = BoolLiter(true, SemanticData.Typed(synPos.pos, KnownType.Bool))
        val notExpr = UnaryExpr(UnaryOperator.Not, boolExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Bool))

        val printStmt = Stmt.Print(notExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        /* Should contain Not instruction */
        val notInstrs = flatOutput.body.collect { case n: TAC.Not => n }
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
        val andInstrs = flatOutput.body.collect { case a: TAC.And => a }
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
        val orInstrs = flatOutput.body.collect { case o: TAC.Or => o }
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

        val elseBranch = instrs.collectFirst { case TAC.BranchCond(_, label) => label }
        elseBranch should be (defined)
        val elseLabel = elseBranch.get

        val endBranch = instrs.collectFirst { case TAC.BranchUncond(label) => label }
        endBranch should be (defined)
        val endLabel = endBranch.get

        /* Label defs should exist */
        instrs should contain (TAC.LabelDef(elseLabel))
        instrs should contain (TAC.LabelDef(endLabel))

        /* else label branching should be before its definition */
        instrs.indexOf(TAC.BranchCond(Condition.EQ, elseLabel)) should be < instrs.indexOf(TAC.LabelDef(elseLabel))
        
        /* end label branching should be before its definition */
        instrs.indexOf(TAC.BranchUncond(endLabel)) should be < instrs.indexOf(TAC.LabelDef(endLabel))
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

        val condBranch = instrs.collectFirst { case TAC.BranchUncond(label) => label }
        condBranch should be (defined)
        val condLabel = condBranch.get

        val bodyBranch = instrs.collectFirst { case TAC.BranchCond(_, label) => label }
        bodyBranch should be (defined)
        val bodyLabel = bodyBranch.get

        /* Label defs should exist */
        instrs should contain (TAC.LabelDef(condLabel))
        instrs should contain (TAC.LabelDef(bodyLabel))

        /* cond label branching should be before its definition */
        instrs.indexOf(TAC.BranchUncond(condLabel)) should be < instrs.indexOf(TAC.LabelDef(condLabel))
        
        /* body label branching should be after its definition */
        instrs.indexOf(TAC.BranchCond(Condition.EQ, bodyLabel)) should be > instrs.indexOf(TAC.LabelDef(bodyLabel))
    }

    "The flattener for unary operators" should "flatten the neg operator" in {
        val intExpr = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int))
        val negExpr = UnaryExpr(UnaryOperator.Neg, intExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Int))

        val printStmt = Stmt.Print(negExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val negInstrs = flatOutput.body.collect { case n: TAC.Neg => n }
        negInstrs should have length 1
        
        /* Check that dest and src are different */
        negInstrs.head match {
            case TAC.Neg(dest, src) => dest should not be src
        }
    }

    it should "flatten chr operator" in {
        val intExpr = IntLiter(97, SemanticData.Typed(synPos.pos, KnownType.Int))
        val chrExpr = UnaryExpr(UnaryOperator.Chr, intExpr,
                                SemanticData.Typed(synPos.pos, KnownType.Char))

        val printStmt = Stmt.Print(chrExpr, semPos)
        val inputAST = programWith(printStmt)

        val flatOutput = Flattener.flatten(inputAST)

        val chrInstrs = flatOutput.body.collect { case c: TAC.Chr => c }
        chrInstrs should have length 1
        
        /* Needs the error widget */
        chrInstrs.head.widget should not be None
    }

    "The flattener for functions" should "flatten a function definition and its return" in {

        val funcTable: Map[String, SemanticData.Func] = Map(
            "f" -> SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        )

        val xVarInfo = SemanticData.Var(synPos.pos, 1, KnownType.Int)
        val xIdent = Ident("x", xVarInfo)
        val paramX = Param(SyntaxType.Int, xIdent, semPos)

        val returnStmt = Stmt.Return(xIdent, semPos)

        val funcSignature = funcTable("f")
        val funcF = Func(
            SyntaxType.Int, 
            Ident("f", funcSignature), 
            List(paramX), 
            returnStmt, 
            semPos
        )

        val mainBody: Stmt[SemanticData] = Stmt.Block(List(Stmt.Skip(semPos)), semPos)

        val inputAST = Program(
            List(funcF),
            mainBody,
            SemanticData.TopLevel(funcTable)
        )

        val flatFuncs = Flattener.flatten(inputAST).funcs

        flatFuncs.length shouldBe 1
        flatFuncs.head.args shouldBe List(TAC.VirtualReg(0, Size.W))
        flatFuncs.head.body shouldBe List(TAC.Mov(TAC.VirtualReg(-1, Size.W), TAC.VirtualReg(0, Size.W)), TAC.Ret)
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
        val allocs = flatOutput.body.collect { case h: TAC.HeapAlloc => h }
        allocs should have length 1
        allocs.head.size shouldBe TAC.Immediate(MemoryLayout.PAIR_HEAP_SIZE_B)

        // should see exactly two PairStores (for fst and snd)
        val stores = flatOutput.body.collect { case p: TAC.PairStore => p }
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
        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 1

        // should emit a PairLoad with Fst
        val loads = flatOutput.body.collect { case p: TAC.PairLoad => p }
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
        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 1

        // Should emit a PairStore with Fst
        val stores = flatOutput.body.collect { case p: TAC.PairStore => p }
        stores should have length 3
        stores.head.index shouldBe PairPos.Fst
    }

    it should "flatten reading from a nested pair (int x = snd fst p)" in {
        // pair(pair, int) p = newpair(null, 3)
        // int x = snd fst p
        val outerPairType = KnownType.Pair(PairAny, KnownType.Int)
        
        // AST: snd (fst p)
        val e1 = PairLiter(SemanticData.Typed(synPos.pos, Null))
        val e2 = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(Null, KnownType.Int)))
        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, outerPairType))
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            pairIdent,
            newPair,
            semPos
        )

        val innerFst = PairElem(isFst = true, pairIdent, SemanticData.Typed(synPos.pos, PairAny))
        val outerSnd = PairElem(isFst = false, innerFst, SemanticData.Typed(synPos.pos, KnownType.Int))
        
        // int x = snd fst p
        val decl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int)),
            outerSnd,
            semPos
        )
        val flatOutput = Flattener.flatten(programWith(List(declP, decl)))
        
        // We should see a sequence of: NullCheck -> PairLoad -> NullCheck -> PairLoad
        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 2
        
        val loads = flatOutput.body.collect { case p: TAC.PairLoad => p }
        loads should have length 2
        
        // Check the traversal order: first load Fst, then load Snd
        loads(0).index shouldBe PairPos.Fst
        loads(1).index shouldBe PairPos.Snd
        
        // The destination of the first load must be the base pointer of the second load
        loads(0).dest shouldBe loads(1).pair
    }

    it should "flatten writing to a nested pair (fst fst p = 5)" in {
        // pair(pair, int) p = newpair(null, 3)
        // fst fst p = 5
        val outerPairType = KnownType.Pair(PairAny, KnownType.Int)
        
        // snd (fst p)
        val e1 = PairLiter(SemanticData.Typed(synPos.pos, Null))
        val e2 = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(Null, KnownType.Int)))
        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, outerPairType))
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            pairIdent,
            newPair,
            semPos
        )
        
        // fst (fst p) = 5
        val innerFst = PairElem(isFst = true, pairIdent, SemanticData.Typed(synPos.pos, PairAny))
        val outerFst = PairElem(isFst = true, innerFst, SemanticData.Typed(synPos.pos, KnownType.Int))
        
        val rhs = IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int))
        val asgn = Stmt.Asgn(outerFst, rhs, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, asgn)))
        
        // we should see NullCheck -> PairLoad (to traverse) -> NullCheck -> PairStore (to write)
        val nullChecks = flatOutput.body.collect { case n: TAC.NullCheck => n }
        nullChecks should have length 2
        
        // there should be exactly one load (to find the inner pair)
        val loads = flatOutput.body.collect { case p: TAC.PairLoad => p }
        loads should have length 1
        loads.head.index shouldBe PairPos.Fst

        // there should be exactly 3 stores (two for the newpair and one to write the 5)
        val stores = flatOutput.body.collect { case p: TAC.PairStore => p }
        stores should have length 3
        stores.head.index shouldBe PairPos.Fst
        
        // the temporary pointer loaded by PairLoad must be the base pointer for the PairStore
        loads.head.dest shouldBe stores.last.pair
    }

    "The flattener for arrays" should "flatten an integer array literal into HeapAlloc, Store (for length), and ArrayStores" in {
        // int[] arr = [1, 2, 3]
        val e1 = IntLiter(1, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e3 = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(3))
        )
        
        val decl = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            arrayLit,
            semPos
        )
        
        val flatOutput = Flattener.flatten(programWith(decl))

        // should see a HeapAlloc of 16 bytes (4 for length + 3 * 4 for ints)
        val allocs = flatOutput.body.collect { case h: TAC.HeapAlloc => h }
        allocs should have length 1
        allocs.head.size shouldBe TAC.Immediate(16)

        // should see a Store for the length header
        val stores = flatOutput.body.collect { case s: TAC.Store => s }
        stores should have length 1
        stores.head.source match {
            case addr: TAC.AddrImmOffset => addr.offset shouldBe TAC.Immediate(0)
            case _ => fail("Expected AddrImmOffset")
        }

        // should see exactly 3 ArrayStores (one for each element)
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        arrayStores should have length 3
    }

    it should "flatten an empty array literal into HeapAlloc and Store with length 0" in {
        // int[] arr = []
        val arrayLit = ArrayLiter(
            List.empty,
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(0))
        )
        
        val decl = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            arrayLit,
            semPos
        )
        
        val flatOutput = Flattener.flatten(programWith(decl))

        // should see a HeapAlloc of 4 bytes (just the length header)
        val allocs = flatOutput.body.collect { case h: TAC.HeapAlloc => h }
        allocs should have length 1
        allocs.head.size shouldBe TAC.Immediate(4)

        // should see a Store for the length header
        val stores = flatOutput.body.collect { case s: TAC.Store => s }
        stores should have length 1

        // should see no ArrayStores (empty array)
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        arrayStores should have length 0
    }

    it should "flatten a char array literal with correct size calculation" in {
        // char[] arr = ['a', 'b', 'c', 'd']
        val e1 = CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val e2 = CharLiter('b', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val e3 = CharLiter('c', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val e4 = CharLiter('d', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3, e4),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Char), Some(4))
        )
        
        val decl = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Char),
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Char))),
            arrayLit,
            semPos
        )
        
        val flatOutput = Flattener.flatten(programWith(decl))

        // should see a HeapAlloc of 8 bytes (4 for length + 4 * 1 for chars)
        val allocs = flatOutput.body.collect { case h: TAC.HeapAlloc => h }
        allocs should have length 1
        allocs.head.size shouldBe TAC.Immediate(8)

        // should see exactly 4 ArrayStores (one for each character)
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        arrayStores should have length 4
    }

    "The flattener for array element access" should "read from a 1D array using ArrayLoad" in {
        // int[] arr = [10, 20, 30]
        // int x = arr[1]
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e3 = IntLiter(30, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(3))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        val arrayElem = ArrayElem(
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            List(IntLiter(1, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val xDecl = Stmt.Decl(SyntaxType.Int, xIdent, arrayElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, xDecl)))
        
        // should see ArrayLoad for reading arr[1]
        val arrayLoads = flatOutput.body.collect { case a: TAC.ArrayLoad => a }
        arrayLoads should have length 1
        arrayLoads.head.index shouldBe TAC.Immediate(1)
    }

    it should "write to a 1D array using ArrayStore" in {
        // int[] arr = [10, 20, 30]
        // arr[2] = 99
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e3 = IntLiter(30, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(3))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        val arrayElem = ArrayElem(
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            List(IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val assign = Stmt.Asgn(arrayElem, IntLiter(99, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)), semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, assign)))
        
        // should see ArrayStores: 3 for initialization + 1 for assignment
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        arrayStores.length should be > 3
        
        // the last ArrayStore should be the assignment with index 2 and value 99
        val assignStore = arrayStores.last
        assignStore.index shouldBe TAC.Immediate(2)
        assignStore.src shouldBe TAC.Immediate(99)
    }

    "The flattener for len operator" should "emit ArrayLen instruction for non-empty array" in {
        // int[] arr = [1, 2, 3]
        // int x = len arr
        val e1 = IntLiter(1, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e3 = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(3))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        // len arr
        val lenExpr = UnaryExpr(
            UnaryOperator.Len,
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val xDecl = Stmt.Decl(SyntaxType.Int, xIdent, lenExpr, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, xDecl)))
        
        // should see exactly one ArrayLen instruction
        val arrayLens = flatOutput.body.collect { case a: TAC.ArrayLen => a }
        arrayLens should have length 1
    }

    it should "emit ArrayLen instruction for empty array" in {
        // int[] arr = []
        // int x = len arr
        val arrayLit = ArrayLiter(
            List.empty,
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(0))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        // len arr
        val lenExpr = UnaryExpr(
            UnaryOperator.Len,
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val xDecl = Stmt.Decl(SyntaxType.Int, xIdent, lenExpr, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, xDecl)))
        
        // should see exactly one ArrayLen instruction
        val arrayLens = flatOutput.body.collect { case a: TAC.ArrayLen => a }
        arrayLens should have length 1
    }

    it should "work when len is used in an expression" in {
        // int[] arr = [10, 20]
        // int x = len arr + 5
        val e1 = IntLiter(10, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(20, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(2))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        // len arr + 5
        val lenExpr = UnaryExpr(
            UnaryOperator.Len,
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val addExpr = BinaryExpr(
            BinaryOperator.Add,
            lenExpr,
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val xIdent = Ident("x", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val xDecl = Stmt.Decl(SyntaxType.Int, xIdent, addExpr, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, xDecl)))
        
        // should see exactly one ArrayLen instruction
        val arrayLens = flatOutput.body.collect { case a: TAC.ArrayLen => a }
        arrayLens should have length 1
        
        // should also see Add instructions (at least one for the expression)
        val adds = flatOutput.body.collect { case a: TAC.Add => a }
        adds.length should be >= 1
    }

    "The flattener for read into array elements" should "read an int into an array element" in {
        // int[] arr = [1, 2, 3]
        // read arr[0]
        val e1 = IntLiter(1, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e2 = IntLiter(2, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val e3 = IntLiter(3, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(e1, e2, e3),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Int), Some(3))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Int), arrIdent, arrayLit, semPos)
        
        val arrayElem = ArrayElem(
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Int))),
            List(IntLiter(0, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))),
            SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None)
        )
        val readStmt = Stmt.Read(arrayElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, readStmt)))
        
        // should see a Read with ReadIntegerWidget
        val reads = flatOutput.body.collect { case r: TAC.Read => r }
        reads should have length 1
        reads.head.widget shouldBe ReadIntegerWidget
        
        // should see an ArrayStore after the read (to write back into the array)
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        // 3 from init + 1 from read writeback
        arrayStores should have length 4
    }

    it should "read a char into an array element" in {
        // char[] arr = ['a', 'b']
        // read arr[1]
        val c1 = CharLiter('a', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val c2 = CharLiter('b', SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None))
        val arrayLit = ArrayLiter(
            List(c1, c2),
            SemanticData.Typed(synPos.pos, KnownType.Array(KnownType.Char), Some(2))
        )
        val arrIdent = Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Char)))
        val arrDecl = Stmt.Decl(SyntaxType.Array(SyntaxType.Char), arrIdent, arrayLit, semPos)
        
        val arrayElem = ArrayElem(
            Ident("arr", SemanticData.Var(synPos.pos, 0, KnownType.Array(KnownType.Char))),
            List(IntLiter(1, SemanticData.Typed(synPos.pos, KnownType.Int, arrayLength=None))),
            SemanticData.Typed(synPos.pos, KnownType.Char, arrayLength=None)
        )
        val readStmt = Stmt.Read(arrayElem, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(arrDecl, readStmt)))
        
        // should see a Read with ReadCharWidget
        val reads = flatOutput.body.collect { case r: TAC.Read => r }
        reads should have length 1
        reads.head.widget shouldBe ReadCharWidget
        
        // should see an ArrayStore after the read (to write back into the array)
        val arrayStores = flatOutput.body.collect { case a: TAC.ArrayStore => a }
        // 2 from init + 1 from read writeback
        arrayStores should have length 3
    }
    
    "The flattener for free statements" should "emit a Free instruction for a pair variable" in {
        // pair(int, int) p = null
        // free p
        val pairType = KnownType.Pair(KnownType.Int, KnownType.Int)
        val pIdent = Ident("p", SemanticData.Var(synPos.pos, 0, pairType))
        
        val decl = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int), 
            pIdent, 
            PairLiter(SemanticData.Typed(synPos.pos, Null)), 
            semPos
        )
        val freeStmt = Stmt.Free(pIdent, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(decl, freeStmt)))
        
        // we should see exactly one free instruction
        val frees = flatOutput.body.collect { case f: TAC.Free => f }
        frees should have length 1
    }

    it should "emit a Free instruction for a nested pair variable" in {
        // pair(pair, int) p = newpair(null, 4)
        // free p
        val e1 = PairLiter(SemanticData.Typed(synPos.pos, Null))
        val e2 = IntLiter(4, SemanticData.Typed(synPos.pos, KnownType.Int))
        val newPair = NewPair(e1, e2, SemanticData.Typed(synPos.pos, KnownType.Pair(Null, KnownType.Int)))
        val pairIdent = Ident("p", SemanticData.Var(synPos.pos, 0, KnownType.Pair(PairAny, KnownType.Int)))
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            pairIdent,
            newPair,
            semPos
        )
        val freeStmt = Stmt.Free(pairIdent, semPos)
        
        val flatOutput = Flattener.flatten(programWith(List(declP, freeStmt)))
        
        // we should see exactly one free instruction
        val frees = flatOutput.body.collect { case f: TAC.Free => f }
        frees should have length 1
    }

    it should "throw an IllegalArgumentException if attempting to free an invalid type like Int" in {
        // int x = 5
        // free x
        val pIdent = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        
        val decl = Stmt.Decl(
            SyntaxType.Int, 
            pIdent, 
            IntLiter(5, SemanticData.Typed(synPos.pos, KnownType.Int)),
            semPos
        )
        val freeStmt = Stmt.Free(pIdent, semPos)
        
        // `require` statement in flattenExpr should throw
        val exception = intercept[IllegalArgumentException] {
            Flattener.flatten(programWith(List(decl, freeStmt)))
        }
        exception.getMessage should include ("Invalid type passed to 'free' statement. Must be an Array or Pair")
    }

    /* ========================================================================================== */
    /* ================================= EXTENSION TESTS ======================================== */
    /* ========================================================================================== */

    "The flattener for tail recursion" should "make a TailCall node instead of a Call" in {
        val (inputAST, callsThatAreTailrec) = threeCallsTwoTailRec()

        val optimState = FrontendOptimState(callsThatAreTailrec.toSet, Set.empty)
        val flatOutput = Flattener.flatten(inputAST, optimState)

        val testFunc = flatOutput.funcs.find(_.name == "f_f").getOrElse(
            fail("couldn't find function 'f_f' in flattened output")
        )

        val tailCallNodes = testFunc.body.collect { case call: TAC.SelfTailCall => call }
        tailCallNodes.length shouldBe 2

        tailCallNodes.foreach { tacCall =>
            tacCall.label.name should include ("f_f") 
        }
        
        val normalCallNodes = testFunc.body.collect { case call: TAC.Call => call }
        normalCallNodes.length shouldBe 1
    }
}
