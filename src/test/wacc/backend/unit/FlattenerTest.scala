package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.{Flattener, Call as IRCall, Print as IRPrint, Label, Immediate, Mov, DataDirective,
                     PrintCharWidget, PrintIntWidget, PrintBoolWidget, PrintStringWidget}
import wacc.frontend.*
import wacc.backend.VirtualReg
import wacc.backend.Size

/**
  * Unit tests for the Flattener
  */
class FlattenerTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal typed AST with no functions */
    private def programWith(stmt: Stmt[SemanticData]): Program[SemanticData] =
        Program(List.empty, stmt, SemanticData.Pos((0, 0)))

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
        
        val mov = result.body.head.asInstanceOf[Mov]
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
        
        val instr1 = result.body(0).asInstanceOf[Mov]
        val instr2 = result.body(1).asInstanceOf[Mov]

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
        val declMov = result.body(0).asInstanceOf[Mov]
        val asgnMov = result.body(1).asInstanceOf[Mov]

        // Both instructions should target the same register
        declMov.reg shouldBe asgnMov.reg
        asgnMov.src shouldBe Immediate(10)
    }

}
