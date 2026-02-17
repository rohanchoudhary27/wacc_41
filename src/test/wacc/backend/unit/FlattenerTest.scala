package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.{Flattener, Call as IRCall, Label, Immediate, Mov}
import wacc.frontend.{SemanticData, IntLiter, Stmt, Program}

/**
  * Unit tests for the Flattener
  */
class FlattenerTest extends AnyFlatSpec with Matchers {

    /** Helper to create a minimal typed AST with no functions */
    private def programWith(stmt: Stmt[SemanticData]): Program[SemanticData] =
        Program(List.empty, stmt, SemanticData.Pos((0, 0)))

    /** Dummy position annotation */
    private val pos: SemanticData = SemanticData.Pos((1, 1))

    "Flattener" should "flatten exit with int literal to a Call instruction" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(7, pos), pos)
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
            Stmt.Exit(IntLiter(0, pos), pos)
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
            Stmt.Exit(IntLiter(-1, pos), pos)
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
            Stmt.Exit(IntLiter(256, pos), pos)
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
                Stmt.Exit(IntLiter(42, pos), pos)
            ), pos)
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
                    Stmt.Exit(IntLiter(99, pos), pos)
                ), pos)
            ), pos)
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
            Stmt.Exit(IntLiter(0, pos), pos)
        )
        val result = Flattener.flatten(ast)

        result.data shouldBe empty
    }

    it should "produce no function definitions for a simple exit" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(0, pos), pos)
        )
        val result = Flattener.flatten(ast)

        result.funcs shouldBe empty
    }

    it should "pass the immediate directly as a Call argument (not wrapping in a Mov)" in {
        val ast = programWith(
            Stmt.Exit(IntLiter(7, pos), pos)
        )
        val result = Flattener.flatten(ast)

        /* There should be exactly 1 instruction (the Call), no Mov instructions */
        result.body should have length 1
        result.body.head shouldBe a [IRCall]
        result.body.exists { case Mov(_, _) => true; case _ => false } shouldBe false
    }
}
