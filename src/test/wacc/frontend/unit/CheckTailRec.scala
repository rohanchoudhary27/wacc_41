package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.frontend.*
import wacc.testhelpers.*

class CheckTailRecTests extends AnyFlatSpec with TailRecPrebuiltTrees {

    "The tail recursion checker" should "identify a simple tail recursive call" in {

        val (ast, calls) = simpleTailRec()

        val tailCalls = CheckTailRec.checkTailRec(ast)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe calls(0)
    }

    it should "identify a tail recursive call in an if statement" in {

        val (inputAST, calls) = ifStmtTailRec()

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe calls(0)
    }

    it should "identify a tail recursive call in a while statement" in {

        val (ast, calls) = whileStmtTailRec()
        
        val tailCalls = CheckTailRec.checkTailRec(ast)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe calls(0)
    }
    
    it should "not mark a non-tail recursive call as tail-recursive" in {
        val (inputAST, _) = basicNonTailrec()
        val tailCalls = CheckTailRec.checkTailRec(inputAST)
        tailCalls.size shouldBe 0
    }

    it should "only mark a tailrec node as tailrec for self-recursion" in {
        val (inputAST, _) = basicNonTailrec()
        val tailCalls = CheckTailRec.checkTailRec(inputAST)
        tailCalls.size shouldBe 0
    }

    it should "allow for assignment statements for tail recursive patterns" in {

        val (inputAST, calls) = assignmentTailRec()

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe calls(0)
    }

    it should "find multiple tailrec calls and ignore non-tail rec ones in the same function" in {

        val (inputAST, calls) = threeCallsTwoTailRec()

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 2
        tailCalls should contain (calls(0))
        tailCalls should contain (calls(2))
    }

}
