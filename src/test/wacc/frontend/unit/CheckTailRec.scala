package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.frontend.*
import wacc.testhelpers.*

class CheckTailRecTests extends AnyFlatSpec with TailRecPrebuiltTrees {
    "The tail recursion checker" should "identify a simple self tail recursive call" in {

        val (ast, calls) = simpleTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(ast)

        generalTailCalls.size shouldBe 0
        selfTailCalls.size shouldBe 1
        selfTailCalls.toList(0) shouldBe calls(0)
    }

    it should "identify a self tail recursive call in an if statement" in {

        val (inputAST, calls) = ifStmtTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        generalTailCalls.size shouldBe 0
        selfTailCalls.size shouldBe 1
        selfTailCalls.toList(0) shouldBe calls(0)
    }

    it should "identify a self tail recursive call in a while statement" in {

        val (ast, calls) = whileStmtTailRec()
        
        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(ast)

        generalTailCalls.size shouldBe 0
        selfTailCalls.size shouldBe 1
        selfTailCalls.toList(0) shouldBe calls(0)
    }
    
    it should "not mark a non-tail recursive call as tail-recursive" in {
        val (inputAST, _) = basicNonTailrec()
        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)
        generalTailCalls.size shouldBe 0
        selfTailCalls.size shouldBe 0
    }

    it should "mark a self tailrec node as self tail rec and not generalised" in {
        val (inputAST, _) = nonSelfTailRecursive() 
        
        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)
        generalTailCalls.size shouldBe 1
        selfTailCalls.size shouldBe 0
    }

    it should "allow for assignment statements for tail recursive patterns" in {

        val (inputAST, calls) = assignmentTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        generalTailCalls.size shouldBe 0
        selfTailCalls.size shouldBe 1
        selfTailCalls.toList(0) shouldBe calls(0)
    }

    it should "find multiple tailrec calls and ignore non-tail rec ones in the same function" in {

        val (inputAST, calls) = threeCallsTwoTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        generalTailCalls.size shouldBe 0

        selfTailCalls.size shouldBe 2
        selfTailCalls should contain (calls(0))
        selfTailCalls should contain (calls(1))
    }

    it should "mark a general tail call as tailrec when parent stack args less than child stack args" in {
        val (inputAST, calls) = safeManyArgsGeneralTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        selfTailCalls.size shouldBe 0
        generalTailCalls.size shouldBe 1
        generalTailCalls.toList(0) shouldBe calls(0)
    }

    it should "ignore a general tail call when parent stack args greater than child stack args" in {
        val (inputAST, _) = unsafeManyArgsGeneralTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        selfTailCalls.size shouldBe 0
        generalTailCalls.size shouldBe 0
    }

    it should "identify a general tail call when it has 8 arguments regardless of incoming args" in {
        val (inputAST, calls) = exactlyEightArgsGeneralTailRec()

        val (selfTailCalls, generalTailCalls) = CheckTailRec.checkTailRec(inputAST)

        selfTailCalls.size shouldBe 0
        generalTailCalls.size shouldBe 1
        generalTailCalls.toList(0) shouldBe calls(0)
    }
}

