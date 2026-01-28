package wacc.frontend.unit

import org.scalatest.matchers.should.Matchers.*
import org.scalatest.flatspec.AnyFlatSpec
import wacc.{RenamerScope, KnownType}
import wacc.{SemanticData,SourcePos}

/**
* Unit tests for RenamerScope
*/
class RenamerScopeUnitTest extends AnyFlatSpec {

    "A RenamerScope" should "allow the user to add and retrieve variables" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val scope = RenamerScope.empty
        scope.set("x", intVar)
        scope.lookup("x") should be (Some(intVar))
    }

    it should "return None for a variable that is not in the scope" in {
        val scope = RenamerScope.empty
        scope.lookup("x") should be (None)
    }

    it should "be able to lookup variables from parent scopes" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = RenamerScope.empty
        previous.set("x", intVar)
        val current = previous.newScope
        current.lookup("x") should be (Some(intVar))
    }

    it should "not allow the same variable to be declared twice in the same scope" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = RenamerScope.empty
        val current1 = previous.newScope
        current1.set("x", intVar)
        val current2 = previous.newScope
        current2.set("x", intVar)
        current1.lookup("x") should be (Some(intVar))
        current2.lookup("x") should be (Some(intVar))
    }

    it should "allow shadowing of variables from parent scopes" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = RenamerScope.empty
        previous.set("x", intVar)
        val current = previous.newScope
        val intVar2: SemanticData.Var = SemanticData.Var((0,0), 1, KnownType.Int)
        current.set("x", intVar2)
        current.lookup("x") should be (Some(intVar2))
    }
}
