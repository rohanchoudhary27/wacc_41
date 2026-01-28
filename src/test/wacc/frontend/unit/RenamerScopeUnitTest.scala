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
        val scope = new RenamerScope(None)
        scope.set("x", intVar)
        scope.get("x") should be (Some(intVar))
    }

    it should "return None for a variable that is not in the scope" in {
        val scope = new RenamerScope(None)
        scope.get("x") should be (None)
    }

    it should "be able to lookup variables from parent scopes" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = new RenamerScope(None)
        previous.set("x", intVar)
        val current = new RenamerScope(Some(previous))
        previous.get("x") should be (Some(intVar))
    }

    it should "not allow the same variable to be declared twice in the same scope" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = new RenamerScope(None)
        val current1 = new RenamerScope(Some(previous))
        current1.set("x", intVar)
        val current2 = new RenamerScope(Some(previous))
        current2.set("x", intVar)
        current1.get("x") should be (Some(intVar))
        current2.get("x") should be (Some(intVar))
    }

    it should "allow shadowing of variables from parent scopes" in {
        val intVar: SemanticData.Var = SemanticData.Var((0,0), 0, KnownType.Int)
        val previous = new RenamerScope(None)
        previous.set("x", intVar)
        val current = new RenamerScope(Some(previous))
        val intVar2: SemanticData.Var = SemanticData.Var((0,0), 1, KnownType.Int)
        current.set("x", intVar2)
        current.get("x") should be (Some(intVar2))
    }
}
