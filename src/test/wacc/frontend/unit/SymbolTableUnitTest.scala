package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.{SymbolTable}
import wacc.SemanticData.Var
import wacc.SemanticType

class SymbolTableSpec extends AnyFlatSpec with Matchers {

    val pos = (0, 0)
    val intVar: Var  = Var(pos, 0, SemanticType.Int)
    val boolVar: Var = Var(pos, 0, SemanticType.Bool)

    "A SymbolTable" should "store and retrieve a symbol in the current scope" in {
        val table  = new SymbolTable(None)

        table.set("x", intVar)
        table.get("x") should be (Some(intVar))
    }

    it should "return none when looking up a symbol that doesn't exist" in {
        val table  = new SymbolTable(None)
        table.get("x") should be (None)
    }

    it should "find a variable defined in a previous scope" in {
        val previous = new SymbolTable(None)
        previous.set("x", intVar)
        val current = new SymbolTable(Some(previous))
        current.set("y", intVar)
        current.get("x") should be (Some(intVar))
    }

    it should "not see variables defined with same previous scope" in {
        val previous = new SymbolTable(None)
        val current1 = new SymbolTable(Some(previous))
        current1.set("x", intVar)
        val current2 = new SymbolTable(Some(previous))
        current2.set("y", intVar)
        current1.get("y") should be (None)
        current2.get("x") should be (None)
    }

    it should "prevent variables being are declared in the same scope and type should remain the same" in {
        val previous = new SymbolTable(None)
        previous.set("x", intVar)
        val result = previous.set("x", boolVar)
        result should be (false)
        previous.get("x") should be (Some(intVar))
    }
}
