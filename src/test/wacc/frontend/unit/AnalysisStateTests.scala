
package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend.lsa.*

class AnalysisStateTests extends AnyFlatSpec with Matchers {

    /* Helper variable ids for testing */
    private val x = VariableId("x", 0)
    private val y = VariableId("y", 1)
    private val z = VariableId("z", 2)

    "AnalysisState lookup" should "return Unknown for untracked variables" in {
        val state = AnalysisState.empty
        state.lookup(x) shouldBe Unknown
    }

    it should "return the stored value for tracked variables" in {
        val state = AnalysisState.empty.update(x, IntConstant(5))
        state.lookup(x) shouldBe IntConstant(5)
    }

    it should "return Unknown for a different variable than the one stored" in {
        val state = AnalysisState.empty.update(x, IntConstant(5))
        state.lookup(y) shouldBe Unknown
    }

    "AnalysisState update" should "add a new variable binding" in {
        val state = AnalysisState.empty.update(x, IntConstant(42))
        state.lookup(x) shouldBe IntConstant(42)
    }

    it should "overwrite an existing binding" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(x, IntConstant(10))
        state.lookup(x) shouldBe IntConstant(10)
    }

    it should "not affect other variables when updating one" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, BoolConstant(true))
        state.lookup(x) shouldBe IntConstant(5)
        state.lookup(y) shouldBe BoolConstant(true)
    }

    it should "be immutable - original state is not modified" in {
        val original = AnalysisState.empty.update(x, IntConstant(5))
        val updated = original.update(x, IntConstant(10))

        original.lookup(x) shouldBe IntConstant(5)
        updated.lookup(x) shouldBe IntConstant(10)
    }

    "AnalysisState invalidate" should "set a variable to Unknown" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .invalidate(x)
        state.lookup(x) shouldBe Unknown
    }

    "AnalysisState invalidateAll" should "invalidate multiple variables at once" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, BoolConstant(true))
            .update(z, NonNull)
            .invalidateAll(Set(x, z))
        state.lookup(x) shouldBe Unknown
        state.lookup(y) shouldBe BoolConstant(true)
        state.lookup(z) shouldBe Unknown
    }

    "AnalysisState join" should "preserve values that agree across both branches" in {
        val left = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, NonNull)
        val right = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, NonNull)

        val joined = left.join(right)
        joined.lookup(x) shouldBe IntConstant(5)
        joined.lookup(y) shouldBe NonNull
    }

    it should "widen disagreeing values" in {
        val left = AnalysisState.empty.update(x, IntConstant(3))
        val right = AnalysisState.empty.update(x, IntConstant(7))

        val joined = left.join(right)
        joined.lookup(x) shouldBe IntRange(3, 7)
    }

    it should "widen to Unknown when variable is only in one branch" in {
        val left = AnalysisState.empty.update(x, IntConstant(5))
        val right = AnalysisState.empty

        val joined = left.join(right)
        joined.lookup(x) shouldBe Unknown
    }

    it should "handle asymmetric variable sets" in {
        val left = AnalysisState.empty
            .update(x, IntConstant(5))
        val right = AnalysisState.empty
            .update(y, BoolConstant(true))

        val joined = left.join(right)
        joined.lookup(x) shouldBe Unknown
        joined.lookup(y) shouldBe Unknown
    }    

    "AnalysisState restrictTo" should "remove variables not in the keep set" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, BoolConstant(true))
            .update(z, NonNull)

        val restricted = state.restrictTo(Set(x, z))
        restricted.lookup(x) shouldBe IntConstant(5)
        restricted.lookup(y) shouldBe Unknown
        restricted.lookup(z) shouldBe NonNull
    }

    it should "handle empty keep set by clearing everything" in {
        val state = AnalysisState.empty.update(x, IntConstant(5))
        val restricted = state.restrictTo(Set.empty)
        restricted.lookup(x) shouldBe Unknown
    }

    it should "handle keep set larger than tracked variables" in {
        val state = AnalysisState.empty.update(x, IntConstant(5))
        val restricted = state.restrictTo(Set(x, y, z))
        restricted.lookup(x) shouldBe IntConstant(5)
    }

    "AnalysisState trackedVariables" should "return empty set for empty state" in {
        AnalysisState.empty.trackedVariables shouldBe Set.empty
    }

    it should "return all updated variable ids" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .update(y, BoolConstant(true))
        state.trackedVariables shouldBe Set(x, y)
    }

    it should "include invalidated variables" in {
        val state = AnalysisState.empty
            .update(x, IntConstant(5))
            .invalidate(x)
        state.trackedVariables should contain(x)
    }
}