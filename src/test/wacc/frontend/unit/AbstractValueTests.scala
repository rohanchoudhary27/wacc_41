package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.frontend.*
import wacc.frontend.lsa.*

class AbstractValueTests extends AnyFlatSpec with Matchers {

    "AbstractValue join" should "preserve IntConstant when both sides agree" in {
        IntConstant(5).join(IntConstant(5)) shouldBe IntConstant(5)
    }

    it should "preserve BoolConstant when both sides agree" in {
        BoolConstant(true).join(BoolConstant(true)) shouldBe BoolConstant(true)
    }

    it should "preserve Null when both sides agree" in {
        wacc.frontend.lsa.Null.join(wacc.frontend.lsa.Null) shouldBe wacc.frontend.lsa.Null
    }

    it should "preserve NonNull when both sides agree" in {
        NonNull.join(NonNull) shouldBe NonNull
    }

    it should "preserve Freed when both sides agree" in {
        Freed.join(Freed) shouldBe Freed
    }

    it should "preserve Unknown when both sides agree" in {
        Unknown.join(Unknown) shouldBe Unknown
    }

    it should "widen to Unknown when joining IntConstant with Unknown" in {
        IntConstant(5).join(Unknown) shouldBe Unknown
        Unknown.join(IntConstant(5)) shouldBe Unknown
    }

    it should "widen to Unknown when joining BoolConstant with Unknown" in {
        BoolConstant(true).join(Unknown) shouldBe Unknown
        Unknown.join(BoolConstant(false)) shouldBe Unknown
    }

    it should "widen to Unknown when joining Null with Unknown" in {
        wacc.frontend.lsa.Null.join(Unknown) shouldBe Unknown
        Unknown.join(wacc.frontend.lsa.Null) shouldBe Unknown
    }

    it should "widen to Unknown when joining NonNull with Unknown" in {
        NonNull.join(Unknown) shouldBe Unknown
        Unknown.join(NonNull) shouldBe Unknown
    }

    it should "widen two different IntConstants to an IntRange" in {
        IntConstant(3).join(IntConstant(7)) shouldBe IntRange(3, 7)
    }

    it should "order the range correctly regardless of join order" in {
        IntConstant(10).join(IntConstant(2)) shouldBe IntRange(2, 10)
    }

    it should "handle negative constants in range creation" in {
        IntConstant(-5).join(IntConstant(5)) shouldBe IntRange(-5, 5)
    }

    it should "absorb an IntConstant into an existing IntRange" in {
        IntRange(2, 8).join(IntConstant(5)) shouldBe IntRange(2, 8)
    }

    it should "expand an IntRange when constant falls outside" in {
        IntRange(2, 8).join(IntConstant(10)) shouldBe IntRange(2, 10)
        IntConstant(-1).join(IntRange(2, 8)) shouldBe IntRange(-1, 8)
    }

    it should "merge two IntRanges to span both" in {
        IntRange(1, 5).join(IntRange(3, 10)) shouldBe IntRange(1, 10)
    }

    it should "preserve an IntRange that already covers the other" in {
        IntRange(1, 10).join(IntRange(3, 7)) shouldBe IntRange(1, 10)
    }

    it should "widen disagreeing BoolConstants to Unknown" in {
        BoolConstant(true).join(BoolConstant(false)) shouldBe Unknown
    }

    it should "widen Null and NonNull to Unknown" in {
        wacc.frontend.lsa.Null.join(NonNull) shouldBe Unknown
        NonNull.join(wacc.frontend.lsa.Null) shouldBe Unknown
    }

    it should "widen Freed with any pointer value to Unknown" in {
        Freed.join(wacc.frontend.lsa.Null) shouldBe Unknown
        Freed.join(NonNull) shouldBe Unknown
        wacc.frontend.lsa.Null.join(Freed) shouldBe Unknown
        NonNull.join(Freed) shouldBe Unknown
    }

    it should "preserve ArrayInfo when both sides agree on length and nullity" in {
        val arr = ArrayInfo(Some(3), NonNull)
        arr.join(arr) shouldBe arr
    }

    it should "lose length when two ArrayInfos disagree on length" in {
        val arr1 = ArrayInfo(Some(3), NonNull)
        val arr2 = ArrayInfo(Some(5), NonNull)
        arr1.join(arr2) shouldBe ArrayInfo(None, NonNull)
    }

    it should "widen to Unknown when two ArrayInfos disagree on nullity" in {
        val arr1 = ArrayInfo(Some(3), NonNull)
        val arr2 = ArrayInfo(Some(3), wacc.frontend.lsa.Null)
        arr1.join(arr2) shouldBe Unknown
    }

    it should "widen to Unknown when both length and nullity disagree" in {
        val arr1 = ArrayInfo(Some(3), NonNull)
        val arr2 = ArrayInfo(Some(5), wacc.frontend.lsa.Null)
        arr1.join(arr2) shouldBe Unknown
    }

    it should "preserve unknown length when both have unknown length" in {
        val arr1 = ArrayInfo(None, NonNull)
        val arr2 = ArrayInfo(None, NonNull)
        arr1.join(arr2) shouldBe ArrayInfo(None, NonNull)
    }

    it should "widen to Unknown when joining int with bool" in {
        IntConstant(1).join(BoolConstant(true)) shouldBe Unknown
    }

    it should "widen to Unknown when joining int with pointer" in {
        IntConstant(0).join(wacc.frontend.lsa.Null) shouldBe Unknown
    }

    it should "widen to Unknown when joining bool with pointer" in {
        BoolConstant(false).join(NonNull) shouldBe Unknown
    }

    "IntRange" should "reject inverted bounds" in {
        an [IllegalArgumentException] should be thrownBy IntRange(10, 5)
    }

    it should "allow single-element ranges" in {
        val range = IntRange(3, 3)
        range.contains(3) shouldBe true
        range.contains(2) shouldBe false
    }

    it should "correctly report non-negative ranges" in {
        IntRange(0, 10).isNonNegative shouldBe true
        IntRange(1, 100).isNonNegative shouldBe true
        IntRange(-1, 10).isNonNegative shouldBe false
    }

    "isDefinitelyZero" should "return true for IntConstant(0)" in {
        AbstractValue.isDefinitelyZero(IntConstant(0)) shouldBe true
    }

    it should "return false for IntConstant(5)" in {
        AbstractValue.isDefinitelyZero(IntConstant(5)) shouldBe false
    }

    it should "return true for IntRange(0, 0)" in {
        AbstractValue.isDefinitelyZero(IntRange(0, 0)) shouldBe true
    }

    it should "return false for IntRange(0, 5)" in {
        AbstractValue.isDefinitelyZero(IntRange(0, 5)) shouldBe false
    }

    it should "return false for Unknown" in {
        AbstractValue.isDefinitelyZero(Unknown) shouldBe false
    }

    "isDefinitelyNonZero" should "return true for IntConstant(5)" in {
        AbstractValue.isDefinitelyNonZero(IntConstant(5)) shouldBe true
    }

    it should "return false for IntConstant(0)" in {
        AbstractValue.isDefinitelyNonZero(IntConstant(0)) shouldBe false
    }

    it should "return true for IntRange(1, 10)" in {
        AbstractValue.isDefinitelyNonZero(IntRange(1, 10)) shouldBe true
    }

    it should "return true for IntRange(-10, -1)" in {
        AbstractValue.isDefinitelyNonZero(IntRange(-10, -1)) shouldBe true
    }

    it should "return false for IntRange(-5, 5) (contains zero)" in {
        AbstractValue.isDefinitelyNonZero(IntRange(-5, 5)) shouldBe false
    }

    "isDefinitelyNull" should "return true for Null" in {
        AbstractValue.isDefinitelyNull(wacc.frontend.lsa.Null) shouldBe true
    }

    it should "return true for ArrayInfo with Null nullity" in {
        AbstractValue.isDefinitelyNull(ArrayInfo(Some(3), wacc.frontend.lsa.Null)) shouldBe true
    }

    it should "return false for NonNull" in {
        AbstractValue.isDefinitelyNull(NonNull) shouldBe false
    }

    it should "return false for Unknown" in {
        AbstractValue.isDefinitelyNull(Unknown) shouldBe false
    }

    "isDefinitelyNonNull" should "return true for NonNull" in {
        AbstractValue.isDefinitelyNonNull(NonNull) shouldBe true
    }

    it should "return true for ArrayInfo with NonNull nullity" in {
        AbstractValue.isDefinitelyNonNull(ArrayInfo(Some(3), NonNull)) shouldBe true
    }

    it should "return false for Null" in {
        AbstractValue.isDefinitelyNonNull(wacc.frontend.lsa.Null) shouldBe false
    }

    it should "return false for Unknown" in {
        AbstractValue.isDefinitelyNonNull(Unknown) shouldBe false
    }
}