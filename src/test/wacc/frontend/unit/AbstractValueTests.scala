package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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

    "AbstractValue.add" should "compute known result for two IntConstants" in {
        AbstractValue.add(IntConstant(3), IntConstant(5)) shouldBe IntConstant(8)
    }

    it should "handle negative constants" in {
        AbstractValue.add(IntConstant(-3), IntConstant(5)) shouldBe IntConstant(2)
    }

    it should "return Unknown when either operand is Unknown" in {
        AbstractValue.add(IntConstant(3), Unknown) shouldBe Unknown
        AbstractValue.add(Unknown, IntConstant(5)) shouldBe Unknown
    }

    it should "return Unknown on 32-bit overflow" in {
        AbstractValue.add(IntConstant(Int.MaxValue), IntConstant(1)) shouldBe Unknown
    }

    it should "return Unknown on 32-bit underflow" in {
        AbstractValue.add(IntConstant(Int.MinValue), IntConstant(-1)) shouldBe Unknown
    }

    "AbstractValue.sub" should "compute known result for two IntConstants" in {
        AbstractValue.sub(IntConstant(10), IntConstant(3)) shouldBe IntConstant(7)
    }

    it should "handle subtraction yielding negative" in {
        AbstractValue.sub(IntConstant(3), IntConstant(10)) shouldBe IntConstant(-7)
    }

    it should "return Unknown when either operand is Unknown" in {
        AbstractValue.sub(IntConstant(3), Unknown) shouldBe Unknown
    }

    it should "return Unknown on overflow" in {
        AbstractValue.sub(IntConstant(Int.MinValue), IntConstant(1)) shouldBe Unknown
    }

    "AbstractValue.mul" should "compute known result for two IntConstants" in {
        AbstractValue.mul(IntConstant(3), IntConstant(5)) shouldBe IntConstant(15)
    }

    it should "return IntConstant(0) when left is zero regardless of right" in {
        AbstractValue.mul(IntConstant(0), Unknown) shouldBe IntConstant(0)
    }

    it should "return IntConstant(0) when right is zero regardless of left" in {
        AbstractValue.mul(Unknown, IntConstant(0)) shouldBe IntConstant(0)
    }

    it should "return Unknown when neither operand is known" in {
        AbstractValue.mul(Unknown, Unknown) shouldBe Unknown
    }

    it should "return Unknown on overflow" in {
        AbstractValue.mul(IntConstant(Int.MaxValue), IntConstant(2)) shouldBe Unknown
    }

    "AbstractValue.div" should "compute known result for two IntConstants" in {
        AbstractValue.div(IntConstant(10), IntConstant(3)) shouldBe Some(IntConstant(3))
    }

    it should "return None when divisor is provably zero (IntConstant)" in {
        AbstractValue.div(IntConstant(10), IntConstant(0)) shouldBe None
    }

    it should "return None when divisor is provably zero (IntRange(0,0))" in {
        AbstractValue.div(IntConstant(10), IntRange(0, 0)) shouldBe None
    }

    it should "return Some(Unknown) when divisor is Unknown" in {
        AbstractValue.div(IntConstant(10), Unknown) shouldBe Some(Unknown)
    }

    it should "return Some(Unknown) when dividend is Unknown" in {
        AbstractValue.div(Unknown, IntConstant(5)) shouldBe Some(Unknown)
    }

    it should "handle negative division" in {
        AbstractValue.div(IntConstant(-10), IntConstant(3)) shouldBe Some(IntConstant(-3))
    }

    "AbstractValue.mod" should "compute known result for two IntConstants" in {
        AbstractValue.mod(IntConstant(10), IntConstant(3)) shouldBe Some(IntConstant(1))
    }

    it should "return None when divisor is provably zero" in {
        AbstractValue.mod(IntConstant(10), IntConstant(0)) shouldBe None
    }

    it should "return Some(Unknown) when either operand is Unknown" in {
        AbstractValue.mod(IntConstant(10), Unknown) shouldBe Some(Unknown)
    }

    "AbstractValue.neg" should "negate a known constant" in {
        AbstractValue.neg(IntConstant(5)) shouldBe IntConstant(-5)
    }

    it should "negate a negative constant" in {
        AbstractValue.neg(IntConstant(-3)) shouldBe IntConstant(3)
    }

    it should "return Unknown for Unknown operand" in {
        AbstractValue.neg(Unknown) shouldBe Unknown
    }

    it should "return Unknown on overflow (negating Int.MinValue)" in {
        /* -Int.MinValue overflows 32-bit signed integer */
        AbstractValue.neg(IntConstant(Int.MinValue)) shouldBe Unknown
    }

    "AbstractValue.not" should "negate a known true" in {
        AbstractValue.not(BoolConstant(true)) shouldBe BoolConstant(false)
    }

    it should "negate a known false" in {
        AbstractValue.not(BoolConstant(false)) shouldBe BoolConstant(true)
    }

    it should "return Unknown for Unknown operand" in {
        AbstractValue.not(Unknown) shouldBe Unknown
    }

    "AbstractValue.ord" should "pass through IntConstant" in {
        AbstractValue.ord(IntConstant(97)) shouldBe IntConstant(97)
    }

    it should "return Unknown for Unknown" in {
        AbstractValue.ord(Unknown) shouldBe Unknown
    }

    "AbstractValue.chr" should "pass through IntConstant" in {
        AbstractValue.chr(IntConstant(65)) shouldBe IntConstant(65)
    }

    it should "return Unknown for Unknown" in {
        AbstractValue.chr(Unknown) shouldBe Unknown
    }

    "AbstractValue.len" should "extract known length from ArrayInfo" in {
        AbstractValue.len(ArrayInfo(Some(5), NonNull)) shouldBe IntConstant(5)
    }

    it should "return Unknown for ArrayInfo with unknown length" in {
        AbstractValue.len(ArrayInfo(None, NonNull)) shouldBe Unknown
    }

    it should "return Unknown for non-array values" in {
        AbstractValue.len(Unknown) shouldBe Unknown
    }

    "AbstractValue.gt" should "return BoolConstant for known operands" in {
        AbstractValue.gt(IntConstant(5), IntConstant(3)) shouldBe BoolConstant(true)
        AbstractValue.gt(IntConstant(3), IntConstant(5)) shouldBe BoolConstant(false)
        AbstractValue.gt(IntConstant(3), IntConstant(3)) shouldBe BoolConstant(false)
    }

    it should "return Unknown for unknown operands" in {
        AbstractValue.gt(IntConstant(5), Unknown) shouldBe Unknown
    }

    "AbstractValue.gte" should "return BoolConstant for known operands" in {
        AbstractValue.gte(IntConstant(5), IntConstant(3)) shouldBe BoolConstant(true)
        AbstractValue.gte(IntConstant(3), IntConstant(3)) shouldBe BoolConstant(true)
        AbstractValue.gte(IntConstant(2), IntConstant(3)) shouldBe BoolConstant(false)
    }

    "AbstractValue.lt" should "return BoolConstant for known operands" in {
        AbstractValue.lt(IntConstant(3), IntConstant(5)) shouldBe BoolConstant(true)
        AbstractValue.lt(IntConstant(5), IntConstant(3)) shouldBe BoolConstant(false)
    }

    "AbstractValue.lte" should "return BoolConstant for known operands" in {
        AbstractValue.lte(IntConstant(3), IntConstant(5)) shouldBe BoolConstant(true)
        AbstractValue.lte(IntConstant(3), IntConstant(3)) shouldBe BoolConstant(true)
        AbstractValue.lte(IntConstant(5), IntConstant(3)) shouldBe BoolConstant(false)
    }

    "AbstractValue.eq" should "return true for equal IntConstants" in {
        AbstractValue.eq(IntConstant(5), IntConstant(5)) shouldBe BoolConstant(true)
    }

    it should "return false for unequal IntConstants" in {
        AbstractValue.eq(IntConstant(3), IntConstant(7)) shouldBe BoolConstant(false)
    }

    it should "return true for equal BoolConstants" in {
        AbstractValue.eq(BoolConstant(true), BoolConstant(true)) shouldBe BoolConstant(true)
    }

    it should "return false for unequal BoolConstants" in {
        AbstractValue.eq(BoolConstant(true), BoolConstant(false)) shouldBe BoolConstant(false)
    }

    it should "return true for Null == Null" in {
        AbstractValue.eq(wacc.frontend.lsa.Null, wacc.frontend.lsa.Null) shouldBe BoolConstant(true)
    }

    it should "return false for Null == NonNull" in {
        AbstractValue.eq(wacc.frontend.lsa.Null, NonNull) shouldBe BoolConstant(false)
        AbstractValue.eq(NonNull, wacc.frontend.lsa.Null) shouldBe BoolConstant(false)
    }

    it should "return Unknown when either is Unknown" in {
        AbstractValue.eq(IntConstant(5), Unknown) shouldBe Unknown
    }

    "AbstractValue.neq" should "return the negation of eq" in {
        AbstractValue.neq(IntConstant(5), IntConstant(5)) shouldBe BoolConstant(false)
        AbstractValue.neq(IntConstant(3), IntConstant(7)) shouldBe BoolConstant(true)
    }

    it should "return Unknown when eq returns Unknown" in {
        AbstractValue.neq(IntConstant(5), Unknown) shouldBe Unknown
    }

    "AbstractValue.and" should "compute known result for two BoolConstants" in {
        AbstractValue.and(BoolConstant(true), BoolConstant(true)) shouldBe BoolConstant(true)
        AbstractValue.and(BoolConstant(true), BoolConstant(false)) shouldBe BoolConstant(false)
        AbstractValue.and(BoolConstant(false), BoolConstant(true)) shouldBe BoolConstant(false)
        AbstractValue.and(BoolConstant(false), BoolConstant(false)) shouldBe BoolConstant(false)
    }

    it should "short-circuit to false when either is known false" in {
        AbstractValue.and(BoolConstant(false), Unknown) shouldBe BoolConstant(false)
        AbstractValue.and(Unknown, BoolConstant(false)) shouldBe BoolConstant(false)
    }

    it should "return Unknown when neither allows short-circuit" in {
        AbstractValue.and(BoolConstant(true), Unknown) shouldBe Unknown
        AbstractValue.and(Unknown, Unknown) shouldBe Unknown
    }

    "AbstractValue.or" should "compute known result for two BoolConstants" in {
        AbstractValue.or(BoolConstant(true), BoolConstant(true)) shouldBe BoolConstant(true)
        AbstractValue.or(BoolConstant(true), BoolConstant(false)) shouldBe BoolConstant(true)
        AbstractValue.or(BoolConstant(false), BoolConstant(true)) shouldBe BoolConstant(true)
        AbstractValue.or(BoolConstant(false), BoolConstant(false)) shouldBe BoolConstant(false)
    }

    it should "short-circuit to true when either is known true" in {
        AbstractValue.or(BoolConstant(true), Unknown) shouldBe BoolConstant(true)
        AbstractValue.or(Unknown, BoolConstant(true)) shouldBe BoolConstant(true)
    }

    it should "return Unknown when neither allows short-circuit" in {
        AbstractValue.or(BoolConstant(false), Unknown) shouldBe Unknown
        AbstractValue.or(Unknown, Unknown) shouldBe Unknown
    }
}