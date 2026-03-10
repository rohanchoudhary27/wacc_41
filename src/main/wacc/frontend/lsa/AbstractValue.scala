package wacc.frontend.lsa

import wacc.frontend.SemanticType

/**
  * Defines the abstract values used by the static analyser to represent
  * compile time knowledge about program variables. Each value lives in a
  * tree like structure where `Unknown` is the top element
  * and the concrete values are at the bottom.
  *
  * This is the core abstraction of the static analyser. Each variable in
  * the program maps to one of these abstract values at every program point.
  */
sealed trait AbstractValue {

    /** Merge this value with `other` at a control flow join.
      *
      * The result must be sound and it must over approximate both inputs for safety.
      * If both sides agree on the same value, preserve it, otherwise widen to `Unknown`
      *
      * @param other The abstract value from the other branch
      * @return The least upper bound of `this` and `other`
      */
    def join(other: AbstractValue): AbstractValue = (this, other) match {
        case (a, b) if a == b => a
        case (Unknown, _) | (_, Unknown) => Unknown
        
        /* converting to a range is useful when it comes to length of arrays, e.g. if we
            know that the length is 10, and the range from the branches is (2, 5), i.e.
                ```
                ...
                if b then
                    x = 2
                else
                    x = 5
                fi
                ...
                int[] arr = [1, 2, 3, ..., 10]
                print arr[x]
                ```
            then we can deduce that x is always less than its length so no need to emit a length
            check on the index
            */
        case (IntConstant(a), IntConstant(b)) => 
            IntRange(math.min(a, b), math.max(a, b))
        case (IntConstant(v), IntRange(lo, hi)) =>
            IntRange(math.min(v, lo), math.max(v, hi))
        case (IntRange(lo, hi), IntConstant(v)) =>
            IntRange(math.min(v, lo), math.max(v, hi))
        case (IntRange(lo1, hi1), IntRange(lo2, hi2)) =>
            IntRange(math.min(lo1, lo2), math.max(hi1, hi2))

        /* Nullity values */
        case (Null, NonNull) | (NonNull, Null)  => Unknown

        /* Freed pointers: joining freed with any pointer state is unsafe,
           so we widen to Unknown */
        case (Freed, _: PointerValue) | (_: PointerValue, Freed) => Unknown
        case (Freed, _) | (_, Freed) => Unknown

        /* Array info: merge lengths and nullity separately */
        case (ArrayInfo(len1), ArrayInfo(len2)) =>
            val mergedLen = (len1, len2) match {
                case (Some(a), Some(b)) if a == b => Some(a)
                case _                            => None
            }
            ArrayInfo(mergedLen)

        /* Pair info: if both agree on inner types keep them
           if not widen to NonNull (still non null but lose type info) */
        case (PairInfo(fst1, snd1), PairInfo(fst2, snd2)) =>
            if (fst1 == fst2 && snd1 == snd2) PairInfo(fst1, snd1)
            else NonNull

        case (PairInfo(_, _), NonNull) | (NonNull, PairInfo(_, _)) => NonNull

        case (PairInfo(_, _), Null) | (Null, PairInfo(_, _)) => Unknown


        /* Cross-domain disagreements widen to Unknown */
        case _ => Unknown
    }

    /** description for diagnostic messages */
    def describe: String
}

/** Abstract values specifically about pointer nullity. */
sealed trait PointerValue extends AbstractValue


/** No compile time information is known about this value.
  *
  * Joining any value with `Unknown` yields `Unknown`. Variables
  * start here when we have no information (e.g. values read from stdin).
  */
case object Unknown extends AbstractValue {
    override def describe: String = "unknown"
}

/** The value is known to be exactly `value` at this program point
  *
  * @param value The concrete integer value
  */
case class IntConstant(value: Int) extends AbstractValue {
    override def describe: String = s"int constant $value"
}

/** The value is known to lie within the interval `[lo, hi]`.
  *
  * This provides more info than `Unknown` for cases like loop
  * bounds or array index checks, while handling the case
  * where two branches assign different constants.
  *
  * @param lo The lower bound
  * @param hi The upper bound
  */
case class IntRange(lo: Int, hi: Int) extends AbstractValue {
    require(lo <= hi, s"IntRange lower bound ($lo) must not exceed upper bound ($hi)")
    override def describe: String = s"int in range [$lo, $hi]"

    /** Check whether a concrete int value is within this range */
    def contains(value: Int): Boolean = value >= lo && value <= hi

    /** Check whether this range is entirely non negative */
    def isNonNegative: Boolean = lo >= 0
}

/** The value is known to be exactly `value` at this program point.
  *
  * @param value The concrete boolean value
  */
case class BoolConstant(value: Boolean) extends AbstractValue {
    override def describe: String = s"bool constant ${if value then "true" else "false"}"
}

/** The value is definitely the null pointer
  *
  * Any dereference (`fst`, `snd`) of a `Null` value is a guaranteed
  * runtime error that we can report at compile time.
  */
case object Null extends PointerValue {
    override def describe: String = "null"
}

/** The value is definitely a valid (non-null) pointer.
  *
  * Dereferences of `NonNull` values are safe, so the
  * runtime null check can be eliminated.
  */
case object NonNull extends PointerValue {
    override def describe: String = "non-null"
}

/** The value has been freed and any access is an error */
case object Freed extends PointerValue {
    override def describe: String = "freed"
}

/** Tracks the known length of an array reference.
  *
  * @param knownLength `Some(n)` if the array was created from a literal of `n` elements
  *                     or its length is otherwise statically determinable, `None` if unknown
  */
case class ArrayInfo(
    knownLength: Option[Int]
) extends AbstractValue {
    override def describe: String = {
        val lenStr = knownLength.map(n => s"length $n").getOrElse("unknown length")
        s"$lenStr"
    }
}

/** Tracks the actual types stored in a pair's fst and snd slots.
  *
  * Created when we see `newpair(e1, e2)`. The types of e1 and e2
  * are recorded so we can detect unsound coercions when the pair is
  * later accessed.
  * 
  * @param fstType The actual `SemanticType` stored in the fst slot
  * @param sndType The actual `SemanticType` stored in the snd slot
  */
case class PairInfo(
    fstType: SemanticType,
    sndType: SemanticType
) extends AbstractValue {
    override def describe: String =
        s"pair(${fstType.outputFormat}, ${sndType.outputFormat}), non-null"
}

object AbstractValue {

    private val ASCII_MIN = 0
    private val ASCII_MAX = 127

    /** no information known */
    val top: AbstractValue = Unknown

    /** Create the initial abstract value for an integer literal */
    def fromInt(n: Int): IntConstant = IntConstant(n)

    /** Create the initial abstract value for a boolean literal */
    def fromBool(b: Boolean): BoolConstant = BoolConstant(b)

    /** Create the initial abstract value for an array literal of known size.
      *
      * @param length The number of elements in the array literal
      */
    def fromArrayLiteral(length: Int): ArrayInfo = ArrayInfo(knownLength = Some(length))

    /** `newpair` always produces a valid, non null pointer. 
      *
      * @param fstType The semantic type of the first element
      * @param sndType The semantic type of the second element
      */
    def fromNewPair(fstType: SemanticType, sndType: SemanticType): AbstractValue =
        PairInfo(fstType, sndType)

    /** Create the initial abstract value for a `newpair` allocation.
      *
      * `newpair` always produces a valid, non-null pointer.
      */
    val freshPair: AbstractValue = NonNull

    /** Create the initial abstract value for a `null` literal */
    val nullValue: AbstractValue = Null

    /** Check whether an abstract value is definitely zero */
    def isDefinitelyZero(av: AbstractValue): Boolean = av match {
        case IntConstant(0) => true
        case IntRange(lo, hi) if lo == 0 && hi == 0 => true
        case _ => false
    }

    /** Check whether an abstract value is definitely non-zero */
    def isDefinitelyNonZero(av: AbstractValue): Boolean = av match {
        case IntConstant(v) => v != 0
        case IntRange(lo, hi)  => lo > 0 || hi < 0
        case _ => false
    }

    /** Check whether an abstract value represents a definite null */
    def isDefinitelyNull(av: AbstractValue): Boolean = av match {
        case Null  => true
        case _ => false
    }

    /** Check whether an abstract value represents a definite non-null */
    def isDefinitelyNonNull(av: AbstractValue): Boolean = av match {
        case NonNull => true
        case ArrayInfo(_) => true
        case _: PairInfo => true
        case _  => false
    }

    /** Check whether an index is provably within bounds `[0, length)` */
    def isProvablyInBounds(indexVal: AbstractValue, length: Int): Boolean =
        indexVal match {
            case IntConstant(i)   => i >= 0 && i < length
            case IntRange(lo, hi) => lo >= 0 && hi < length
            case _                => false
        }

    /** Check whether an index is provably outside bounds `[0, length)`.
      *
      * For `IntRange`, the entire range must be outside bounds for this to return true
      */
    def isProvablyOutOfBounds(indexVal: AbstractValue, length: Int): Boolean =
        indexVal match {
            case IntConstant(i)   => i < 0 || i >= length
            case IntRange(lo, hi) => hi < 0 || lo >= length
            case _                => false
        }

    /** Check whether the argument for `chr` is provably inside the ASCII range */
    def isProvablyValidChr(av: AbstractValue): Boolean = av match
        case IntConstant(v) => v >= ASCII_MIN && v <= ASCII_MAX
        case IntRange(lo, hi) => lo >= ASCII_MIN && hi <= ASCII_MAX
        case _ => false

    /** Check whether the argument for `chr` is provably outside the ASCII range */
    def isProvablyInvalidChr(av: AbstractValue): Boolean = av match
        case IntConstant(v) => v < ASCII_MIN || v > ASCII_MAX
        case IntRange(lo, hi) => hi < ASCII_MIN || lo > ASCII_MAX
        case _ => false

    /** WACC uses signed 32 bit integers */
    private val IntMin: Long = Int.MinValue.toLong
    private val IntMax: Long = Int.MaxValue.toLong

    /** Attempt to wrap a Long result back to a 32 bit IntConstant.
      *
      * Returns `Unknown` if the result overflows the 32 bit signed range,
      * since at runtime this would trigger an overflow error.
      */
    private def safeIntResult(result: Long): AbstractValue =
        if (!wouldOverflow(result)) IntConstant(result.toInt)
        else Unknown

    /** Check if the result from arithmetic expression overflows or underflows */
    def wouldOverflow(result: Long): Boolean = result < IntMin || result > IntMax

    /** Check whether an addition of two abstract values is provably safe
      * (no overflow possible) */
    def isProvablySafeAdd(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(a), IntConstant(b)) => !wouldOverflow(a.toLong + b.toLong)
            case (IntConstant(0), _) | (_, IntConstant(0)) => true
            case (IntRange(lo1, hi1), IntRange(lo2, hi2)) => /* extremes */
                !wouldOverflow(lo1.toLong + lo2.toLong) && !wouldOverflow(hi1.toLong + hi2.toLong)
            case (IntConstant(a), IntRange(lo, hi)) =>
                !wouldOverflow(a.toLong + lo.toLong) && !wouldOverflow(a.toLong + hi.toLong)
            case (IntRange(lo, hi), IntConstant(b)) =>
                !wouldOverflow(lo.toLong + b.toLong) && !wouldOverflow(hi.toLong + b.toLong)
            case _ => false
        }

    /** Check whether a subtraction of two abstract values is provably safe */
    def isProvablySafeSub(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(a), IntConstant(b)) => !wouldOverflow(a.toLong - b.toLong)
            case (_, IntConstant(0)) => true
            case (IntRange(lo1, hi1), IntRange(lo2, hi2)) =>
                !wouldOverflow(lo1.toLong - hi2.toLong) && !wouldOverflow(hi1.toLong - lo2.toLong)
            case (IntConstant(a), IntRange(lo, hi)) =>
                !wouldOverflow(a.toLong - hi.toLong) && !wouldOverflow(a.toLong - lo.toLong)
            case (IntRange(lo, hi), IntConstant(b)) =>
                !wouldOverflow(lo.toLong - b.toLong) && !wouldOverflow(hi.toLong - b.toLong)
            case _ => false
        }

    /** Check whether a multiplication of two abstract values is provably safe */
    def isProvablySafeMul(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(-1), IntConstant(v)) => !wouldOverflow(-v.toLong)
            case (IntConstant(v), IntConstant(-1)) => !wouldOverflow(-v.toLong)
            case (IntConstant(1), _) | (_, IntConstant(1)) => true
            case (IntConstant(0), _) | (_, IntConstant(0)) => true
            case (IntConstant(a), IntConstant(b)) => !wouldOverflow(a.toLong * b.toLong)
            case _ => false
        }

    /** Check whether a negation is provably safe (no overflow) */
    def isProvablySafeNeg(operand: AbstractValue): Boolean = operand match {
        case IntConstant(v) => !wouldOverflow(-v.toLong)
        case IntRange(lo, hi) =>
            !wouldOverflow(-lo.toLong) && !wouldOverflow(-hi.toLong)
        case _ => false
    }

    /** Check whether an addition provably overflows for possible values */
    def doesProvablyOverflowAdd(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(a), IntConstant(b)) => wouldOverflow(a.toLong + b.toLong)
            case _ => false
        }

    /** Check whether a subtraction provably overflows for possible values */
    def doesProvablyOverflowSub(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(a), IntConstant(b)) => wouldOverflow(a.toLong - b.toLong)
            case _ => false
        }

    /** Check whether a multiplication provably overflows for possible values */
    def doesProvablyOverflowMul(left: AbstractValue, right: AbstractValue): Boolean =
        (left, right) match {
            case (IntConstant(a), IntConstant(b)) => wouldOverflow(a.toLong * b.toLong)
            case _ => false
        }

    /** Check whether a negation provably overflows */
    def doesProvablyOverflowNeg(operand: AbstractValue): Boolean = operand match {
        case IntConstant(v) => wouldOverflow(-v.toLong)
        case _ => false
    }


    def add(l: AbstractValue, r: AbstractValue) = intBinaryOp(l, r, _ + _)
    def sub(l: AbstractValue, r: AbstractValue) = intBinaryOp(l, r, _ - _)

    def mul(left: AbstractValue, right: AbstractValue): AbstractValue =
        (left, right) match {
            /* anything multiplied by 0 is just 0, we don't care if the other side is unkown */
            case (IntConstant(0), _) | (_, IntConstant(0)) => IntConstant(0)
            case _ => intBinaryOp(left, right, _ * _)
        }

    def div(l: AbstractValue, r: AbstractValue) = checkedDivOp(l, r, _ / _)
    def mod(l: AbstractValue, r: AbstractValue) = checkedDivOp(l, r, _ % _)

    def neg(operand: AbstractValue): AbstractValue = operand match {
        case IntConstant(v) => safeIntResult(-v.toLong)
        case _ => Unknown
    }

    def not(operand: AbstractValue): AbstractValue = operand match {
        case BoolConstant(v) => BoolConstant(!v)
        case _ => Unknown
    }

    def ord(operand: AbstractValue): AbstractValue = operand match {
        case c: IntConstant => c
        case _ => Unknown
    }

    def chr(operand: AbstractValue): AbstractValue = operand match {
        case c: IntConstant => c
        case _  => Unknown
    }

    def len(operand: AbstractValue): AbstractValue = operand match {
        case ArrayInfo(Some(n)) => IntConstant(n)
        case _ => Unknown
    }

    /** Abstract greater-than */
    def gt(left: AbstractValue, right: AbstractValue): AbstractValue =
        compareInts(left, right, _ > _)

    /** Abstract greater-than-or-equal */
    def gte(left: AbstractValue, right: AbstractValue): AbstractValue =
        compareInts(left, right, _ >= _)

    /** Abstract less-than */
    def lt(left: AbstractValue, right: AbstractValue): AbstractValue =
        compareInts(left, right, _ < _)

    /** Abstract less-than-or-equal */
    def lte(left: AbstractValue, right: AbstractValue): AbstractValue =
        compareInts(left, right, _ <= _)

    def eq(left: AbstractValue, right: AbstractValue): AbstractValue =
        (left, right) match {
            case (IntConstant(a), IntConstant(b))   => BoolConstant(a == b)
            case (BoolConstant(a), BoolConstant(b)) => BoolConstant(a == b)
            case (Null, Null)                       => BoolConstant(true)
            case (Null, NonNull) | (NonNull, Null)  => BoolConstant(false)
            case _                                  => Unknown
        }

    def neq(left: AbstractValue, right: AbstractValue): AbstractValue =
        eq(left, right) match {
            case BoolConstant(v) => BoolConstant(!v)
            case other           => other
        }

    def and(left: AbstractValue, right: AbstractValue): AbstractValue =
        (left, right) match {
            case (BoolConstant(a), BoolConstant(b)) => BoolConstant(a && b)
            case (BoolConstant(false), _) | (_, BoolConstant(false)) => BoolConstant(false)
            case _ => Unknown
        }

    def or(left: AbstractValue, right: AbstractValue): AbstractValue =
        (left, right) match {
            case (BoolConstant(a), BoolConstant(b)) => BoolConstant(a || b)
            case (BoolConstant(true), _) | (_, BoolConstant(true)) => BoolConstant(true)
            case _ => Unknown
        }

    /** Helper for integer binary operations */
    private def intBinaryOp(
        left: AbstractValue,
        right: AbstractValue,
        op: (Long, Long) => Long
    ): AbstractValue = (left, right) match {
        case (IntConstant(a), IntConstant(b)) => safeIntResult(op(a.toLong, b.toLong))
        case _                                => Unknown
    }

    private def checkedDivOp(
        left: AbstractValue,
        right: AbstractValue,
        op: (Long, Long) => Long
    ): Option[AbstractValue] =
        if (isDefinitelyZero(right)) None
        else Some(intBinaryOp(left, right, op))

    /** Helper for integer comparison operations */
    private def compareInts(
        left: AbstractValue,
        right: AbstractValue,
        op: (Int, Int) => Boolean
    ): AbstractValue = (left, right) match {
        case (IntConstant(a), IntConstant(b)) => BoolConstant(op(a, b))
        case _                                => Unknown
    }
}
