package wacc.backend

import wacc.frontend.KnownType
import wacc.frontend.SemanticType

/**
  * Size in bytes for registers/memory operations
  */
enum Size {
    case B, W, X
}

/**
  * Companion object of `Size` enum
  */
object Size {

    /* Get the numerical value of the `Size` class */
    def bytes(s: Size): Int = s match {
        case `B` => 1
        case `W` => 4
        case `X` => 8
    }

    /* Get the size of a type */
    def fromType(typ: KnownType): Size = typ match {
        case KnownType.Char | KnownType.Bool => Size.B
        case KnownType.Int => Size.W
        case KnownType.String | _: KnownType.Array | _: KnownType.Pair => Size.X
    }

    /* Get the size of a semantic type */
    def fromSemType(typ: SemanticType): Size = typ match {
      case t: KnownType => fromType(t)
      case wacc.frontend.PairAny | wacc.frontend.Null | wacc.frontend.EmptyArray => Size.X
    }
}

/**
  * The size of a shift in an instruction like MOVK, and in
  * shifted register addressing .
  */
object ShiftAmounts {
  opaque type ShiftAmount = Int
  val _0: ShiftAmount = 0
  val _16: ShiftAmount = 16
  val _32: ShiftAmount = 32 
  val _48: ShiftAmount = 48

  def value(shift: ShiftAmount): Int = shift
}

/**
  * The conditions for branching instructions
  */
enum Condition {
    case EQ, NE, LT, LE, GT, GE, HI
}

/**
  * The positions in a pair
  */
enum PairPos {
    case Fst
    case Snd
}
