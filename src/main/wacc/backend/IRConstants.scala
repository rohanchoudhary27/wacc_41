package wacc.backend

import wacc.frontend.KnownType

/* Size in bytes for registers/memory operations */
enum Size {
    case B, W, X
}

object Size {

    /* Get the numerical value of the `Size` class */
    def bytes(s: Size): Int = s match {
        case `B` => 1
        case `W` => 4
        case `X` => 8
    }

    /* Get the numerical value of the `Size` class */
    def fromType(typ: KnownType): Size = typ match {
        case KnownType.Char | KnownType.Bool => Size.B
        case KnownType.Int => Size.W
        case KnownType.String | _: KnownType.Array | _: KnownType.Pair => Size.X
    }
}

object ShiftAmounts {
  opaque type ShiftAmount = Int
  val _0: ShiftAmount = 0
  val _16: ShiftAmount = 16
  val _32: ShiftAmount = 32 
  val _48: ShiftAmount = 48

  def value(shift: ShiftAmount): Int = shift
}

/* Conditions */
enum Condition {
    case EQ, NE, LT, LE, GT, GE
}

/* Positions in a pair */
enum PairPos {
    case Fst
    case Snd
}
