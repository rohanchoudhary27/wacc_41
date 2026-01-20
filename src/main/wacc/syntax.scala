// Traits
sealed trait LValue
sealed trait RValue
sealed trait Expr extends RValue

// The WACC type system
enum Type {
    case Int, Bool, Char, String, ErasedPair
    case Pair(fst: Type, snd: Type)
    case Array(arrType: Type)
}

// WACC operators
enum BinaryOperator {
    case Add, Sub, Mul, Div, Mod
    case GT, GTE, LT, LTE, EQ, NEQ
    case And, Or
}
enum UnaryOperator {
    case Neg, Not, Len, Ord, Chr
}

