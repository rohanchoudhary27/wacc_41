type Stmts = List[Stmt]
type Args = List[Expr]

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

case class Program(funcs: List[Func], stmt: Stmt)
case class Func(typ: Type, ident: Ident, params: List[Param], stmt: Stmt)
case class Param(typ: Type, ident: Ident)

enum Stmt {
    case Skip
    case Decl(typ: Type, ident: Ident, rval: RValue)
    case Asgn(lval: LValue, rval: RValue)
    case Read(lval: LValue)
    case Free(expr: Expr)
    case Return(expr: Expr)
    case Exit(expr: Expr)
    case Print(expr: Expr)
    case Println(expr: Expr)
    case If(cond: Expr, ifStmts: Stmts, elseStmts: Stmts)
    case While(cond: Expr, stmts: Stmts)
    case Scope(stmts: Stmts)
}
