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

// Operator nodes
case class UnaryExpr(op: UnaryOperator, expr: Expr) extends Expr
case class BinaryExpr(op: BinaryOperator, left: Expr, right: Expr) extends Expr

// LValues and reference access
case class Ident(ident: String) extends LValue with Expr
case class ArrayElem(ident: Ident, expr: Expr) extends LValue with Expr
case class PairElem(isFst: Boolean, lval: LValue) extends LValue with RValue

// Complex RValues
case class NewPair(e1: Expr, e2: Expr) extends RValue
case class Call(ident: Ident, argList: Args) extends RValue

// Literal nodes
case class ArrayLiter(e1: Expr, e2: Expr) extends RValue
case class IntLiter(value: Int) extends Expr
case class BoolLiter(value: Boolean) extends Expr
case class CharLiter(value: Char) extends Expr
case class StrLiter(value: String) extends Expr
case object PairLiter extends Expr
