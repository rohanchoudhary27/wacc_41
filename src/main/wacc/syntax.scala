package wacc

type Args[A] = List[Expr[A]]

// Traits
sealed trait LValue[A] { val info: A }
sealed trait RValue[A] { val info: A }
sealed trait Expr[A] extends RValue[A] { val info: A }

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

case class Program[A](funcs: List[Func[A]], stmt: Stmt[A], info: A)
case class Func[A](typ: Type, ident: Ident[A], params: List[Param[A]], stmt: Stmt[A], info: A)
case class Param[A](typ: Type, ident: Ident[A], info: A)

enum Stmt[A] {
    case Skip(info: A)
    case Decl(typ: Type, ident: Ident[A], rval: RValue[A], info: A)
    case Asgn(lval: LValue[A], rval: RValue[A], info: A)
    case Read(lval: LValue[A], info: A)
    case Free(expr: Expr[A], info: A)
    case Return(expr: Expr[A], info: A)
    case Exit(expr: Expr[A], info: A)
    case Print(expr: Expr[A], info: A)
    case Println(expr: Expr[A], info: A)
    case If(cond: Expr[A], ifBlock: Stmt[A], elseBlock: Stmt[A], info: A)
    case While(cond: Expr[A], stmt: Stmt[A], info: A)
    case Scope(stmt: Stmt[A], info: A)
    case Block(stmts: List[Stmt[A]], info: A)
}

// Operator nodes
case class UnaryExpr[A](op: UnaryOperator, expr: Expr[A], info: A) extends Expr[A]
case class BinaryExpr[A](op: BinaryOperator, left: Expr[A], right: Expr[A], info: A) extends Expr[A]

// LValues and reference access
case class Ident[A](ident: String, info: A) extends LValue[A] with Expr[A]
case class ArrayElem[A](ident: Ident[A], indices: List[Expr[A]], info: A) extends LValue[A] with Expr[A]
case class PairElem[A](isFst: Boolean, lval: LValue[A], info: A) extends LValue[A] with RValue[A]

// Complex RValues
case class NewPair[A](e1: Expr[A], e2: Expr[A], info: A) extends RValue[A]
case class Call[A](ident: Ident[A], argList: Args[A], info: A) extends RValue[A]

// Literal nodes
case class ArrayLiter[A](elements: List[Expr[A]], info: A) extends RValue[A]
case class IntLiter[A](value: Int, info: A) extends Expr[A]
case class BoolLiter[A](value: Boolean, info: A) extends Expr[A]
case class CharLiter[A](value: Char, info: A) extends Expr[A]
case class StrLiter[A](value: String, info: A) extends Expr[A]
case class PairLiter[A](info: A) extends Expr[A]
