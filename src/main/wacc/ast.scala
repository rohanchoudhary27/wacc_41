package wacc

import parsley.macros.isMeta

type Args[A] = List[Expr[A]]

/* Traits */
sealed trait LValue[A] { val info: A }
sealed trait RValue[A] { val info: A }
sealed trait Expr[A] extends RValue[A] { val info: A }

/* The WACC syntactic type system */
enum SyntaxType {
    case Int, Bool, Char, String, ErasedPair
    case Pair(fst: SyntaxType, snd: SyntaxType)
    case Array(arrType: SyntaxType)
}

/* WACC operators */
enum BinaryOperator {
    case Add, Sub, Mul, Div, Mod
    case GT, GTE, LT, LTE, EQ, NEQ
    case And, Or
}
enum UnaryOperator {
    case Neg, Not, Len, Ord, Chr
}

case class Program[A](funcs: List[Func[A]], stmt: Stmt[A], @isMeta info: A)
case class Func[A](typ: SyntaxType, ident: Ident[A], params: List[Param[A]], stmt: Stmt[A], @isMeta info: A)
case class Param[A](typ: SyntaxType, ident: Ident[A], @isMeta info: A)

enum Stmt[A] {
    case Skip(@isMeta info: A)
    case Decl(typ: SyntaxType, ident: Ident[A], rval: RValue[A], @isMeta info: A)
    case Asgn(lval: LValue[A], rval: RValue[A], @isMeta info: A)
    case Read(lval: LValue[A], @isMeta info: A)
    case Free(expr: Expr[A], @isMeta info: A)
    case Return(expr: Expr[A], @isMeta info: A)
    case Exit(expr: Expr[A], @isMeta info: A)
    case Print(expr: Expr[A], @isMeta info: A)
    case Println(expr: Expr[A], @isMeta info: A)
    case If(cond: Expr[A], ifBlock: Stmt[A], elseBlock: Stmt[A], @isMeta info: A)
    case While(cond: Expr[A], stmt: Stmt[A], @isMeta info: A)
    case Block(stmts: List[Stmt[A]], @isMeta info: A)
}

/* Operator nodes */
case class UnaryExpr[A](op: UnaryOperator, expr: Expr[A], @isMeta info: A) extends Expr[A]
case class BinaryExpr[A](op: BinaryOperator, left: Expr[A], right: Expr[A], @isMeta info: A) extends Expr[A]

/* LValues and reference access */
case class Ident[A](name: String, @isMeta info: A) extends LValue[A] with Expr[A]
case class ArrayElem[A](ident: Ident[A], indices: List[Expr[A]], @isMeta info: A) extends LValue[A] with Expr[A]
case class PairElem[A](isFst: Boolean, lval: LValue[A], @isMeta info: A) extends LValue[A] with RValue[A]

/* Complex RValues */
case class NewPair[A](e1: Expr[A], e2: Expr[A], @isMeta info: A) extends RValue[A]
case class Call[A](ident: Ident[A], argList: Args[A], @isMeta info: A) extends RValue[A]

/* Literal nodes */
case class ArrayLiter[A](elements: List[Expr[A]], @isMeta info: A) extends RValue[A]
case class IntLiter[A](value: Int, @isMeta info: A) extends Expr[A]
case class BoolLiter[A](value: Boolean, @isMeta info: A) extends Expr[A]
case class CharLiter[A](value: Char, @isMeta info: A) extends Expr[A]
case class StrLiter[A](value: String, @isMeta info: A) extends Expr[A]
case class PairLiter[A](@isMeta info: A) extends Expr[A]
