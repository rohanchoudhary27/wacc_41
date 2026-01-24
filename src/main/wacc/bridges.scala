package wacc

import parsley.macros.bridge
import parsley.Parsley

import wacc.{Program, Func, Param, Stmt, Expr, LValue, RValue, Ident, ArrayElem, PairElem, 
             IntLiter, BoolLiter, CharLiter, StrLiter, PairLiter, ArrayLiter, NewPair, Call,
             UnaryExpr, BinaryExpr, SyntaxType, UnaryOperator, BinaryOperator, SyntacticData}

object Bridges {
    /* Type-converted lexer parsers */
    val int       = lexer.integer.map(_.toInt)
    val character = lexer.char.map(_.toChar)

    /* Top-level constructs */
    val Program = bridge[Program[SyntacticData]]
    val Func    = bridge[Func[SyntacticData]]
    val Param   = bridge[Param[SyntacticData]]

    /* Statements */
    val Skip    = bridge[Stmt.Skip[SyntacticData]]
    val Decl    = bridge[Stmt.Decl[SyntacticData]]
    val Asgn    = bridge[Stmt.Asgn[SyntacticData]]
    val Read    = bridge[Stmt.Read[SyntacticData]]
    val Free    = bridge[Stmt.Free[SyntacticData]]
    val Return  = bridge[Stmt.Return[SyntacticData]]
    val Exit    = bridge[Stmt.Exit[SyntacticData]]
    val Print   = bridge[Stmt.Print[SyntacticData]]
    val Println = bridge[Stmt.Println[SyntacticData]]
    val If      = bridge[Stmt.If[SyntacticData]]
    val While   = bridge[Stmt.While[SyntacticData]]
    val Scope   = bridge[Stmt.Scope[SyntacticData]]
    val Block   = bridge[Stmt.Block[SyntacticData]]

    /* Operator nodes */
    val UnaryExpr  = bridge[UnaryExpr[SyntacticData]]
    val BinaryExpr = bridge[BinaryExpr[SyntacticData]]

    /* LValues and reference access */
    val Ident     = bridge[Ident[SyntacticData]]
    val ArrayElem = bridge[ArrayElem[SyntacticData]]
    val PairElem  = bridge[PairElem[SyntacticData]]

    /* Complex RValues */
    val NewPair = bridge[NewPair[SyntacticData]]
    val Call    = bridge[Call[SyntacticData]]

    /* Literal nodes */
    val IntLiter   = bridge[IntLiter[SyntacticData]]
    val BoolLiter  = bridge[BoolLiter[SyntacticData]]
    val CharLiter  = bridge[CharLiter[SyntacticData]]
    val StrLiter   = bridge[StrLiter[SyntacticData]]
    val PairLiter  = bridge[PairLiter[SyntacticData]]
    val ArrayLiter = bridge[ArrayLiter[SyntacticData]]
}
