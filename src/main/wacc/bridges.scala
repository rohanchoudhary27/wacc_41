package wacc

import parsley.macros.bridge
import parsley.Parsley

import wacc.{Program, Func, Param, Stmt, Expr, LValue, RValue, Ident, ArrayElem, PairElem, 
             IntLiter, BoolLiter, CharLiter, StrLiter, PairLiter, ArrayLiter, NewPair, Call,
             UnaryExpr, BinaryExpr, Type, UnaryOperator, BinaryOperator}
import parser.SourcePos

object Bridges {
    // Type-converted lexer parsers
    val int       = lexer.integer.map(_.toInt)
    val character = lexer.char.map(_.toChar)

    // Top-level constructs
    val Program = bridge[Program[SourcePos]]
    val Func    = bridge[Func[SourcePos]]
    val Param   = bridge[Param[SourcePos]]

    // Statements
    val Skip    = bridge[Stmt.Skip[SourcePos]]
    val Decl    = bridge[Stmt.Decl[SourcePos]]
    val Asgn    = bridge[Stmt.Asgn[SourcePos]]
    val Read    = bridge[Stmt.Read[SourcePos]]
    val Free    = bridge[Stmt.Free[SourcePos]]
    val Return  = bridge[Stmt.Return[SourcePos]]
    val Exit    = bridge[Stmt.Exit[SourcePos]]
    val Print   = bridge[Stmt.Print[SourcePos]]
    val Println = bridge[Stmt.Println[SourcePos]]
    val If      = bridge[Stmt.If[SourcePos]]
    val While   = bridge[Stmt.While[SourcePos]]
    val Scope   = bridge[Stmt.Scope[SourcePos]]
    val Block   = bridge[Stmt.Block[SourcePos]]

    // Operator nodes
    val UnaryExpr  = bridge[UnaryExpr[SourcePos]]
    val BinaryExpr = bridge[BinaryExpr[SourcePos]]

    // LValues and reference access
    val Ident     = bridge[Ident[SourcePos]]
    val ArrayElem = bridge[ArrayElem[SourcePos]]
    val PairElem  = bridge[PairElem[SourcePos]]

    // Complex RValues
    val NewPair = bridge[NewPair[SourcePos]]
    val Call    = bridge[Call[SourcePos]]

    // Literal nodes
    val IntLiter  = bridge[IntLiter[SourcePos]]
    val BoolLiter = bridge[BoolLiter[SourcePos]]
    val CharLiter = bridge[CharLiter[SourcePos]]
    val StrLiter  = bridge[StrLiter[SourcePos]]
    val PairLiter = bridge[PairLiter[SourcePos]]
    val ArrayLiter = bridge[ArrayLiter[SourcePos]]
}
