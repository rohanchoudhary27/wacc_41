package wacc

import parsley.Parsley
import parsley.position.pos
import parsley.macros.{bridge, ParsableMetadata}

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

/**
* Provides automatic position tracking for Parsley bridges.
* This allows bridges to automatically inject SyntacticData.Pos
* without manual position threading in grammar rules.
*
* When a bridge encounters a parameter annotated with @isMeta of type
* SyntacticData, it will use this given to automatically parse and inject
* the position information.
*/
given ParsableMetadata[SyntacticData] with {
  def meta: Parsley[SyntacticData] = pos.map(SyntacticData.Pos.apply)
}
