package wacc

import parsley.{Parsley, Result, Success, Failure}
import parsley.expr.chain
import parsley.combinator.*
import parsley.expr.{precedence, Ops, InfixL, InfixN, Prefix}
import parsley.position.pos
import parsley.errors.combinator.*

import lexer.{fully, ident, string, boolLiter, nullLiter}
import lexer.implicits.given

import wacc.{Program, Func, Param, Stmt, Expr, LValue, RValue, Ident, ArrayElem, PairElem, 
             IntLiter, BoolLiter, CharLiter, StrLiter, PairLiter, ArrayLiter, NewPair, Call,
             UnaryExpr, BinaryExpr, SyntaxType, UnaryOperator, BinaryOperator, SyntacticData, SourcePos}
import Bridges.{int, character}

object parser {
    def parse(input: String): Either[String, Program[SyntacticData]] = 
        programParser.parse(input) match {
            case Success(ast) => Right(ast)
            case Failure(err) => Left(err.toString)
        }

    /* Helper to wrap position in unified SyntacticData wrapper */
    private val syntacticPos: Parsley[SyntacticData] = pos.map(SyntacticData.Pos.apply)

    /* Top-level grammar */
    private lazy val programParser = fully(program)
    private lazy val program: Parsley[Program[SyntacticData]] = ???
    private lazy val func: Parsley[Func[SyntacticData]] = ???
    private lazy val paramList: Parsley[List[Param[SyntacticData]]] = ???
    private lazy val param: Parsley[Param[SyntacticData]] = ???

    /* Type grammar */
    private lazy val wacc_type: Parsley[SyntaxType] = ???

    private lazy val baseType: Parsley[SyntaxType] = ???

    private lazy val arrayType: Parsley[SyntaxType] = ???
    private lazy val pairType: Parsley[SyntaxType] = ???
    private lazy val pairElemType: Parsley[SyntaxType] = ???

    /* Statement grammar */
    private lazy val stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val skip: Parsley[Stmt[SyntacticData]] = ???
    private lazy val decl: Parsley[Stmt[SyntacticData]] = ???
    private lazy val asgn: Parsley[Stmt[SyntacticData]] = ???
    private lazy val read: Parsley[Stmt[SyntacticData]] = ???
    private lazy val free: Parsley[Stmt[SyntacticData]] = ???
    private lazy val return_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val exit: Parsley[Stmt[SyntacticData]] = ???
    private lazy val print: Parsley[Stmt[SyntacticData]] = ???
    private lazy val println_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val if_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val while_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val scope: Parsley[Stmt[SyntacticData]] = ???
    private lazy val stmtSeq: Parsley[Stmt[SyntacticData]] = ???

    /* Expression grammar */
    private lazy val expr: Parsley[Expr[SyntacticData]] = ???
    private lazy val atom: Parsley[Expr[SyntacticData]] = ???

    /* LValue grammar */
    private lazy val lvalue: Parsley[LValue[SyntacticData]] = ???
    private lazy val arrayElem: Parsley[ArrayElem[SyntacticData]] = ???
    private lazy val pairElem: Parsley[PairElem[SyntacticData]] = ???

    /* RValue grammar */
    private lazy val rvalue: Parsley[RValue[SyntacticData]] = ???
    private lazy val arrayLiter: Parsley[ArrayLiter[SyntacticData]] = ???
    private lazy val newPair: Parsley[NewPair[SyntacticData]] = ???
    private lazy val call: Parsley[Call[SyntacticData]] = ???
    private lazy val argList: Parsley[List[Expr[SyntacticData]]] = ???

    /* Literal grammar */
    private lazy val intLit: Parsley[IntLiter[SyntacticData]] = ???
    private lazy val boolLit: Parsley[BoolLiter[SyntacticData]] = ???
    private lazy val charLit: Parsley[CharLiter[SyntacticData]] = ???
    private lazy val strLit: Parsley[StrLiter[SyntacticData]] = ???
    private lazy val pairLit: Parsley[PairLiter[SyntacticData]] = ???

    /* Operators */
    private lazy val unaryOps: List[Parsley[Expr[SyntacticData] => Expr[SyntacticData]]] = ???
    private lazy val mulOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val addOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val cmpOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val eqOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val andOp: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val orOp: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
}
