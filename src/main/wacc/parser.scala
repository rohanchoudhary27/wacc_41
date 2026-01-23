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
             UnaryExpr, BinaryExpr, Type, UnaryOperator, BinaryOperator}
import Bridges.{int, character}

object parser {
    type SourcePos = (Int, Int)

    def parse(input: String): Either[String, Program[SourcePos]] = 
        programParser.parse(input) match {
            case Success(ast) => Right(ast)
            case Failure(err) => Left(err.toString)
        }

    // Top-level grammar
    private lazy val programParser = fully(program)
    private lazy val program: Parsley[Program[SourcePos]] = ???
    private lazy val func: Parsley[Func[SourcePos]] = ???
    private lazy val paramList: Parsley[List[Param[SourcePos]]] = ???
    private lazy val param: Parsley[Param[SourcePos]] = ???

    // Type grammar
    private lazy val wacc_type: Parsley[Type] = ???

    private lazy val baseType: Parsley[Type] = ???

    private lazy val arrayType: Parsley[Type] = ???
    private lazy val pairType: Parsley[Type] = ???
    private lazy val pairElemType: Parsley[Type] = ???

    // Statement grammar
    private lazy val stmt: Parsley[Stmt[SourcePos]] = ???
    private lazy val skip: Parsley[Stmt[SourcePos]] = ???
    private lazy val decl: Parsley[Stmt[SourcePos]] = ???
    private lazy val asgn: Parsley[Stmt[SourcePos]] = ???
    private lazy val read: Parsley[Stmt[SourcePos]] = ???
    private lazy val free: Parsley[Stmt[SourcePos]] = ???
    private lazy val return_stmt: Parsley[Stmt[SourcePos]] = ???
    private lazy val exit: Parsley[Stmt[SourcePos]] = ???
    private lazy val print: Parsley[Stmt[SourcePos]] = ???
    private lazy val println_stmt: Parsley[Stmt[SourcePos]] = ???
    private lazy val if_stmt: Parsley[Stmt[SourcePos]] = ???
    private lazy val while_stmt: Parsley[Stmt[SourcePos]] = ???
    private lazy val scope: Parsley[Stmt[SourcePos]] = ???
    private lazy val stmtSeq: Parsley[Stmt[SourcePos]] = ???

    // Expression grammar
    private lazy val expr: Parsley[Expr[SourcePos]] = ???
    private lazy val atom: Parsley[Expr[SourcePos]] = ???

    // LValue grammar
    private lazy val lvalue: Parsley[LValue[SourcePos]] = ???
    private lazy val arrayElem: Parsley[ArrayElem[SourcePos]] = ???
    private lazy val pairElem: Parsley[PairElem[SourcePos]] = ???

    // RValue grammar
    private lazy val rvalue: Parsley[RValue[SourcePos]] = ???
    private lazy val arrayLiter: Parsley[ArrayLiter[SourcePos]] = ???
    private lazy val newPair: Parsley[NewPair[SourcePos]] = ???
    private lazy val call: Parsley[Call[SourcePos]] = ???
    private lazy val argList: Parsley[List[Expr[SourcePos]]] = ???

    // Literal grammar
    private lazy val intLit: Parsley[IntLiter[SourcePos]] = ???
    private lazy val boolLit: Parsley[BoolLiter[SourcePos]] = ???
    private lazy val charLit: Parsley[CharLiter[SourcePos]] = ???
    private lazy val strLit: Parsley[StrLiter[SourcePos]] = ???
    private lazy val pairLit: Parsley[PairLiter[SourcePos]] = ???

    // Operators
    private lazy val unaryOps: List[Parsley[Expr[SourcePos] => Expr[SourcePos]]] = ???
    private lazy val mulOps: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
    private lazy val addOps: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
    private lazy val cmpOps: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
    private lazy val eqOps: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
    private lazy val andOp: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
    private lazy val orOp: List[Parsley[(Expr[SourcePos], Expr[SourcePos]) => Expr[SourcePos]]] = ???
}
