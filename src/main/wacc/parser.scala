package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.*
import parsley.Parsley.*
import parsley.expr.{chain, precedence, Ops, InfixL, InfixN, Prefix}

import lexer.{fully, ident, string, boolLiter, nullLiter}
import lexer.implicits.given
import Bridges.*

object parser {
    def parse(input: String): Either[String, Program[SyntacticData]] = 
        programParser.parse(input) match {
            case Success(ast) => Right(ast)
            case Failure(err) => Left(err.toString)
        }

    /* Top-level grammar */
    private lazy val programParser = fully(program)
    private lazy val program = Program("begin" ~> many(func), stmt <~ "end")
    private lazy val func: Parsley[Func[SyntacticData]] = Parsley.empty
    private lazy val paramList: Parsley[List[Param[SyntacticData]]] = ???
    private lazy val param: Parsley[Param[SyntacticData]] = ???

    /* Type grammar */
    private lazy val wacc_type: Parsley[SyntaxType] = ???

    private lazy val baseType: Parsley[SyntaxType] = ???

    private lazy val arrayType: Parsley[SyntaxType] = ???
    private lazy val pairType: Parsley[SyntaxType] = ???
    private lazy val pairElemType: Parsley[SyntaxType] = ???

    /* Statement grammar */
    private lazy val stmt: Parsley[Stmt[SyntacticData]] = skip | exit | print | println_stmt
    private lazy val skip: Parsley[Stmt[SyntacticData]] = Skip from "skip"
    private lazy val decl: Parsley[Stmt[SyntacticData]] = ???
    private lazy val asgn: Parsley[Stmt[SyntacticData]] = ???
    private lazy val read: Parsley[Stmt[SyntacticData]] = ???
    private lazy val free: Parsley[Stmt[SyntacticData]] = ???
    private lazy val return_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val exit: Parsley[Stmt[SyntacticData]] = Exit("exit" ~> expr)
    private lazy val print: Parsley[Stmt[SyntacticData]] = Print("print" ~> expr)
    private lazy val println_stmt: Parsley[Stmt[SyntacticData]] = Println("println" ~> expr)
    private lazy val if_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val while_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val scope: Parsley[Stmt[SyntacticData]] = ???
    private lazy val stmtSeq: Parsley[Stmt[SyntacticData]] = ???

    /* Expression grammar */
    private lazy val expr: Parsley[Expr[SyntacticData]] = atom
    private lazy val atom: Parsley[Expr[SyntacticData]] = intLit | boolLit | charLit | strLit | pairLit

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
    private lazy val intLit  = IntLiter(int)
    private lazy val boolLit = BoolLiter(boolLiter)
    private lazy val charLit = CharLiter(character)
    private lazy val strLit  = StrLiter(string)
    private lazy val pairLit = PairLiter from nullLiter

    /* Operators */
    private lazy val unaryOps: List[Parsley[Expr[SyntacticData] => Expr[SyntacticData]]] = ???
    private lazy val mulOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val addOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val cmpOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val eqOps: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val andOp: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
    private lazy val orOp: List[Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]]] = ???
}
