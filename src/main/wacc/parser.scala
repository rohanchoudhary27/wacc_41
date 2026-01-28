package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.*
import parsley.Parsley.*
import parsley.position.pos
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}

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
    private lazy val waccType: Parsley[SyntaxType] = baseType

    private lazy val baseType: Parsley[SyntaxType] =
        "int" #> SyntaxType.Int |
        "char" #> SyntaxType.Char |
        "bool" #> SyntaxType.Bool |
        "string" #> SyntaxType.String
    private lazy val arrayType: Parsley[SyntaxType] = ???
    private lazy val pairType: Parsley[SyntaxType] = ???
    private lazy val pairElemType: Parsley[SyntaxType] = ???

    /* Statement grammar */
    private lazy val stmt: Parsley[Stmt[SyntacticData]] = Block(sepBy1(singleStmt, ";"))
    private lazy val singleStmt: Parsley[Stmt[SyntacticData]] = skip | exit | print | println_stmt | decl | asgn |
                                                                scope| ifStmt | whileStmt
    private lazy val skip: Parsley[Stmt[SyntacticData]] = Skip from "skip"
    private lazy val decl: Parsley[Stmt[SyntacticData]] = Decl(baseType, identLit, "=" ~> rvalue)
    private lazy val asgn: Parsley[Stmt[SyntacticData]] = Asgn(lvalue, "=" ~> rvalue)
    private lazy val read: Parsley[Stmt[SyntacticData]] = ???
    private lazy val free: Parsley[Stmt[SyntacticData]] = ???
    private lazy val return_stmt: Parsley[Stmt[SyntacticData]] = ???
    private lazy val exit: Parsley[Stmt[SyntacticData]] = Exit("exit" ~> expr)
    private lazy val print: Parsley[Stmt[SyntacticData]] = Print("print" ~> expr)
    private lazy val println_stmt: Parsley[Stmt[SyntacticData]] = Println("println" ~> expr)
    private lazy val ifStmt: Parsley[Stmt[SyntacticData]] =
        If("if" ~> expr <~ "then", stmt, "else" ~> stmt <~ "fi")
    private lazy val whileStmt: Parsley[Stmt[SyntacticData]] = While("while" ~> expr <~ "do", stmt <~ "done")
    private lazy val scope: Parsley[Stmt[SyntacticData]] = "begin" ~> stmt <~ "end"

    /* Expression grammar */
    private lazy val expr: Parsley[Expr[SyntacticData]] = precedence(atom)(
        Ops(Prefix)(unaryOps),
        Ops(InfixL)(mulOps),
        Ops(InfixL)(addOps),
        Ops(InfixN)(cmpOps),
        Ops(InfixN)(eqOps),
        Ops(InfixR)(andOps),
        Ops(InfixR)(orOps)
    )
    private lazy val atom: Parsley[Expr[SyntacticData]] =
        intLit | boolLit | charLit | strLit | pairLit | identLit

    /* LValue grammar */
    private lazy val lvalue: Parsley[LValue[SyntacticData]] = identLit // | arrayElem | pairElem
    private lazy val arrayElem: Parsley[ArrayElem[SyntacticData]] = ???
    private lazy val pairElem: Parsley[PairElem[SyntacticData]] = ???

    /* RValue grammar */
    private lazy val rvalue: Parsley[RValue[SyntacticData]] = expr
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
    private lazy val identLit  = Ident(ident)

    /* Operators */
    private lazy val unaryOps: Parsley[Expr[SyntacticData] => Expr[SyntacticData]] = {
        prefix("!", UnaryOperator.Not) |
        prefix("-", UnaryOperator.Neg) |
        prefix("len", UnaryOperator.Len) |
        prefix("ord", UnaryOperator.Ord) |
        prefix("chr", UnaryOperator.Chr)
    }
    private lazy val mulOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("*", BinaryOperator.Mul) |
        infix("/", BinaryOperator.Div) |
        infix("%", BinaryOperator.Mod)
    }
    private lazy val addOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("+", BinaryOperator.Add) |
        infix("-", BinaryOperator.Sub)
    }
    private lazy val cmpOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix(">", BinaryOperator.GT) |
        infix(">=", BinaryOperator.GTE) |
        infix("<", BinaryOperator.LT) |
        infix("<=", BinaryOperator.LTE)
    }
    private lazy val eqOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("==", BinaryOperator.EQ) |
        infix("!=", BinaryOperator.NEQ)
    }
    private lazy val andOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("&&", BinaryOperator.And)
    }
    private lazy val orOps: Parsley[(Expr[SyntacticData], Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("||", BinaryOperator.Or)
    }

    /* Helper */
    private def prefix(str: String, op: UnaryOperator): Parsley[Expr[SyntacticData] => Expr[SyntacticData]] = 
        (str *> pos).map{p => (e: Expr[SyntacticData]) => (new UnaryExpr(op, e, SyntacticData.Pos(p)))}

    private def infix(str: String, op: BinaryOperator): Parsley[(Expr[SyntacticData],Expr[SyntacticData]) => Expr[SyntacticData]] =
        (str *> pos).map{p => (left: Expr[SyntacticData], right: Expr[SyntacticData]) => new BinaryExpr(op, left, right, SyntacticData.Pos(p))}
}
