package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.*
import parsley.Parsley.*
import parsley.position.pos
import parsley.lift.lift2
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix, chain}
import parsley.errors.combinator.ErrorMethods

import lexer.{fully, ident, string, boolLiter, nullLiter, brackets}
import lexer.implicits.given
import Bridges.*

/**
* The Parsley parser, using combinators to create higher-level parsers to
* ultimately be able to parse an entire WACC program. 
*/
object parser {

    /**
    * Parse a WACC program inputted as a string.
    * 
    * @param input: The input WACC program
    * 
    * @return Either[String, Program[SyntacticData]]: Either a parsed
    * AST tagged with SyntacticData on success, or an error string on failure
    */
    def parse(input: String): Either[String, Program[SyntacticData]] = 
        programParser.parse(input) match {
            case Success(ast) => Right(ast)
            case Failure(err) => Left(err.toString)
        }

    /* Top-level grammar */
    private lazy val programParser = fully(program)
    private lazy val program = Program(
        "begin" ~> many(atomic(func)) label "function definitions",
        (stmt label "program body" explain "program must contain at least one statement") <~ "end"
    )
    private lazy val func: Parsley[Func[SyntacticData]] =
        Func(waccType, identLit, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end") label "function definition"
    private lazy val paramList: Parsley[List[Param[SyntacticData]]] = sepBy(param, ",") label "parameter list"
    private lazy val param: Parsley[Param[SyntacticData]] = Param(waccType, identLit) label "parameter"

    /* Type grammar */
    /* arrayType uses chain.postfix, so is equivalent to base/pair types if no brackets */
    private lazy val waccType: Parsley[SyntaxType] = arrayType
    private lazy val baseType: Parsley[SyntaxType] = (
        "int" #> SyntaxType.Int |
        "char" #> SyntaxType.Char |
        "bool" #> SyntaxType.Bool |
        "string" #> SyntaxType.String
    ) label "type"
    private lazy val arrayType: Parsley[SyntaxType] =
        chain.postfix(baseType | atomic(pairType))(
            brackets(unit) #> ((t: SyntaxType) => SyntaxType.Array(t))
        )
    private lazy val pairType: Parsley[SyntaxType] =
        "pair" ~> "(" ~> lift2(SyntaxType.Pair.apply, pairElemType <~ ",", pairElemType) <~ ")" label "pair type"
    private lazy val pairElemType: Parsley[SyntaxType] =
        chain.postfix(baseType | "pair" #> SyntaxType.ErasedPair)(
            brackets(unit) #> ((t: SyntaxType) => SyntaxType.Array(t))
        )

    /* Statement grammar */
    private lazy val stmt: Parsley[Stmt[SyntacticData]] =
        Block(sepBy1(singleStmt, ";")) label "statement"
    private lazy val singleStmt: Parsley[Stmt[SyntacticData]] = skip | exit | print | println_stmt |
                                                                decl | asgn | read | scope |
                                                                ifStmt | whileStmt | returnStmt |
                                                                free
    private lazy val skip: Parsley[Stmt[SyntacticData]] = Skip from "skip"
    private lazy val decl: Parsley[Stmt[SyntacticData]] =
        Decl(waccType, identLit, "=" ~> (
            rvalue label "initial value" explain "variable must be initialised with a value"
        ))
    private lazy val asgn: Parsley[Stmt[SyntacticData]] =
        Asgn(lvalue, "=" ~> (rvalue label "value" explain "assignment requires a value"))
    private lazy val read: Parsley[Stmt[SyntacticData]] =
        Read("read" ~> lvalue)
    private lazy val free: Parsley[Stmt[SyntacticData]] = Free("free" ~> expr)
    private lazy val returnStmt: Parsley[Stmt[SyntacticData]] = Return("return" ~> expr)
    private lazy val exit: Parsley[Stmt[SyntacticData]] = Exit("exit" ~> expr label "exit code")
    private lazy val print: Parsley[Stmt[SyntacticData]] =
        Print("print" ~> expr label "value to print")
    private lazy val println_stmt: Parsley[Stmt[SyntacticData]] =
        Println("println" ~> expr label "value to print")
    private lazy val ifStmt: Parsley[Stmt[SyntacticData]] =
        If(
            "if" ~> (expr label "condition" explain "if conditions must be boolean expressions")
                <~ "then",
            stmt label "then branch",
            "else" ~> (stmt label "else branch") <~ "fi"
        )
    private lazy val whileStmt: Parsley[Stmt[SyntacticData]] = While(
        "while" ~> (expr label "condition" explain "while conditions must be boolean expressions")
            <~ "do",
        (stmt label "loop body") <~ "done"
    )
    private lazy val scope: Parsley[Stmt[SyntacticData]] =
        "begin" ~> (stmt label "scoped statement") <~ "end"

    /* Expression grammar */
    private lazy val expr: Parsley[Expr[SyntacticData]] = precedence(atom)(
        Ops(Prefix)(unaryOps),
        Ops(InfixL)(mulOps),
        Ops(InfixL)(addOps),
        Ops(InfixN)(cmpOps),
        Ops(InfixN)(eqOps),
        Ops(InfixR)(andOps),
        Ops(InfixR)(orOps)
    ) label "expression"
    /* atomic(arrayElem) is tried before identLit since both start with ident */
    private lazy val atom: Parsley[Expr[SyntacticData]] =
        intLit | boolLit | charLit | strLit | pairLit |
        atomic(arrayElem) | identLit |
        ("(" ~> (
            expr label "expression" explain "expression required inside parentheses"
        ) <~ ")") label "bracketed expression"

    /* LValue grammar */
    private lazy val lvalue: Parsley[LValue[SyntacticData]] =
        (atomic(arrayElem) label "array element") |
        (pairElem label "pair element") |
        (identLit label "assignable variable")
    private lazy val arrayElem: Parsley[ArrayElem[SyntacticData]] =
        ArrayElem(identLit, some(brackets(expr)))
    private lazy val pairElem: Parsley[PairElem[SyntacticData]] =
        PairElem("fst" #> true | "snd" #> false, lvalue) label "pair element"

    /* RValue grammar */
    private lazy val rvalue: Parsley[RValue[SyntacticData]] =
        (arrayLiter label "array literal") |
        (expr | call label "value expression") |
        (NewPair("newpair" ~> "(" ~> expr <~ ",", expr <~ ")") label "newpair expression") |
        (pairElem label "pair element expression")
    private lazy val arrayLiter: Parsley[ArrayLiter[SyntacticData]] =
        ArrayLiter(brackets(sepBy(expr, ",")))
    private lazy val call: Parsley[Call[SyntacticData]] =
        Call("call" ~> identLit, "(" ~> sepBy(expr, ",") <~ ")") label "function call"

    /* Literal grammar */
    private lazy val intLit  = IntLiter(int) label "integer"
    private lazy val boolLit = BoolLiter(boolLiter) label "boolean"
    private lazy val charLit = CharLiter(character) label "character"
    private lazy val strLit  = StrLiter(string) label "string"
    private lazy val pairLit = (PairLiter from nullLiter) label "pair literal"
    private lazy val identLit  = Ident(ident) label "identifier"

    /* Operator grammar */
    private lazy val unaryOps: Parsley[Expr[SyntacticData] => Expr[SyntacticData]] = {
        prefix("!", UnaryOperator.Not) |
        prefix("-", UnaryOperator.Neg) |
        prefix("len", UnaryOperator.Len) |
        prefix("ord", UnaryOperator.Ord) |
        prefix("chr", UnaryOperator.Chr)
    }
    private lazy val mulOps: Parsley[(Expr[SyntacticData],
                                      Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("*", BinaryOperator.Mul) |
        infix("/", BinaryOperator.Div) |
        infix("%", BinaryOperator.Mod)
    }
    private lazy val addOps: Parsley[(Expr[SyntacticData],
                                      Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("+", BinaryOperator.Add) |
        infix("-", BinaryOperator.Sub)
    }
    private lazy val cmpOps: Parsley[(Expr[SyntacticData],
                                      Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix(">", BinaryOperator.GT) |
        infix(">=", BinaryOperator.GTE) |
        infix("<", BinaryOperator.LT) |
        infix("<=", BinaryOperator.LTE)
    }
    private lazy val eqOps: Parsley[(Expr[SyntacticData],
                                     Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("==", BinaryOperator.EQ) |
        infix("!=", BinaryOperator.NEQ)
    }
    private lazy val andOps: Parsley[(Expr[SyntacticData],
                                      Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("&&", BinaryOperator.And)
    }
    private lazy val orOps: Parsley[(Expr[SyntacticData],
                                     Expr[SyntacticData]) => Expr[SyntacticData]] = {
        infix("||", BinaryOperator.Or)
    }

    /* Parses a prefix operator string and wraps the operand in a UnaryExpr node */
    private def prefix(str: String, op: UnaryOperator): Parsley[Expr[SyntacticData]
                                                        => Expr[SyntacticData]] = {
        (str *> pos).map{p => (e: Expr[SyntacticData]) =>
            (new UnaryExpr(op, e, SyntacticData.Pos(p)))}
   }

    /* Parses an infix operator string and combines operands to a BinaryExpr node */
    private def infix(str: String,
                      op: BinaryOperator): Parsley[(Expr[SyntacticData],
                                                    Expr[SyntacticData]) => Expr[SyntacticData]] = {

        (str *> pos).map{p => (left: Expr[SyntacticData], right: Expr[SyntacticData]) =>
            new BinaryExpr(op, left, right, SyntacticData.Pos(p))}
    }
}
