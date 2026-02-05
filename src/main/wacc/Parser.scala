package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.*
import parsley.Parsley.*
import parsley.position.pos

import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix, chain}
import parsley.errors.combinator.ErrorMethods

import lexer.{fully, ident, string, boolLiter, nullLiter, brackets, parens}
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

    private lazy val func =
        Func(waccType, identLit, 
            parens(paramList), "is" ~> (stmt label "function body") <~ "end"
        ) label "function definition"

    private lazy val paramList = sepBy(param, ",") label "parameter list"
    private lazy val param = Param(waccType, identLit) label "parameter"

    /* Type grammar */
    /* arrayType uses chain.postfix, so is equivalent to base/pair types if no brackets */
    private lazy val waccType = arrayType

    private lazy val baseType = (
        "int"    #> SyntaxType.Int |
        "char"   #> SyntaxType.Char |
        "bool"   #> SyntaxType.Bool |
        "string" #> SyntaxType.String
    ) label "type"

    private lazy val arrayType = chain.postfix(baseType | atomic(pairType))(
        (brackets(unit) label "array type") #> ((t: SyntaxType) => SyntaxType.Array(t))
    )

    private lazy val pairType =
        "pair" ~> parens(Pair(pairElemType <~ ",", pairElemType)) label "pair type"

    private lazy val pairElemType = chain.postfix(baseType | "pair" #> SyntaxType.ErasedPair)(
        brackets(unit) #> ((t: SyntaxType) => SyntaxType.Array(t))
    ) label "pair element type" explain 
        "pair elements can have base types (int, bool, char, string), nested pairs, or arrays"

    /* Statement grammar */
    private lazy val stmt: Parsley[Stmt[SyntacticData]] = 
        Block(sepBy1(singleStmt, ";"))

    private lazy val singleStmt = 
        skip | 
        read | 
        returnStmt |
        (asgn label "assignment") |
        (decl label "type") |
        ((print | println_stmt | exit | free) label "builtin call") |
        (scope label "new scope") |
        ((ifStmt | whileStmt) label "control flow")

    private lazy val skip = Skip from "skip"
    private lazy val decl = Decl(waccType, identLit, "=" ~> (
        rvalue label "initial value" explain "variable must be initialised with a value"
    ))
    private lazy val asgn = Asgn(lvalue, "=" ~> 
        (rvalue label "value" explain "assignment requires a value"))

    private lazy val read = Read("read" ~> (lvalue label "variable to read into"))
    private lazy val free = Free("free" ~> (expr label "pair or array to free"))
    private lazy val returnStmt = Return("return" ~> (expr label "return value"))
    private lazy val exit = Exit("exit" ~> expr label "exit code")

    private lazy val print = Print("print" ~> expr label "value to print")
    private lazy val println_stmt = Println("println" ~> expr label "value to print")

    private lazy val ifStmt = If(
        "if" ~> 
            (expr label "condition" explain "if conditions must be boolean expressions") 
        <~ "then",
            (stmt label "then branch"),
        "else" ~> 
            (stmt label "else branch") 
        <~ "fi"
    )

    private lazy val whileStmt = While(
        "while" ~> 
            (expr label "condition" explain "while conditions must be boolean expressions") 
        <~ "do",
            (stmt label "loop body") 
        <~ "done"
    )

    private lazy val scope = "begin" ~> (stmt label "scoped statement") <~ "end"

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
    private lazy val atom = intLit | boolLit | charLit | strLit | pairLit |
        atomic(arrayElem) | identLit |
        parens(
            expr label "expression" explain "expression required inside parentheses"
        ) label "bracketed expression"

    /* LValue grammar */
    private lazy val lvalue: Parsley[LValue[SyntacticData]] = 
        (atomic(arrayElem) label "array element") |
        (pairElem label "pair element") | 
        (identLit label "assignable variable")

    private lazy val arrayElem = ArrayElem(identLit, some(brackets(
        expr label "array index" explain "array indices must be integer expressions"
    )))
    private lazy val pairElem = 
        PairElem("fst" #> true | "snd" #> false, lvalue) label "pair element"

    /* RValue grammar */
    private lazy val rvalue = (arrayLiter label "array literal") |
        (expr | call label "value expression") |
        (newPair label "newpair expression") |
        (pairElem label "pair element expression")

    private lazy val arrayLiter = ArrayLiter(brackets(sepBy(expr, ",")))
    private lazy val newPair = NewPair(
        "newpair" ~> "(" ~> (expr label "first element") <~ ",", 
        (expr label "second element") <~ ")"
    )
    private lazy val call = 
        Call("call" ~> identLit, parens(sepBy(expr, ","))) label "function call"

    /* Literal grammar */
    private lazy val intLit   = IntLiter(int) label "integer"
    private lazy val boolLit  = BoolLiter(boolLiter) label "boolean"
    private lazy val charLit  = CharLiter(character) label "character"
    private lazy val strLit   = StrLiter(string) label "string"
    private lazy val pairLit  = (PairLiter from nullLiter) label "pair literal"
    private lazy val identLit = Ident(ident) label "identifier"

    /* Operator grammar */
    private lazy val unaryOps = {
        prefix("!", UnaryOperator.Not) |
        prefix("-", UnaryOperator.Neg) |
        prefix("len", UnaryOperator.Len) |
        prefix("ord", UnaryOperator.Ord) |
        prefix("chr", UnaryOperator.Chr)
    }
    private lazy val mulOps = {
        infix("*", BinaryOperator.Mul) |
        infix("/", BinaryOperator.Div) |
        infix("%", BinaryOperator.Mod)
    }
    private lazy val addOps = {
        infix("+", BinaryOperator.Add) |
        infix("-", BinaryOperator.Sub)
    }
    private lazy val cmpOps = {
        infix(">", BinaryOperator.GT) |
        infix(">=", BinaryOperator.GTE) |
        infix("<", BinaryOperator.LT) |
        infix("<=", BinaryOperator.LTE)
    }
    private lazy val eqOps = {
        infix("==", BinaryOperator.EQ) |
        infix("!=", BinaryOperator.NEQ)
    }
    private lazy val andOps = {
        infix("&&", BinaryOperator.And)
    }
    private lazy val orOps = {
        infix("||", BinaryOperator.Or)
    }

    /* Parses a prefix operator string and wraps the operand in a UnaryExpr node */
    private def prefix(str: String, op: UnaryOperator) = {
        ((str *> pos).map{p => (e: Expr[SyntacticData]) => 
            (new UnaryExpr(op, e, SyntacticData.Pos(p)))}) label "unary operator"
    }

    /* Parses an infix operator string and combines operands to a BinaryExpr node */
    private def infix(str: String, op: BinaryOperator) = {
        ((str *> pos).map{p => (left: Expr[SyntacticData], right: Expr[SyntacticData]) =>
            (new BinaryExpr(op, left, right, SyntacticData.Pos(p)))}) label "binary operator"
    }
}
