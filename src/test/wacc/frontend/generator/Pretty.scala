package wacc.frontend.generator

import wacc.frontend.*
import wacc.backend.PairPos

/** Configuration for the WACC pretty printer
  *
  * @param indentSize
  *   Number of spaces to use per indentation level
  */
final case class PrettyConfig(
    indentSize: Int = 4
)

/** Pretty printing utility for turning a WACC AST into source text
  */
object Pretty {

    /** Pretty print a WACC program using the default PrettyConfig
      *
      * @param p
      *   The AST to pretty print
      * @return
      *   A WACC source textual representation of `p`
      */
    def program[A](p: Program[A]): String =
        programWith(PrettyConfig())(p)

    /** Pretty print a WACC using a provided configuration
      *
      * @param cfg
      *   Pretty print configuration
      * @param p
      *   The program AST to pretty print
      * @return
      *   A WACC source textual representation of `p`
      */
    def programWith[A](cfg: PrettyConfig)(p: Program[A]): String =
        val b = new Builder[A](cfg)
        b.emitLine("begin")
        b.indent {
            /* prettify functions and separate with "" */
            p.funcs.foreach { f =>
                b.emitFunc(f)
                b.emitLine("")
            }
            p.stmt match {
                case Stmt.Block(stmts, _) =>
                    // don't display a block again because we already display above
                    b.emitStmtList(stmts)
                case _ => ???
            }
        }
        b.emitLine("end")
        b.result()


    /** Internal printing builder
      *
      * Maintains indentation level
      *
      * @param cfg
      *   Pretty print configuration
      */
    private final class Builder[A](cfg: PrettyConfig) {
        private val sb = new StringBuilder
        private var ind: Int = 0

        /** Return the accumulated pretty printed output
          *
          * @return
          *   The pretty printed program as a string
          */
        def result(): String = sb.toString

        /** indentation spaces for the current indentation level
          *
          * @return
          *   A string of spaces of length `ind * cfg.indentSize`
          */
        private def pad: String = " " * (ind * cfg.indentSize)

        /** Emit a line at the current indentation level
          *
          * @param s
          *   The line content to emit
          */
        def emitLine(s: String): Unit =
            sb.append(pad).append(s).append('\n')

        /** Render `body< one indentation level deeper
          *
          * @param body
          *   The body function to display under an increased indentation level
          * @return
          *   The return of `body`
          */
        def indent[T](body: => T): T =
            ind += 1
            try body
            finally ind -= 1

        /** Emit a function definition in WACC syntax
          *
          * Prints the function signature, then the indented body, then a terminating `end`
          *
          * @param f
          *   Function AST to print
          */
        def emitFunc(f: Func[A]): Unit = {
            val ps = f.params
                .map { p =>
                    s"${showSynType(p.typ)} ${ident(p.ident)}"
                }
                .mkString(", ")

            emitLine(
                s"${showSynType(f.typ)} ${ident(f.ident)}($ps) is"
            )
            indent {
                f.stmt match {
                    case Stmt.Block(stmts, _) =>
                        emitStmtList(stmts)
                    case other =>
                        emitStmt(other)
                }
            }
            emitLine("end")
        }

        private def isSimpleOneLine(s: Stmt[A]): Boolean =
            s match
                case Stmt.Block(_, _)        => false
                case Stmt.If(_, _, _, _) => false
                case Stmt.While(_, _, _) => false
                case _                                     => true

        /** emit a list of statements (delimited with ";") */
        def emitStmtList(stmts: List[Stmt[A]]): Unit =
            stmts.zipWithIndex.foreach { (s, i) =>
                val isLast = i == stmts.length - 1
                // if it's the last statement don't add ';'
                if isLast then emitStmt(s)
                // if it is simple one line statement then emit it with ';'
                else if isSimpleOneLine(s) then emitLine(stmt1(s) + ";")
                // if it is a multiline statement (e.g. block, if, while, see above), let it handle
                else
                    emitStmtWithTrailingSemi(s)
            }

        /** emit multi line statements with semicolon on closing */
        private def emitStmtWithTrailingSemi(stmt: Stmt[A]): Unit =
            stmt match
                case Stmt.Block(stmts, _) =>
                    emitLine("begin")
                    indent {
                        emitStmtList(stmts)
                    }
                    emitLine("end;")

                case Stmt.If(cond, ifB, elseB, _) =>
                    emitLine(s"if ${expr(cond)} then")
                    indent {
                        ifB match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("else")
                    indent {
                        elseB match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("fi;")

                case Stmt.While(cond, body, _) =>
                    emitLine(s"while ${expr(cond)} do")
                    indent {
                        body match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("done;")

                case other => emitLine(stmt1(other) + ";") 

        /** emit any statement */
        def emitStmt(stmt: Stmt[A]): Unit =
            stmt match
                case Stmt.Block(stmts, _) =>
                    emitLine("begin")
                    indent {
                        emitStmtList(stmts)
                    }
                    emitLine("end")

                case Stmt.If(cond, ifB, elseB, _) =>
                    emitLine(s"if ${expr(cond)} then")
                    indent {
                        ifB match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("else")
                    indent {
                        elseB match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("fi")

                case Stmt.While(cond, body, _) =>
                    emitLine(s"while ${expr(cond)} do")
                    indent {
                        body match {
                            case Stmt.Block(stmts, _) => emitStmtList(stmts)
                            case other => emitStmt(other)
                        }
                    }
                    emitLine("done")

                case other =>
                    emitLine(stmt1(other))

        /** display single line statements */
        private def stmt1(stmt: Stmt[A]): String =
            stmt match
                case Stmt.Skip(_) => "skip"
                case Stmt.Decl(ty, id, rv, _) => s"${showSynType(ty)} ${ident(id)} = ${rvalue(rv)}"
                case Stmt.Asgn(lv, rv, _) => s"${lvalue(lv)} = ${rvalue(rv)}"
                case Stmt.Read(lv, _) => s"read ${lvalue(lv)}"
                case Stmt.Free(e, _) => s"free ${expr(e)}"
                case Stmt.Return(e, _) => s"return ${expr(e)}"
                case Stmt.Exit(e, _) => s"exit ${expr(e)}"
                case Stmt.Print(e, _) => s"print ${expr(e)}"
                case Stmt.Println(e, _) => s"println ${expr(e)}"

                // multi-line statements are handled are handled above
                case _ => ""

        /** render an identifier as it should appear in source */
        def ident(id: Ident[A]): String = id.name

        /** Render an lvalue in source text
          *
          * @param lv
          *   Lvalue AST node
          * @return
          *   Source string for the l-valu.
          */
        def lvalue(lv: LValue[A]): String =
            lv match
                case i: Ident[A] => ident(i)
                case ArrayElem(id, idxs, _) =>
                    val base = ident(id)
                    idxs.foldLeft(base)((acc, i) => s"$acc[${expr(i)}]")
                case PairElem(isFst, inner, _) =>
                    val kw = if isFst then PairPos.Fst.outputFormat else PairPos.Snd.outputFormat
                    s"$kw ${lvalue(inner)}"

        /** Render an rvalue in source text
          *
          * @param rv
          *   Rvalue AST node
          * @return
          *   Source string for the rvalue
          */
        def rvalue(rv: RValue[A]): String =
            rv match
                case e: Expr[A] => expr(e)
                case ArrayLiter(elems, _) =>
                    elems.map(expr).mkString("[", ", ", "]")
                case NewPair(e1, e2, _) =>
                    s"newpair(${expr(e1)}, ${expr(e2)})"
                case Call(id, args, _) =>
                    val as = args.map(expr).mkString(", ")
                    s"call ${ident(id)}($as)"
                case PairElem(isFst, inner, _) =>
                    val kw = if isFst then PairPos.Fst.outputFormat else PairPos.Snd.outputFormat
                    s"$kw ${lvalue(inner)}"

        /** Render an expression in source text
          *
          * @param e
          *   Expression AST node
          * @return
          *   Source string for the expression
          */
        def expr(e: Expr[A]): String =
            e match
                case i: Ident[A]         => ident(i)
                case a: ArrayElem[A] => lvalue(a)
                case UnaryExpr(op, x, _) =>
                    val o = showUnary(op)
                    op match
                        case UnaryOperator.Len | UnaryOperator.Ord | UnaryOperator.Chr =>
                            s"$o ${atomOrNested(x)}"
                        case UnaryOperator.Neg | UnaryOperator.Not =>
                            s"$o${atomOrNested(x)}"

                case BinaryExpr(op, l, r, _) =>
                    s"${atomOrNested(l)} ${showBinary(op)} ${atomOrNested(r)}"

                case IntLiter(v, _)    => v.toString
                case BoolLiter(v, _) => v.toString
                case CharLiter(v, _) => s"'${escapeChar(v)}'"
                case StrLiter(v, _)    => s"\"${escapeString(v)}\""
                case PairLiter(_)        => "null"

        /** render an expression, adding parentheses when it is not an atomic form
          *
          * @param e
          *   Expression to render
          * @return
          *   `expr(e)` if atomic otherwise the parenthesised rendering
          */
        private def atomOrNested(e: Expr[A]): String =
            e match
                // atomic cases
                case _: IntLiter[A] | _: BoolLiter[A] | _: CharLiter[A] | _: StrLiter[A] |
                     _: PairLiter[A] | _: Ident[A] | _: ArrayElem[A] =>
                    expr(e)
                case _ =>
                    s"(${expr(e)})"
    }

    /** Convert a syntactic type into string
      *
      * @param t
      *   Syntactic type
      * @return
      *   Source string for the type
      */
    private def showSynType(t: SyntaxType): String = t match
        case SyntaxType.Int => "int"
        case SyntaxType.Bool => "bool"
        case SyntaxType.Char => "char"
        case SyntaxType.String => "string"
        case SyntaxType.ErasedPair => "pair"
        case SyntaxType.Pair(f, s) => s"pair(${showSynType(f)}, ${showSynType(s)})"
        case SyntaxType.Array(a) => s"${showSynType(a)}[]"

    /** Convert a binary operator into source string
      *
      * @param op
      *   Binary operator
      * @return
      *   The operator
      */
    private def showBinary(op: BinaryOperator): String = op match
        case BinaryOperator.Add => "+"
        case BinaryOperator.Sub => "-"
        case BinaryOperator.Mul => "*"
        case BinaryOperator.Div => "/"
        case BinaryOperator.Mod => "%"
        case BinaryOperator.GT => ">"
        case BinaryOperator.GTE => ">="
        case BinaryOperator.LT => "<"
        case BinaryOperator.LTE => "<="
        case BinaryOperator.EQ => "=="
        case BinaryOperator.NEQ => "!="
        case BinaryOperator.And => "&&"
        case BinaryOperator.Or => "||"

    /** Convert a unary operator into source string
      *
      * @param op
      *   Unary operator
      * @return
      *   The operator
      */
    private def showUnary(op: UnaryOperator): String = op match
        case UnaryOperator.Neg => "-"
        case UnaryOperator.Not => "!"
        case UnaryOperator.Len => "len"
        case UnaryOperator.Ord => "ord"
        case UnaryOperator.Chr => "chr"

    
    private def escapeChar(c: Char): String =
        c match
            case '\u0000' => "\\0"
            case '\b' => "\\b"
            case '\t' => "\\t"
            case '\n'=> "\\n"
            case '\f'=> "\\f"
            case '\r'=> "\\r"
            case '\"'=> "\\\""
            case '\''=> "\\'"
            case '\\'=> "\\\\"
            case other=> other.toString

    /** escape a string */
    private def escapeString(s: String): String =
        s.flatMap(escapeChar)
}
