package wacc.frontend.generator

import wacc.*

final case class PrettyConfig(
    indentSize: Int = 4
)

object Pretty {

    def program[A](p: Program[A]): String =
        programWith(PrettyConfig())(p)

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

    private final class Builder[A](cfg: PrettyConfig) {
        private val sb = new StringBuilder
        private var ind: Int = 0

        def result(): String = sb.toString

        private def pad: String = " " * (ind * cfg.indentSize)

        def emitLine(s: String): Unit =
            sb.append(pad).append(s).append('\n')

        def indent[T](body: => T): T =
            ind += 1
            try body
            finally ind -= 1

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
                emitStmt(f.stmt)
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
                    indent { emitStmt(ifB) }
                    emitLine("else")
                    indent { emitStmt(elseB) }
                    emitLine("fi;")

                case Stmt.While(cond, body, _) =>
                    emitLine(s"while ${expr(cond)} do")
                    indent { emitStmt(body) }
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
                    emitLine(s"if ${expr(cond)} then}")
                    indent {
                        emitStmt(ifB)
                    }
                    emitLine("else")
                    indent {
                        emitStmt(elseB)
                    }
                    emitLine("fi")

                case Stmt.While(cond, body, _) =>
                    emitLine(s"while ${expr(cond)} do")
                    indent {
                        emitStmt(body)
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

        def ident(id: Ident[A]): String = id.name

        def lvalue(lv: LValue[A]): String =
            lv match
                case i: Ident[A] => ident(i)
                case ArrayElem(id, idxs, _) =>
                    val base = ident(id)
                    idxs.foldLeft(base)((acc, i) => s"$acc[${expr(i)}]")
                case PairElem(isFst, inner, _) =>
                    val kw = if isFst then "fst" else "snd"
                    s"$kw ${lvalue(inner)}"

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
                    val kw = if isFst then "fst" else "snd"
                    s"$kw ${lvalue(inner)}"

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

        private def atomOrNested(e: Expr[A]): String =
            e match
                case _: IntLiter[A] | _: BoolLiter[A] | _: CharLiter[A] | _: StrLiter[A] |
                     _: PairLiter[A] | _: Ident[A] | _: ArrayElem[A] =>
                    expr(e)
                case _ =>
                    s"(${expr(e)})"
    }

    private def showSynType(t: SyntaxType): String = t match
        case SyntaxType.Int => "int"
        case SyntaxType.Bool => "bool"
        case SyntaxType.Char => "char"
        case SyntaxType.String => "string"
        case SyntaxType.ErasedPair => "pair"
        case SyntaxType.Pair(f, s) => s"pair(${showSynType(f)}, ${showSynType(s)})"
        case SyntaxType.Array(a) => s"${showSynType(a)}[]"

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

    private def escapeString(s: String): String =
        s.flatMap(escapeChar)
}
