package wacc.frontend.generator

import org.scalacheck.Shrink, Shrink.shrink
import scala.annotation.nowarn
import wacc.frontend._

/** Shrinks a list but never produces the empty list (otherwise its a syntax error) */
@nowarn def shrinkNonEmptyList[A](xs: List[A])(using s: Shrink[A]): Stream[List[A]] =
    shrink(xs).filter(_.nonEmpty)

given Shrink[Expr[SyntacticData]] = Shrink(exprShrink)


/** Shrinker for expressions that reduces literals and
  * recursively shrinks sub-expressions
  */
@nowarn def exprShrink(e: Expr[SyntacticData]): Stream[Expr[SyntacticData]]  = e match {
    case IntLiter(n, pos) =>
        shrink(n).map(IntLiter(_, pos))

    case StrLiter(s, pos) =>
        shrink(s).map(StrLiter(_, pos))

    case UnaryExpr(op, expr, pos) =>
        expr #:: shrink(expr).map(UnaryExpr(op, _, pos))

    case BinaryExpr(op, left, right, pos) =>
        left #:: right #:: shrink((left, right)).map { case (l, r) =>
            BinaryExpr(op, l, r, pos)
        }

    case ArrayElem(ident, indices, pos) =>
        shrinkNonEmptyList(indices).map(ArrayElem(ident, _, pos))

    case _: (BoolLiter[SyntacticData] | CharLiter[SyntacticData]
           | PairLiter[SyntacticData] | Ident[SyntacticData]) =>
        Stream.empty
}

given Shrink[LValue[SyntacticData]] = Shrink(lvalueShrink)


/** Shrinker for lvalues that simplifies nested structures
  * (array accesses, pair elements) but keeps identifiers atomic
  */
@nowarn def lvalueShrink(lv: LValue[SyntacticData]): Stream[LValue[SyntacticData]] = lv match {
    case _: Ident[SyntacticData] =>
        Stream.empty

    case ArrayElem(ident, indices, pos) =>
        (ident: LValue[SyntacticData]) #:: shrinkNonEmptyList(indices).map(ArrayElem(ident, _, pos))

    case PairElem(isFst, inner, pos) =>
        inner #:: shrink(inner).map(PairElem(isFst, _, pos))
}

given Shrink[RValue[SyntacticData]] = Shrink(rvalueShrink)


/** Shrinker for rvalues */
@nowarn def rvalueShrink(rv: RValue[SyntacticData]): Stream[RValue[SyntacticData]] = rv match {
    case e: Expr[SyntacticData] =>
        shrink(e).map(x => x: RValue[SyntacticData])

    case ArrayLiter(elems, pos) =>
        shrink(elems).map(ArrayLiter(_, pos))

    case NewPair(e1, e2, pos) =>
        shrink((e1, e2)).map { case (a, b) => NewPair(a, b, pos) }

    case Call(ident, args, pos) =>
        shrink(args).map(Call(ident, _, pos))

    case PairElem(isFst, inner, pos) =>
        shrink(inner).map(PairElem(isFst, _, pos))
}

given Shrink[Stmt[SyntacticData]] = Shrink(stmtShrink)


/** Shrinks a block body without unwrapping (if/while bodies must stay Blocks) */
@nowarn def shrinkBlockBody(s: Stmt[SyntacticData]): Stream[Stmt[SyntacticData]] = s match {
    case Stmt.Block(stmts, pos) =>
        shrinkNonEmptyList(stmts).map(Stmt.Block(_, pos))
    case _ =>
        Stream.empty
}

/** Shrinker for statements that reduces expressions, sub blocks,
  * and control flow components
  */
@nowarn def stmtShrink(s: Stmt[SyntacticData]): Stream[Stmt[SyntacticData]] = s match {
    case _: Stmt.Skip[SyntacticData] =>
        Stream.empty

    case Stmt.Decl(typ, ident, rval, pos) =>
        shrink(rval).map(Stmt.Decl(typ, ident, _, pos))

    case Stmt.Asgn(lval, rval, pos) =>
        shrink((lval, rval)).map { case (l, r) => Stmt.Asgn(l, r, pos) }

    case Stmt.Read(lval, pos) =>
        shrink(lval).map(Stmt.Read(_, pos))

    case Stmt.Free(expr, pos) =>
        shrink(expr).map(Stmt.Free(_, pos))

    case Stmt.Return(expr, pos) =>
        shrink(expr).map(Stmt.Return(_, pos))

    case Stmt.Exit(expr, pos) =>
        shrink(expr).map(Stmt.Exit(_, pos))

    case Stmt.Print(expr, pos) =>
        shrink(expr).map(Stmt.Print(_, pos))

    case Stmt.Println(expr, pos) =>
        shrink(expr).map(Stmt.Println(_, pos))

    case Stmt.If(cond, ifBlock, elseBlock, pos) =>
        ifBlock #:: elseBlock #::
        shrink(cond).map(Stmt.If(_, ifBlock, elseBlock, pos)) #:::
        shrinkBlockBody(ifBlock).map(Stmt.If(cond, _, elseBlock, pos)) #:::
        shrinkBlockBody(elseBlock).map(Stmt.If(cond, ifBlock, _, pos))

    case Stmt.While(cond, body, pos) =>
        body #::
        shrink(cond).map(Stmt.While(_, body, pos)) #:::
        shrinkBlockBody(body).map(Stmt.While(cond, _, pos))

    case Stmt.Block(stmts, pos) =>
        shrinkNonEmptyList(stmts).map(Stmt.Block(_, pos))
}

given Shrink[Func[SyntacticData]] = Shrink(funcShrink)

/** Shrinker for functions that shrinks parameter lists
  * and simplifies the function body (body must stay a Block)
  */
@nowarn def funcShrink(f: Func[SyntacticData]): Stream[Func[SyntacticData]] = {
    val Func(typ, ident, params, body, pos) = f
    val shrunkParams = shrink(params).map(Func(typ, ident, _, body, pos))
    val shrunkBody = shrinkBlockBody(body).map(Func(typ, ident, params, _, pos))
    shrunkParams #::: shrunkBody
}

given Shrink[Program[SyntacticData]] = Shrink(programShrink)

/** Shrinker for whole programs that reduces the set of functions
  * and simplifies the main statement block (main body must stay a Block)
  */
@nowarn def programShrink(p: Program[SyntacticData]): Stream[Program[SyntacticData]] = {
    val Program(funcs, stmt, pos) = p
    val shrunkFuncs = shrink(funcs).map(Program(_, stmt, pos))
    val shrunkBody = shrinkBlockBody(stmt).map(Program(funcs, _, pos))
    shrunkFuncs #::: shrunkBody
}
