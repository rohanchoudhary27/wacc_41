package wacc.frontend.generator

import org.scalacheck.Gen
import wacc.*

/**
 * AST Generator for randomised testing
 *
 * This generator produces syntactically valid ASTs.
 * Semantic concerns (including types, scopes, declarations) are ignored
 */
object AstGenerator {

    val maxDepth = 3 // default max recursion depth

    /* Generate a syntactic pos annotation */
    def genPos: Gen[SyntacticData] = for {
        line <- Gen.choose(1, 1000)
        col <- Gen.choose(1, 100)
    } yield SyntacticData.Pos((line, col))

    /* Generate a base type (int, bool, char, string) */
    def genBaseType: Gen[SyntaxType] =
        Gen.oneOf(SyntaxType.Int, SyntaxType.Bool, SyntaxType.Char, SyntaxType.String)

    /* Generate a pair element type (base, array, or erased pair, no nested pairs) */
    def genPairElemType: Gen[SyntaxType] = Gen.oneOf(
        genBaseType,
        genBaseType.map(SyntaxType.Array(_)),
        Gen.const(SyntaxType.ErasedPair)
    )

    /** Generate any syntax type */
    def genSyntaxType(depth: Int): Gen[SyntaxType] =
        if (depth <= 0) genBaseType
        else Gen.frequency(
            (4, genBaseType),
            (2, genSyntaxType(depth - 1).map(SyntaxType.Array(_))),
            (2, for {
                fst <- genPairElemType
                snd <- genPairElemType
            } yield SyntaxType.Pair(fst, snd))
        )

    // simple helper definition (depth = 2 is reasonable for most cases, tho we might make 
    // it random too)
    def genSimpleType: Gen[SyntaxType] = genSyntaxType(2)


    /* Generate an integer literal (within 32 bit range) */
    def genIntLiter: Gen[Expr[SyntacticData]] = for {
        value <- Gen.choose(Int.MinValue, Int.MaxValue)
        pos <- genPos
    } yield IntLiter(value, pos)

    /* Generate a boolean literal */
    def genBoolLiter: Gen[Expr[SyntacticData]] = for {
        value <- Gen.oneOf(true, false)
        pos <- genPos
    } yield BoolLiter(value, pos)

    /** Generate a valid WACC character (printable ascii) */
    def genValidChar: Gen[Char] = Gen.frequency(
        (10, Gen.choose('a', 'z')),
        (10, Gen.choose('A', 'Z')),
        (5, Gen.choose('0', '9')),
        (4, Gen.oneOf(' ', '_', '-', '+', '=', '.', ',', '!', '?', '@', '#', '$',
            '%', '^', '&', '*', '(', ')', '[', ']', '{', '}', '<', '>', '/',
            '|', '~', '`', ':', ';'))
    )

    /** Generate a character literal */
    def genCharLiter: Gen[Expr[SyntacticData]] = for {
        value <- genValidChar
        pos <- genPos
    } yield CharLiter(value, pos)

    /** Generate a string literal */
    def genStrLiter: Gen[Expr[SyntacticData]] = for {
        len <- Gen.choose(0, 20) // 20 is max str literal length
        chars <- Gen.listOfN(len, genValidChar)
        pos <- genPos
    } yield StrLiter(chars.mkString, pos)

    /** Generate a null pair literal */
    def genPairLiter: Gen[Expr[SyntacticData]] = for {
        pos <- genPos
    } yield PairLiter(pos)

    /** Generate a random identifier name */
    def genIdentName: Gen[String] = for {
        prefix <- Gen.oneOf("x", "y", "z", "a", "b", "var", "tmp", "arr", "p", "data")
        suffix <- Gen.choose(0, 99)
    } yield s"${prefix}_$suffix"

    /** Generate an identifier */
    def genIdent: Gen[Ident[SyntacticData]] = for {
        name <- genIdentName
        pos <- genPos
    } yield Ident(name, pos)

    /** Generate any expression */
    def genExpr(depth: Int): Gen[Expr[SyntacticData]] = {
        val identGen = genIdent.map(id => id: Expr[SyntacticData])

        /* (leaves) */
        if (depth <= 0) Gen.oneOf(
            genIntLiter, genBoolLiter, genCharLiter, genStrLiter, genPairLiter, identGen
        )
        else Gen.frequency(
            (2, genIntLiter),
            (1, genBoolLiter),
            (1, genCharLiter),
            (1, genStrLiter),
            (1, genPairLiter),
            (2, identGen),
            (2, genUnaryExpr(depth - 1)),
            (2, genBinaryExpr(depth - 1)),
            (1, genArrayElem(depth - 1).map(ae => ae: Expr[SyntacticData]))
        )
    }

    /** Generate a unary expression */
    def genUnaryExpr(depth: Int): Gen[Expr[SyntacticData]] = for {
        op <- Gen.oneOf(UnaryOperator.values.toSeq)
        expr <- genExpr(depth)
        pos <- genPos
    } yield UnaryExpr(op, expr, pos)

    /** Generate a binary expression */
    def genBinaryExpr(depth: Int): Gen[Expr[SyntacticData]] = for {
        op <- Gen.oneOf(BinaryOperator.values.toSeq)
        left <- genExpr(depth)
        right <- genExpr(depth)
        pos <- genPos
    } yield BinaryExpr(op, left, right, pos)

    /** Generate an array element access */
    def genArrayElem(depth: Int): Gen[ArrayElem[SyntacticData]] = for {
        ident <- genIdent
        indexCount <- Gen.choose(1, 3)
        indices <- Gen.listOfN(indexCount, genExpr(depth))
        pos <- genPos
    } yield ArrayElem(ident, indices, pos)

    /** Generate any lvalue */
    def genLValue(depth: Int): Gen[LValue[SyntacticData]] = {
        if (depth <= 0) genIdent.map(id => id: LValue[SyntacticData]) // leaf case
        else Gen.frequency(
            (3, genIdent.map(id => id: LValue[SyntacticData])),
            (2, genArrayElem(depth - 1).map(ae => ae: LValue[SyntacticData])),
            (2, genPairElem(depth - 1).map(pe => pe: LValue[SyntacticData]))
        )
    }

    /** Generate any rvalue */
    def genRValue(depth: Int): Gen[RValue[SyntacticData]] = {
        if (depth <= 0) genExpr(depth).map(e => e: RValue[SyntacticData]) // leaf case here
         else Gen.frequency(
            (4, genExpr(depth).map(e => e: RValue[SyntacticData])),
            (2, genArrayLiter(depth - 1).map(al => al: RValue[SyntacticData])),
            (2, genNewPair(depth - 1).map(np => np: RValue[SyntacticData])),
            (2, genCall(depth - 1).map(c => c: RValue[SyntacticData])),
            (2, genPairElem(depth - 1).map(pe => pe: RValue[SyntacticData]))
        )
    }

    /** Generate an array literal */
    def genArrayLiter(depth: Int): Gen[ArrayLiter[SyntacticData]] = for {
        len <- Gen.choose(0, 4)
        elems <- Gen.listOfN(len, genExpr(depth))
        pos <- genPos
    } yield ArrayLiter(elems, pos)

    /** Generate a newpair expression */
    def genNewPair(depth: Int): Gen[NewPair[SyntacticData]] = for {
        fst <- genExpr(depth)
        snd <- genExpr(depth)
        pos <- genPos
    } yield NewPair(fst, snd, pos)

    /** Generate a function call */
    def genCall(depth: Int): Gen[Call[SyntacticData]] = for {
        ident <- genIdent
        argCount <- Gen.choose(0, 3)
        args <- Gen.listOfN(argCount, genExpr(depth))
        pos <- genPos
    } yield Call(ident, args, pos)

    /** Generate a pair element access */
    def genPairElem(depth: Int): Gen[PairElem[SyntacticData]] = for {
        isFst <- Gen.oneOf(true, false)
        lval <- genLValue(depth)
        pos <- genPos
    } yield PairElem(isFst, lval, pos)
}
