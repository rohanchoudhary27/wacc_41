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
}
