package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.token.Basic
import parsley.token.descriptions.ExponentDesc.NoExponents

object lexer {
    private val desc = LexicalDesc.plain.copy(

        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
        ),

        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set(
                "begin", "end", "is", "skip", "read", "free", "return",
                "exit", "print", "println", "if", "then", "else",
                "fi", "while", "do", "done", "newpair", "call",
                "fst", "snd", "int", "bool", "char", "string",
                "pair", "len", "ord", "chr", "true", "false", "null"
            ),

            hardOperators = Set(
              "!", "-", "len", "ord", "chr", "*", "/", "%", "+",
              "-", ">", ">=", "<", "<=", "==", "!=", "&&", "||",
              "=", "[", "]", "(", ")", ",", ";"
            ),
        ),

        numericDesc = NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
            hexadecimalLeads = Set(),
            octalLeads = Set(),
            binaryLeads = Set(),
            decimalExponentDesc = NoExponents,
            hexadecimalExponentDesc = NoExponents,
            octalExponentDesc = NoExponents,
            binaryExponentDesc = NoExponents,
        ),

        textDesc = TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = '\\',
                literals = Set('\'', '\"', '\\'),
                mapping = Map(
                    "0" -> 0x00, "b" -> 0x08, "t" -> 0x09,
                    "n" -> 0x0a, "f" -> 0x0c, "r" -> 0x0d
                )
            ),
            characterLiteralEnd = '\'',
        ),

        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),

    )

    private val lexer = Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
