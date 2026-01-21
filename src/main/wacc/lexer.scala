package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.token.Basic
import parsley.token.descriptions.ExponentDesc.NoExponents
import parsley.token.errors.ErrorConfig

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
              "!", "-", "*", "/", "%", "+",
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

    // stub for now
    private val errorConfig = new ErrorConfig {}

    private val lexer = Lexer(desc, errorConfig)

    // Primitive parsers for use in parser.scala
    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    val ident = lexer.lexeme.names.identifier
    val string = lexer.lexeme.string.fullUtf16
    val char = lexer.lexeme.character.fullUtf16
    val nullLiter = lexer.lexeme.symbol("null").void
    val boolLiter =
      lexer.lexeme.symbol("true") #> true |
      lexer.lexeme.symbol("false") #> false

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
