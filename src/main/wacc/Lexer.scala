package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.token.Basic
import parsley.token.descriptions.ExponentDesc.NoExponents
import parsley.token.errors.{ErrorConfig, Label, LabelConfig, LabelWithExplainConfig, Hidden}
import parsley.token.Unicode

/**
* The Parsley lexer, describing language structure and creating the base case
* primitive parsers.
*/
object lexer {

    /* Reserved keywords */
    private val keywords = Set(
        "begin", "end", "is", "skip", "read", "free", "return",
        "exit", "print", "println", "if", "then", "else",
        "fi", "while", "do", "done", "newpair", "call",
        "fst", "snd", "int", "bool", "char", "string",
        "pair", "len", "ord", "chr", "true", "false", "null"
    )

    /* Delimiters */
    private val delimiters = Set("[", "]", "(", ")", ",", ";")

    private val desc = LexicalDesc.plain.copy(

        /* Identifier rules */
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
        ),

        /* Reserved keywords and operators */
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = keywords,
            hardOperators = Set(
              "!", "-", "*", "/", "%", "+",
              "-", ">", ">=", "<", "<=", "==", "!=", "&&", "||",
              "=", "[", "]", "(", ")", ",", ";"
            ),
        ),

        /* Description of WACC number features */
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

        /* Description of WACC string/char features */
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
            graphicCharacter = Unicode(c =>
                /* All printable ASCII except \, ', and ", which must be escaped */
                c >= ' '.toInt && c != '\''.toInt && c != '\"'.toInt && c != '\\'.toInt
            ),
        ),

        /* Description of WACC comments and whitespace */
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),
    )

    private val errorConfig = new ErrorConfig {

        /* Hide whitespace and comments from error messages to reduce noise */
        override def labelSpaceEndOfLineComment: LabelConfig = Hidden
        
        /* Label operators, delimiters, and keywords with meaningful names */
        override def labelSymbol = {
            val operators = Map(
                /* Arithmetic operators */
                "+" -> Label("addition"),
                "-" -> Label("subtraction"),
                "*" -> Label("multiplication"),
                "/" -> Label("division"),
                "%" -> Label("modulo"),
                
                /* Comparison operators */
                ">"  -> Label("greater than"),
                ">=" -> Label("greater than or equal"),
                "<"  -> Label("less than"),
                "<=" -> Label("less than or equal"),
                "==" -> Label("equality"),
                "!=" -> Label("inequality"),
                
                /* Logical operators */
                "&&" -> Label("logical AND"),
                "||" -> Label("logical OR"),
                "!"  -> Label("logical NOT"),
                
                /* Assignment */
                "=" -> Label("assignment")
            )
            
            /* Generate labels for delimiters and keywords, which are shown as-is */
            val delimAndKeywordLabels = (delimiters ++ keywords).map(s => s -> Label(s)).toMap
            
            operators ++ delimAndKeywordLabels
        }
        
        /* Label numeric literals */
        override def labelIntegerUnsignedDecimal: LabelConfig = Label("integer literal")
        
        /* Label text literals */
        override def labelCharUtf16: LabelWithExplainConfig = Label("character literal")
        override def labelStringUtf16(multi: Boolean, raw: Boolean): LabelWithExplainConfig =
            Label("string literal")
        
        /* Label identifiers and operators */
        override def labelNameIdentifier: String = "identifier"
        override def labelNameOperator: String = "operator"
    }

    private val lexer = Lexer(desc, errorConfig)

    /* Primitive parsers for use in parser.scala */
    val integer = lexer.lexeme.integer.decimal32
    val implicits = lexer.lexeme.symbol.implicits
    val ident = lexer.lexeme.names.identifier
    val string = lexer.lexeme.string.fullUtf16
    val char = lexer.lexeme.character.fullUtf16
    val nullLiter = lexer.lexeme.symbol("null").void
    val boolLiter =
      lexer.lexeme.symbol("true") #> true |
      lexer.lexeme.symbol("false") #> false

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    def brackets[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.brackets(p)
    def parens[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.parens(p)
}
