package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.token.Basic
import parsley.token.descriptions.ExponentDesc.NoExponents
import parsley.token.errors.{ErrorConfig, Label, LabelConfig, LabelWithExplainConfig}

/**
* The Parsley lexer, describing language structure and creating the base case
* primitive parsers.
*/
object lexer {

    private val desc = LexicalDesc.plain.copy(

        /* Identifier rules */
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
        ),

        /* Reserved keywords and operators */
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
        ),

        /* Description of WACC comments and whitespace */
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),
    )

    private val errorConfig = new ErrorConfig {

        /* Label operators, delimiters, and keywords with meaningful names */
        override def labelSymbol = Map(

            /* Arithmetic operators */
            "+" -> Label("addition operator"),
            "-" -> Label("subtraction operator"),
            "*" -> Label("multiplication operator"),
            "/" -> Label("division operator"),
            "%" -> Label("modulo operator"),
            
            /* Comparison operators */
            ">"  -> Label("greater than operator"),
            ">=" -> Label("greater than or equal operator"),
            "<"  -> Label("less than operator"),
            "<=" -> Label("less than or equal operator"),
            "==" -> Label("equality operator"),
            "!=" -> Label("inequality operator"),
            
            /* Logical operators */
            "&&" -> Label("logical AND operator"),
            "||" -> Label("logical OR operator"),
            "!"  -> Label("logical NOT operator"),
            
            /* Assignment and delimiters */
            "=" -> Label("assignment operator"),
            "[" -> Label("opening bracket"),
            "]" -> Label("closing bracket"),
            "(" -> Label("opening parenthesis"),
            ")" -> Label("closing parenthesis"),
            "," -> Label("comma"),
            ";" -> Label("semicolon"),
            
            /* Keywords */
            "begin"   -> Label("'begin' keyword"),
            "end"     -> Label("'end' keyword"),
            "skip"    -> Label("'skip' statement"),
            "read"    -> Label("'read' statement"),
            "free"    -> Label("'free' statement"),
            "return"  -> Label("'return' statement"),
            "exit"    -> Label("'exit' statement"),
            "print"   -> Label("'print' statement"),
            "println" -> Label("'println' statement"),
            "if"      -> Label("'if' keyword"),
            "then"    -> Label("'then' keyword"),
            "else"    -> Label("'else' keyword"),
            "fi"      -> Label("'fi' keyword"),
            "while"   -> Label("'while' keyword"),
            "do"      -> Label("'do' keyword"),
            "done"    -> Label("'done' keyword"),
            "newpair" -> Label("'newpair' keyword"),
            "call"    -> Label("'call' keyword"),
            "fst"     -> Label("'fst' keyword"),
            "snd"     -> Label("'snd' keyword"),
            "int"     -> Label("'int' type"),
            "bool"    -> Label("'bool' type"),
            "char"    -> Label("'char' type"),
            "string"  -> Label("'string' type"),
            "pair"    -> Label("'pair' type"),
            "len"     -> Label("'len' operator"),
            "ord"     -> Label("'ord' operator"),
            "chr"     -> Label("'chr' operator"),
            "true"    -> Label("boolean literal 'true'"),
            "false"   -> Label("boolean literal 'false'"),
            "null"    -> Label("null literal")
        )
        
        /* Label numeric literals */
        override def labelIntegerUnsignedDecimal: LabelConfig = Label("integer literal")
        
        /* Label text literals */
        override def labelCharUtf16: LabelWithExplainConfig = Label("character literal")
        override def labelStringUtf16(multi: Boolean, raw: Boolean): LabelWithExplainConfig = Label("string literal")
        
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
}
