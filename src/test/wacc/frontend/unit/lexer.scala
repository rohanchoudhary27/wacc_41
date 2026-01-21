import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success}

import wacc.lexer

class Lexer extends AnyFlatSpec {

    "The integer lexer" should "should be able to parse positive numbers" in {
        lexer.integer.parse("42") shouldBe Success(42)
    }

    it should "be able to parse negative numbers" in {
        lexer.integer.parse("-2") shouldBe Success(-2)
    }

    "The char lexer" should "be able to parse standard characters" in {
        lexer.char.parse("'a'") shouldBe Success('a')
        lexer.char.parse("'3'") shouldBe Success('3')
        lexer.char.parse("';'") shouldBe Success(';')
    }

    it should "be able to parse escape characters" in {
        lexer.char.parse("'\\n'") shouldBe Success('\n')
        lexer.char.parse("'\\\"'") shouldBe Success('\"')
        lexer.char.parse("'\\\\'") shouldBe Success('\\')
    }

    it should "fail on invalid character literals" in {
        lexer.char.parse("''").isFailure shouldBe true // too long
        lexer.char.parse("'ab'").isFailure shouldBe true // multiple chars
        lexer.char.parse("a").isFailure shouldBe true // string
        lexer.char.parse("\"a\"").isFailure shouldBe true // wrong quotes
    }

    "The boolean lexer" should "parse correctly formatted boolean values" in {
        lexer.boolLiter.parse("true") shouldBe Success(true)
        lexer.boolLiter.parse("false") shouldBe Success(false)
    }

    it should "fail on uppercase boolean attempts" in {
        lexer.boolLiter.parse("True").isFailure shouldBe true
        lexer.boolLiter.parse("False").isFailure shouldBe true
        lexer.boolLiter.parse("TRUE").isFailure shouldBe true
        lexer.boolLiter.parse("FALSE").isFailure shouldBe true
    }

    "The string lexer" should "parse plain strings" in {
        lexer.string.parse("\"\"") shouldBe Success("")
        lexer.string.parse("\"hello\"") shouldBe Success("hello")
        lexer.string.parse("\"123abc45\"") shouldBe Success("123abc45")
    }

    it should "parse strings with escape chars" in {
        lexer.string.parse("\"\\n\\t\\r\"") shouldBe Success("\n\t\r")
        lexer.string.parse("\"\\\"hello\\\"\"") shouldBe Success("\"hello\"")
        lexer.string.parse("\"back\\\\slash\"") shouldBe Success("back\\slash")
    }

    it should "fail on unclosed strings" in {
        lexer.string.parse("\"hello").isFailure shouldBe true
    }

    "The identifier lexer" should "parse valid identifiers" in {
        lexer.ident.parse("x") shouldBe Success("x")
        lexer.ident.parse("var_name") shouldBe Success("var_name")
        lexer.ident.parse("_underscore") shouldBe Success("_underscore")
        lexer.ident.parse("var123") shouldBe Success("var123")
    }

    it should "reject WACC keywords" in {
        lexer.ident.parse("begin").isFailure shouldBe true
        lexer.ident.parse("len").isFailure shouldBe true
        lexer.ident.parse("do").isFailure shouldBe true
    }

    it should "fail on invalid variable names" in {
        lexer.ident.parse("123var").isFailure shouldBe true
        lexer.ident.parse(".32hi").isFailure shouldBe true
    }

    "The null lexer" should "parse null" in {
        lexer.nullLiter.parse("null") shouldBe Success(())
    }
}
