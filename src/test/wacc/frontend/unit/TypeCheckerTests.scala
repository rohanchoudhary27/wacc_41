package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.{parser, Renamer, TypeChecker, SemanticErrorLogger}

class TypeCheckerTests extends AnyFlatSpec with Matchers {

    "The type checker for read statements" should "accept read with int variable" in {
      val input = "begin int x = 0; read x end"
      val parseResult = parser.parse(input)
      parseResult.isRight shouldBe true
      
      val program = parseResult.toOption.get
      val errorLogger = SemanticErrorLogger()
      val renamedProgram = Renamer.rename(program, errorLogger)
      
      errorLogger.hasErrors() shouldBe false
      
      val funcTable = renamedProgram.info match {
        case wacc.SemanticData.TopLevel(ft) => ft
        case _ => Map.empty
      }
      val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
      
      errorLogger.hasErrors() shouldBe false
      typeCheckResult.isDefined shouldBe true
    }

    it should "accept read with char variable" in {
      val input = "begin char c = 'a'; read c end"
      val parseResult = parser.parse(input)
      parseResult.isRight shouldBe true
      
      val program = parseResult.toOption.get
      val errorLogger = SemanticErrorLogger()
      val renamedProgram = Renamer.rename(program, errorLogger)
      
      errorLogger.hasErrors() shouldBe false
      
      val funcTable = renamedProgram.info match {
        case wacc.SemanticData.TopLevel(ft) => ft
        case _ => Map.empty
      }
      val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
      
      errorLogger.hasErrors() shouldBe false
      typeCheckResult.isDefined shouldBe true
    }

    it should "reject read with bool variable" in {
      val input = "begin bool b = true; read b end"
      val parseResult = parser.parse(input)
      parseResult.isRight shouldBe true
      
      val program = parseResult.toOption.get
      val errorLogger = SemanticErrorLogger()
      val renamedProgram = Renamer.rename(program, errorLogger)
      
      errorLogger.hasErrors() shouldBe false
      
      val funcTable = renamedProgram.info match {
        case wacc.SemanticData.TopLevel(ft) => ft
        case _ => Map.empty
      }
      val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
      
      errorLogger.hasErrors() shouldBe true
      val errors = errorLogger.retrieveAllErrors()
      errors should include ("Type Mismatch")
    }

    it should "reject read with string variable" in {
      val input = "begin string s = \"hello\"; read s end"
      val parseResult = parser.parse(input)
      parseResult.isRight shouldBe true
      
      val program = parseResult.toOption.get
      val errorLogger = SemanticErrorLogger()
      val renamedProgram = Renamer.rename(program, errorLogger)
      
      errorLogger.hasErrors() shouldBe false
      
      val funcTable = renamedProgram.info match {
        case wacc.SemanticData.TopLevel(ft) => ft
        case _ => Map.empty
      }
      val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
      
      errorLogger.hasErrors() shouldBe true
      val errors = errorLogger.retrieveAllErrors()
      errors should include ("Type Mismatch")
    }

}
