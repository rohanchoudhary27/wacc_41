package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc._

class TypeCheckerTests extends AnyFlatSpec with Matchers {

    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos

    "The type checker for read statements" should "accept read with int variable" in {
        val input = "begin int x = 0; read x end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
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
        val errorLogger = ErrorLogger()
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
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }
        val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type mismatch")
    }

    it should "reject read with string variable" in {
        val input = "begin string s = \"hello\"; read s end"
        val parseResult = parser.parse(input)
        parseResult.isRight shouldBe true
        
        val program = parseResult.toOption.get
        val errorLogger = ErrorLogger()
        val renamedProgram = Renamer.rename(program, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val funcTable = renamedProgram.info match {
            case wacc.SemanticData.TopLevel(ft) => ft
            case _ => Map.empty
        }
        val typeCheckResult = TypeChecker.check(renamedProgram, errorLogger, funcTable)
        
        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        errors should include ("Type mismatch")
    }

    "Type-checking functions" should "correctly type-check a valid function call" in {

        val funcSignature: SemanticData.Func = SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val callExpr =
            Call(Ident("f", funcSignature), List(IntLiter(10, SemanticData.Expr(synPos.pos, KnownType.Int))), semPos)
        val decl =
            Stmt.Decl(SyntaxType.Int, Ident("y", SemanticData.Var(synPos.pos, 1, KnownType.Int)), callExpr, semPos)

        val inputAST = Program(List(), Stmt.Block(List(decl), semPos), SemanticData.TopLevel(funcTable))
        val errorLogger = new ErrorLogger()

        TypeChecker.check(inputAST, errorLogger, funcTable)

        if (errorLogger.hasErrors()) fail(errorLogger.retrieveAllErrors())
    }
}
