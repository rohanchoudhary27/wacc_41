package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc._

/* Get the renamedID of a renamed variable */
def extractId(ident: Ident[SemanticData]): Option[Int] = ident.info match {
    case SemanticData.Var(_, id, _) => Some(id)
    case _ => None
}

class RenamerSpec extends AnyFlatSpec with Matchers {

    val pos = SyntacticData.Pos(0, 0)

    "The Renamer" should "assign different rename IDs to shadowed variables" in {

        val innerDecl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            IntLiter(0, pos),
            pos)

        val outerDecl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos), 
            IntLiter(0, pos),
            pos)

        val testBlock = Stmt.Block(
            List(
            outerDecl,
            Stmt.Block(List(innerDecl), pos)
            ), 
            pos
        )

        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val mainStmt = renamedAST.stmt

        val Stmt.Block(
            List(
            Stmt.Decl(_, outerIdent, _, _),
            Stmt.Block(
                List(Stmt.Decl(_, innerIdent, _, _)), _
            )
            ), 
            _
        ) = mainStmt: @unchecked

        val outerId = extractId(outerIdent)
        val innerId = extractId(innerIdent)

        if (outerId.isEmpty) {
            fail("Error in variable shadowing test - outer var not tagged with `Var` semantic data")
        }

        if (innerId.isEmpty) {
            fail("Error in variable shadowing test - inner var not tagged with `Var` semantic data")
        }

        outerId should not equal innerId
    }

    it should "not allow variables to be referenced before declaration" in {

        val decl2 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("var2", pos), 
            IntLiter(5, pos), 
            pos
        )

        val decl1 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("var1", pos), 
            Ident("var2", pos),
            pos
        )

        val testBlock = Stmt.Block(List(decl1, decl2), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger() 
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Variable 'var2' is not defined")
    }

    it should "not allow a variable in one branch of an If-Else be referenced by another" in {
        val cond = BinaryExpr(BinaryOperator.GT, IntLiter(2, pos), IntLiter(4, pos), pos)
        
        val ifBlock = Stmt.Block(List(
            Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(5, pos), pos)
        ), pos)

        val elseBlock = Stmt.Block(List(
            Stmt.Print(Ident("x", pos), pos)
        ), pos)

        val ifStmt = Stmt.If(cond, ifBlock, elseBlock, pos)

        val testBlock = Stmt.Block(List(ifStmt), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger() 
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Variable 'x' is not defined")
    }


    it should "suggest a similar variable name when a typo occurs in the same scope" in {

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("myVariable", pos), 
            IntLiter(5, pos), 
            pos
        )

        val printStmt = Stmt.Print(
            Ident("myVarble", pos), 
            pos
        )

        val testBlock = Stmt.Block(List(decl, printStmt), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger() 
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Variable 'myVarble' is not defined")
        errors should include ("Did you mean 'myVariable'?")
    }

    it should "suggest a similar variable name from a parent scope" in {

        val outerDecl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("jamie", pos), 
            IntLiter(10, pos), 
            pos
        )
        
        val innerPrint = Stmt.Print(
            Ident("jame", pos), 
            pos
        )
        val innerBlock = Stmt.Block(List(innerPrint), pos)

        val testBlock = Stmt.Block(List(outerDecl, innerBlock), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'jame' is not defined")
        errors should include ("Did you mean 'jamie'?")
    }

    it should "not suggest anything if no variable is similar enough" in {
        val decl = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", pos), 
            IntLiter(5, pos), 
            pos
        )

        val printStmt = Stmt.Print(
            Ident("konstantinos", pos), 
            pos
        )

        val testBlock = Stmt.Block(List(decl, printStmt), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'konstantinos' is not defined")
        errors should not include ("Did you mean")
    }

    it should "output the declaration site location on a redeclaration error" in {

        val decl1 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", SyntacticData.Pos(10, 5)),
            IntLiter(5, SyntacticData.Pos(10, 10)), 
            SyntacticData.Pos(10, 1)
        )

        val decl2 = Stmt.Decl(
            SyntaxType.Int, 
            Ident("number", SyntacticData.Pos(20, 5)),
            IntLiter(5, SyntacticData.Pos(20, 10)), 
            SyntacticData.Pos(20, 1)
        )

        val testBlock = Stmt.Block(List(decl1, decl2), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'number' is already declared")
        
        errors should include ("10") 
        errors should include ("1")
    }

    "The Renamer in for read" should "maintain the lvalue in a read statement after renaming" in {

        val decl = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            IntLiter(5, pos),
            pos)

        val readStmt = Stmt.Read(Ident("x", pos), pos)
        
        val testBlock = Stmt.Block(List(decl, readStmt), pos)
        val inputAST = Program(List[Func[SyntacticData]](), testBlock, pos)
        
        val errorLogger = ErrorLogger()
        val renamedAST = Renamer.rename(inputAST, errorLogger)
        
        errorLogger.hasErrors() shouldBe false
        
        val Stmt.Block(
            List(
                Stmt.Decl(_, declIdent, _, _),
                Stmt.Read(readIdent: Ident[SemanticData], _)
            ), 
            _
        ) = renamedAST.stmt: @unchecked
        
        val declId = extractId(declIdent)
        val readId = extractId(readIdent)
        
        declId.isDefined shouldBe true
        readId.isDefined shouldBe true
        declId shouldEqual readId
    }

    "Renaming functions" should "detect duplicate function definitions" in {

        val func1 = Func(SyntaxType.Int, Ident("f", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)
        
        val func2 = Func(SyntaxType.Int, Ident("f", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(1, pos), pos)), pos), pos)

        val inputAST = Program(List(func1, func2), Stmt.Skip(pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true

        val errors = errorLogger.retrieveAllErrors()
        errors should include ("'f' is already defined")
    }

    it should "let function parameters to be used in the function body" in {

        val paramX = Param(SyntaxType.Int, Ident("x", pos), pos)
        val returnStmt = Stmt.Return(Ident("x", pos), pos)
        
        val func = Func(SyntaxType.Int, Ident("f", pos), List(paramX), 
                       Stmt.Block(List(returnStmt), pos), pos)

        val inputAST = Program(List(func), Stmt.Skip(pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val renamedFunc = renamedAST.funcs.head
        val Stmt.Block(List(Stmt.Return(retIdent: Ident[SemanticData], _)), _) =
            renamedFunc.stmt: @unchecked
        val Param(_, paramIdent, _) = renamedFunc.params.head

        extractId(retIdent) shouldBe extractId(paramIdent)
    }

    it should "allow functions to call others defined later" in {
        
        val funcG = Func(SyntaxType.Int, Ident("g", pos), List(), 
                        Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)

        val callG = Call(Ident("g", pos), List(), pos)
        val bodyF = Stmt.Block(List(Stmt.Decl(SyntaxType.Int, Ident("u", pos), callG, pos)), pos)
        val funcF = Func(SyntaxType.Int, Ident("f", pos), List(), bodyF, pos)

        val inputAST = Program(List(funcF, funcG), Stmt.Skip(pos), pos)

        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe false
    }

    it should "allow call to pass the same renamed arguments to the function" in {
    
        val declX = Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(5, pos), pos)

        val callF = Call(Ident("f", pos), List(Ident("x", pos)), pos)
        val declResult = Stmt.Decl(SyntaxType.Int, Ident("res", pos), callF, pos)

        val funcF = Func(SyntaxType.Int, Ident("f", pos),
                         List(Param(SyntaxType.Int, Ident("p", pos), pos)), 
                         Stmt.Block(List(Stmt.Return(IntLiter(0, pos), pos)), pos), pos)

        val inputAST = Program(List(funcF), Stmt.Block(List(declX, declResult), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredX, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, Call(_, args, _), _) = stmts(1): @unchecked
        
        val usedX = args match {
            case (id: Ident[SemanticData]) :: _ => id
            case _ => fail("Expected an Identifier as the first argument")
        }

        extractId(usedX) shouldBe extractId(declaredX)
    }

    it should "report an error when calling an undefined function" in {

        val callExpr = Call(Ident("imaginaryFunc", pos), List(), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", pos), callExpr, pos)
        
        val inputAST = Program(List(), Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Function 'imaginaryFunc' is not defined")
    }

    it should "report an error on an incorrect number of arguments in a Call" in {

        val returnStmt = Stmt.Return(StrLiter("bye", pos), pos)
        val func = Func(SyntaxType.String, Ident("f", pos), List(), returnStmt, pos)
        val call = Call(Ident("f", pos), List(StrLiter("hello", pos)), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", pos), call, pos)

        val inputAST = Program(List(func), Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()
        
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include
            ("Function 'f' takes 0 arguments but 1 were given")
    }

    "Renaming pairs" should "let a declared pair identifier be used inside fst/snd" in {

        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            PairElem(isFst = true, Ident("p", pos), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declP, declX), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, pIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, usedP: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedP) shouldBe extractId(pIdent)
    }

    it should "rename nested pair extractions" in {

        // pair(pair, int) p = null;
        // int q = fst fst p
        val declP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.ErasedPair, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        // fst fst p  ==> PairElem(fst, PairElem(fst, Ident(p)))
        val nested =
            PairElem(isFst = true, PairElem(isFst = true, Ident("p", pos), pos),pos)

        val declQ = Stmt.Decl(SyntaxType.Int, Ident("q", pos), nested, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declP, declQ), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, pIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, innerLval, _), _) = stmts(1): @unchecked

        // innerLval is PairElem(fst, Ident(p))
        val PairElem(_, usedP: Ident[SemanticData], _) = innerLval: @unchecked
        extractId(usedP) shouldBe extractId(pIdent)
    }

    it should "report an error when fst/snd refers to an out-of-scope identifier" in {
        // int x = fst p  (p is never declared)
        val declX = Stmt.Decl(
            SyntaxType.Int, Ident("x", pos), 
            PairElem(isFst = true,
            Ident("p", pos), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declX), pos), pos)
        val errorLogger = new ErrorLogger()

        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Scope error")
    }

    it should "resolve shadowing correctly for pair identifiers inside nested blocks" in {
        // begin
        //   pair(int, int) p = null;
        //   begin
        //     pair(int, int) p = null;
        //     int x = fst p;
        //   end
        // end
        val outerDeclP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val innerDeclP = Stmt.Decl(
            SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int),
            Ident("p", pos),
            PairLiter(pos),
            pos
        )

        val innerDeclX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            PairElem(isFst = true, Ident("p", pos), pos),
            pos
        )

        val innerBlock = Stmt.Block(List(innerDeclP, innerDeclX), pos)
        val inputAST = Program(Nil, Stmt.Block(List(outerDeclP, innerBlock), pos), pos)

        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(outerStmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, outerP: Ident[SemanticData], _, _) = outerStmts(0): @unchecked
        val Stmt.Block(innerStmts, _) = outerStmts(1): @unchecked

        val Stmt.Decl(_, innerP: Ident[SemanticData], _, _) = innerStmts(0): @unchecked
        val Stmt.Decl(_, _, PairElem(_, usedP: Ident[SemanticData], _), _) =
            innerStmts(1): @unchecked

        // The use in fst p should bind to the inner p, not the outer p
        extractId(usedP) shouldBe extractId(innerP)
        extractId(innerP) should not be extractId(outerP)
    }

    "Renaming arrays" should "let a declared array identifier be used in array element access" in {
        // int[] arr = [1, 2, 3]; int x = arr[0]
        val declArr = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            Ident("arr", pos),
            ArrayLiter(List(IntLiter(1, pos), IntLiter(2, pos), IntLiter(3, pos)), pos),
            pos
        )

        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            ArrayElem(Ident("arr", pos), List(IntLiter(0, pos)), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declArr, declX), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, arrIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, ArrayElem(usedArr: Ident[SemanticData], _, _), _) = stmts(1): @unchecked

        extractId(usedArr) shouldBe extractId(arrIdent)
    }

    it should "rename array element access as lvalue in assignment" in {
        // int[] arr = [1]; arr[0] = 5
        val declArr = Stmt.Decl(
            SyntaxType.Array(SyntaxType.Int),
            Ident("arr", pos),
            ArrayLiter(List(IntLiter(1, pos)), pos),
            pos
        )

        val asgn = Stmt.Asgn(
            ArrayElem(Ident("arr", pos), List(IntLiter(0, pos)), pos),
            IntLiter(5, pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declArr, asgn), pos), pos)
        val errorLogger = new ErrorLogger()
        val renamedAST = Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe false

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, arrIdent: Ident[SemanticData], _, _) = stmts(0): @unchecked
        val Stmt.Asgn(ArrayElem(usedArr: Ident[SemanticData], _, _), _, _) = stmts(1): @unchecked

        extractId(usedArr) shouldBe extractId(arrIdent)
    }

    it should "report an error when array element refers to an out-of-scope identifier" in {
        // int x = arr[0]  (arr is never declared)
        val declX = Stmt.Decl(
            SyntaxType.Int,
            Ident("x", pos),
            ArrayElem(Ident("arr", pos), List(IntLiter(0, pos)), pos),
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declX), pos), pos)
        val errorLogger = new ErrorLogger()

        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        errorLogger.retrieveAllErrors() should include ("Scope error")
    }

    "Renaming free statements" should "correctly resolve the variable being freed" in {
        // pair(int, int) p = newpair(1, 2);
        // free p;
        val pairType = SyntaxType.Pair(SyntaxType.Int, SyntaxType.Int)
        val declP = Stmt.Decl(pairType, Ident("p", pos), PairLiter(pos), pos)
        
        val freeStmt = Stmt.Free(Ident("p", pos), pos)

        val inputAST = Program(Nil, Stmt.Block(List(declP, freeStmt), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredP, _, _) = stmts(0): @unchecked
        
        // Check that 'p' in 'free p' points to the declaration
        val Stmt.Free(usedP: Ident[SemanticData], _) = stmts(1): @unchecked

        extractId(usedP) shouldBe extractId(declaredP)
    }

    it should "recurse into array elements inside free (e.g. free arr[0])" in {
        // int[] arr = ...
        // free arr[0];
        val arrType = SyntaxType.Array(SyntaxType.Int)
        val declArr = Stmt.Decl(arrType, Ident("arr", pos), ArrayLiter(Nil, pos), pos)
        
        val innerExpr = ArrayElem(Ident("arr", pos), List(IntLiter(0, pos)), pos)
        val freeStmt = Stmt.Free(innerExpr, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declArr, freeStmt), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredArr, _, _) = stmts(0): @unchecked
        
        val Stmt.Free(ArrayElem(usedArr: Ident[SemanticData], _, _), _) = stmts(1): @unchecked

        extractId(usedArr) shouldBe extractId(declaredArr)
    }

    it should "report an error when freeing an undefined variable" in {
        // free x;
        val freeStmt = Stmt.Free(Ident("x", pos), pos)

        val inputAST = Program(Nil, Stmt.Block(List(freeStmt), pos), pos)
        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()
        
        errors should include ("Variable 'x' is not defined")
    }
    "The Unary Expression Renamer" should "error when the param of an expr is not declared" in {
        // x is the identifier that has not been declared
        val notExpr = UnaryExpr(UnaryOperator.Not, Ident("x", pos), pos)

        val decl = Stmt.Decl(
            SyntaxType.Bool,
            Ident("b", pos),
            notExpr,
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()

        Renamer.rename(inputAST, errorLogger)

        // Assert that the renamer caught the error
        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'x' is not defined")
    }

    it should "correctly resolve variables in Negation (-)" in {
        // int x = 10;
        // int y = -x;
        val declX = Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(10, pos), pos)
        val declY = Stmt.Decl(
            SyntaxType.Int, 
            Ident("y", pos), 
            UnaryExpr(UnaryOperator.Neg, Ident("x", pos), pos), 
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declX, declY), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredX, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, UnaryExpr(UnaryOperator.Neg,
                                      usedX: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedX) shouldBe extractId(declaredX)
    }

    it should "correctly resolve variables in Boolean Not (!)" in {
        // bool b = true;
        // bool res = !b;
        val declB = Stmt.Decl(SyntaxType.Bool, Ident("b", pos), BoolLiter(true, pos), pos)
        val declRes = Stmt.Decl(
            SyntaxType.Bool, 
            Ident("res", pos), 
            UnaryExpr(UnaryOperator.Not, Ident("b", pos), pos), 
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declB, declRes), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredB, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, UnaryExpr(UnaryOperator.Not,
                                      usedB: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedB) shouldBe extractId(declaredB)
    }

    it should "correctly resolve variables in Ordinal (ord)" in {
        // char c = 'a';
        // int i = ord c;
        val declC = Stmt.Decl(SyntaxType.Char, Ident("c", pos), CharLiter('a', pos), pos)
        val declI = Stmt.Decl(
            SyntaxType.Int, 
            Ident("i", pos), 
            UnaryExpr(UnaryOperator.Ord, Ident("c", pos), pos), 
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declC, declI), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredC, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, UnaryExpr(UnaryOperator.Ord,
                                      usedC: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedC) shouldBe extractId(declaredC)
    }

    it should "correctly resolve variables in Character Conversion (chr)" in {
        // int i = 65;
        // char c = chr i;
        val declI = Stmt.Decl(SyntaxType.Int, Ident("i", pos), IntLiter(65, pos), pos)
        val declC = Stmt.Decl(
            SyntaxType.Char, 
            Ident("c", pos), 
            UnaryExpr(UnaryOperator.Chr, Ident("i", pos), pos), 
            pos
        )

        val inputAST = Program(Nil, Stmt.Block(List(declI, declC), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredI, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, _, UnaryExpr(UnaryOperator.Chr,
                                      usedI: Ident[SemanticData], _), _) = stmts(1): @unchecked

        extractId(usedI) shouldBe extractId(declaredI)
    }

    it should "correctly rename nested unary expressions" in {
        val declX = Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(10, pos), pos)

        // -x
        val innerNeg = UnaryExpr(UnaryOperator.Neg, Ident("x", pos), pos)
        // -(-x)
        val outerNeg = UnaryExpr(UnaryOperator.Neg, innerNeg, pos)

        val declY = Stmt.Decl(SyntaxType.Int, Ident("y", pos), outerNeg, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declX, declY), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredX, _, _) = stmts(0): @unchecked
        
        val Stmt.Decl(_, _, UnaryExpr(UnaryOperator.Neg,
                                      UnaryExpr(UnaryOperator.Neg,
                                                usedX: Ident[SemanticData],
                                                _), _), _) = stmts(1): @unchecked

        extractId(usedX) shouldBe extractId(declaredX)
    }

    "The Binary Expression renamer" should "correctly identify vars in arithmetic operations" in {
        // int x = 10;
        // int y = 20;
        // int res = x + y;
        val declX = Stmt.Decl(SyntaxType.Int, Ident("x", pos), IntLiter(10, pos), pos)
        val declY = Stmt.Decl(SyntaxType.Int, Ident("y", pos), IntLiter(20, pos), pos)

        val addExpr = BinaryExpr(BinaryOperator.Add, Ident("x", pos), Ident("y", pos), pos)
        val declRes = Stmt.Decl(SyntaxType.Int, Ident("res", pos), addExpr, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declX, declY, declRes), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredX, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, declaredY, _, _) = stmts(1): @unchecked
        
        val Stmt.Decl(_, _, BinaryExpr(BinaryOperator.Add,
                                       usedX: Ident[SemanticData],
                                       usedY: Ident[SemanticData], _), _) = stmts(2): @unchecked

        extractId(usedX) shouldBe extractId(declaredX)
        extractId(usedY) shouldBe extractId(declaredY)
    }

    it should "correctly identify variables in comparison operations" in {
        // int a = 5;
        // int b = 10;
        // bool res = a > b;
        val declA = Stmt.Decl(SyntaxType.Int, Ident("a", pos), IntLiter(5, pos), pos)
        val declB = Stmt.Decl(SyntaxType.Int, Ident("b", pos), IntLiter(10, pos), pos)

        val gtExpr = BinaryExpr(BinaryOperator.GT, Ident("a", pos), Ident("b", pos), pos)
        val declRes = Stmt.Decl(SyntaxType.Bool, Ident("res", pos), gtExpr, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declA, declB, declRes), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredA, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, declaredB, _, _) = stmts(1): @unchecked
        
        val Stmt.Decl(_, _, BinaryExpr(BinaryOperator.GT,
                                       usedA: Ident[SemanticData],
                                       usedB: Ident[SemanticData], _), _) = stmts(2): @unchecked

        extractId(usedA) shouldBe extractId(declaredA)
        extractId(usedB) shouldBe extractId(declaredB)
    }

    it should "correctly identify variables in logical operations" in {
        // bool p = true;
        // bool q = false;
        // bool res = p && q;
        val declP = Stmt.Decl(SyntaxType.Bool, Ident("p", pos), BoolLiter(true, pos), pos)
        val declQ = Stmt.Decl(SyntaxType.Bool, Ident("q", pos), BoolLiter(false, pos), pos)

        val andExpr = BinaryExpr(BinaryOperator.And, Ident("p", pos), Ident("q", pos), pos)
        val declRes = Stmt.Decl(SyntaxType.Bool, Ident("res", pos), andExpr, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declP, declQ, declRes), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredP, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, declaredQ, _, _) = stmts(1): @unchecked
        
        val Stmt.Decl(_, _, BinaryExpr(BinaryOperator.And,
                                       usedP: Ident[SemanticData],
                                       usedQ: Ident[SemanticData], _), _) = stmts(2): @unchecked

        extractId(usedP) shouldBe extractId(declaredP)
        extractId(usedQ) shouldBe extractId(declaredQ)
    }

    it should "correctly identify variables in equality operations" in {
        // char c1 = 'a';
        // char c2 = 'b';
        // bool res = c1 == c2;
        val declC1 = Stmt.Decl(SyntaxType.Char, Ident("c1", pos), CharLiter('a', pos), pos)
        val declC2 = Stmt.Decl(SyntaxType.Char, Ident("c2", pos), CharLiter('b', pos), pos)

        val eqExpr = BinaryExpr(BinaryOperator.EQ, Ident("c1", pos), Ident("c2", pos), pos)
        val declRes = Stmt.Decl(SyntaxType.Bool, Ident("res", pos), eqExpr, pos)

        val inputAST = Program(Nil, Stmt.Block(List(declC1, declC2, declRes), pos), pos)
        val renamedAST = Renamer.rename(inputAST, new ErrorLogger())

        val Stmt.Block(stmts, _) = renamedAST.stmt: @unchecked
        val Stmt.Decl(_, declaredC1, _, _) = stmts(0): @unchecked
        val Stmt.Decl(_, declaredC2, _, _) = stmts(1): @unchecked
        
        val Stmt.Decl(_, _, BinaryExpr(BinaryOperator.EQ,
                                       usedC1: Ident[SemanticData],
                                       usedC2: Ident[SemanticData], _), _) = stmts(2): @unchecked

        extractId(usedC1) shouldBe extractId(declaredC1)
        extractId(usedC2) shouldBe extractId(declaredC2)
    }

    it should "correctly handle undeclared variables in binary expressions" in {
        // int x = y + 5;
        val addExpr = BinaryExpr(BinaryOperator.Add, Ident("y", pos), IntLiter(5, pos), pos)
        val decl = Stmt.Decl(SyntaxType.Int, Ident("x", pos), addExpr, pos)
        val inputAST = Program(Nil, Stmt.Block(List(decl), pos), pos)
        val errorLogger = new ErrorLogger()
        Renamer.rename(inputAST, errorLogger)

        errorLogger.hasErrors() shouldBe true
        val errors = errorLogger.retrieveAllErrors()

        errors should include ("Variable 'y' is not defined")
    }
}