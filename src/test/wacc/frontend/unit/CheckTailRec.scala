package wacc.frontend.unit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.frontend.*

class CheckTailRecTests extends AnyFlatSpec {

    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos

    "The tail recursion checker" should "identify a simple tail recursive call" in {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val funcIdent: Ident[SemanticData] = Ident("f", semPos)

        val paramIdent = Ident("x", SemanticData.Typed(synPos.pos, KnownType.Int))
        val params = List(Param(SyntaxType.Int, paramIdent, semPos))

        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)

        /* int z = call f(x); */
        val xExpr = Ident("x", typedInt)
        val callF = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", typedInt)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)

        /* return z; */
        val returnZ = Stmt.Return(zIdent, semPos)

        val body = Stmt.Block(List(declZ, returnZ), semPos)

        val func: Func[SemanticData] = Func(SyntaxType.Int, funcIdent, params, body, funcSignature)

        val inputAST =
            Program(
                List(func),
                Stmt.Block(List(), semPos),
                SemanticData.TopLevel(funcTable)
            )

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe callF
    }

    it should "identify a tail recursive call in an if statement" in {
        val funcSignature: SemanticData.Func = SemanticData.Func(synPos.pos,
                                                                 "f",
                                                                 KnownType.Int,
                                                                 List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        val typedBool = SemanticData.Typed(synPos.pos, KnownType.Bool)
        
        val xExpr = Ident("x", typedInt)
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", typedInt)
        
        /* if (true) { int z = call f(x); return z; } else { return x; } */
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        val ifBlock = Stmt.Block(List(declZ, returnZ), semPos)
        val elseBlock = Stmt.Block(List(Stmt.Return(xExpr, semPos)), semPos)
        
        val trueExpr = BoolLiter(true, typedBool)
        val ifStmt = Stmt.If(trueExpr, ifBlock, elseBlock, semPos)
        val body = Stmt.Block(List(ifStmt), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int,
                                   Ident("x", typedInt), semPos)),
                                   body, funcSignature)
        val inputAST = Program(List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe callF
    }

    it should "identify a tail recursive call in a while statement" in {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        val typedBool = SemanticData.Typed(synPos.pos, KnownType.Bool)
        
        val xExpr = Ident("x", typedInt)
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", typedInt)
        
        /* while (true) { int z = call f(x); return z; } */
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        val whileBlock = Stmt.Block(List(declZ, returnZ), semPos)
        
        val trueExpr = BoolLiter(true, typedBool)
        val whileStmt = Stmt.While(trueExpr, whileBlock, semPos)
        val body = Stmt.Block(List(whileStmt), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int, Ident("x", typedInt), semPos)),
                        body, funcSignature)
        val inputAST = Program(List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe callF
    }
    
    it should "not mark a non-tail recursive call as tail-recursive" in {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        
        val xExpr: Expr[SemanticData] = Ident("x", typedInt)
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", typedInt)
        
        /* int z = call f(x); int y = z + x; return y; */
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)
        val yIdent = Ident("y", typedInt)
        val addExpr = BinaryExpr(BinaryOperator.Add, zIdent, xExpr, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, addExpr, semPos)
        val returnY = Stmt.Return(yIdent, semPos)
        
        val body = Stmt.Block(List(declZ, declY, returnY), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int, Ident("x", typedInt), semPos)),
                        body, funcSignature)

        val inputAST = Program(List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 0
    }

    it should "only mark a tailrec node as tailrec for self-recursion" in {
        val funcFSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcGSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "g", KnownType.Int, List(KnownType.Int))
        
        val funcFIdent: Ident[SemanticData] = Ident("f", semPos)
        val funcGIdent: Ident[SemanticData] = Ident("g", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        
        val xExpr = Ident("x", typedInt)
        val zIdent = Ident("z", typedInt)
        
        /* int z = call g(x); return z; */
        val callG: Call[SemanticData] = Call(funcGIdent, List(xExpr), typedInt)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callG, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        
        val body = Stmt.Block(List(declZ, returnZ), semPos)

        val param = Param(SyntaxType.Int, Ident("x", typedInt), semPos)
        val funcF = Func(SyntaxType.Int, funcFIdent, List(param), body, funcFSignature)
        val funcTable = Map("f" -> funcFSignature, "g" -> funcGSignature)
        val inputAST =
            Program(List(funcF), Stmt.Block(List(), semPos), SemanticData.TopLevel(funcTable))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 0
    }

    it should "allow for assignment statements for tail recursive patterns" in {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        
        val xExpr: Expr[SemanticData] = Ident("x", typedInt)
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", typedInt)
        
        /* int z = 0; z = call f(x); return z; */
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, IntLiter(0, typedInt), semPos)
        val asgnZ = Stmt.Asgn(zIdent, callF, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        
        val body = Stmt.Block(List(declZ, asgnZ, returnZ), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int, Ident("x", typedInt), semPos)),
                        body, funcSignature)

        val inputAST = Program(List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 1
        tailCalls.toList(0) shouldBe callF
    }

    it should "find multiple tailrec calls and ignore non-tail rec ones in the same function" in {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
        val typedBool = SemanticData.Typed(synPos.pos, KnownType.Bool)
        
        val xExpr: Expr[SemanticData] = Ident("x", typedInt)
        
        /* if (true) { int a = call f(x); return a; } */
        val call1: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val aIdent = Ident("a", typedInt)
        val declA = Stmt.Decl(SyntaxType.Int, aIdent, call1, semPos)
        val returnA = Stmt.Return(aIdent, semPos)
        val ifBlock = Stmt.Block(List(declA, returnA), semPos)
        
        /* else { int b = call f(x); int c = call f(b); return c; } */

        /* not tail recursive */
        val call2 = Call(funcIdent, List(xExpr), typedInt)
        val bIdent = Ident("b", typedInt)
        val declB = Stmt.Decl(SyntaxType.Int, bIdent, call2, semPos)
        
        /* tail recursive */
        val call3 = Call(funcIdent, List(bIdent), typedInt)
        val cIdent = Ident("c", typedInt)
        val declC = Stmt.Decl(SyntaxType.Int, cIdent, call3, semPos)
        val returnC = Stmt.Return(cIdent, semPos)
        val elseBlock = Stmt.Block(List(declB, declC, returnC), semPos)
        
        val trueExpr = BoolLiter(true, typedBool)
        val ifStmt = Stmt.If(trueExpr, ifBlock, elseBlock, semPos)
        val body = Stmt.Block(List(ifStmt), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int,
                                   Ident("x", typedInt), semPos)), body, funcSignature)

        val inputAST = Program(List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        val tailCalls = CheckTailRec.checkTailRec(inputAST)

        tailCalls.size shouldBe 2
        tailCalls.toList should contain (call1)
        tailCalls.toList should contain (call3)
    }

}
