package wacc.testhelpers

import wacc.frontend.*

trait ASTBoilerplate {
    val synPos = SyntacticData.Pos(0, 0)
    val semPos = synPos.toSemanticPos
    val typedInt = SemanticData.Typed(synPos.pos, KnownType.Int)
    val typedBool = SemanticData.Typed(synPos.pos, KnownType.Bool)
}

trait TailRecPrebuiltTrees extends ASTBoilerplate {

    /* Simple tailrec ast */
    def simpleTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcTable: Map[String, SemanticData.Func] = Map("f" -> funcSignature)

        val funcIdent: Ident[SemanticData] = Ident("f", semPos)

        val paramIdent = Ident("x", SemanticData.Typed(synPos.pos, KnownType.Int))
        val params = List(Param(SyntaxType.Int, paramIdent, semPos))

        /* int z = call f(x); */
        val xExpr = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val callF = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)

        /* return z; */
        val returnZ = Stmt.Return(zIdent, semPos)

        val body = Stmt.Block(List(declZ, returnZ), semPos)

        val func: Func[SemanticData] = Func(SyntaxType.Int, funcIdent, params, body, funcSignature)

        val ast =
            Program(
                Nil,
                List(func),
                Stmt.Block(List(), semPos),
                SemanticData.TopLevel(funcTable)
            )

        (ast, List(callF)) 
    }

    /* Tailrec ast with if statement */
    def ifStmtTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {

        val funcSignature: SemanticData.Func = SemanticData.Func(synPos.pos,
                                                                 "f",
                                                                 KnownType.Int,
                                                                 List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        
        val xExpr = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        
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
        val ast = Program(Nil, List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        (ast, List(callF))

    }

    /* Tailrec ast with while statement */
    def whileStmtTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        
        val xExpr = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        
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
        val ast = Program(Nil, List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        (ast, List(callF))

    }

    /* Tailrec ast but it's a mutual recursion */
    def nonSelfTailRecursive(): (Program[SemanticData], List[Call[SemanticData]]) = {

        val funcFSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcGSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "g", KnownType.Int, List(KnownType.Int))
        
        val funcFIdent: Ident[SemanticData] = Ident("f", semPos)
        val funcGIdent: Ident[SemanticData] = Ident("g", semPos)
        
        val xExpr = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        
        /* int z = call g(x); return z; */
        val callG: Call[SemanticData] = Call(funcGIdent, List(xExpr), typedInt)
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callG, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        
        val body = Stmt.Block(List(declZ, returnZ), semPos)

        val param = Param(SyntaxType.Int, Ident("x", typedInt), semPos)
        val funcF = Func(SyntaxType.Int, funcFIdent, List(param), body, funcFSignature)
        val funcTable = Map("f" -> funcFSignature, "g" -> funcGSignature)
        val ast =
            Program(Nil, List(funcF), Stmt.Block(List(), semPos), SemanticData.TopLevel(funcTable))

        (ast, List())
    }

    /* Ast with call node that isn't tailrec */
    def basicNonTailrec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        
        val xExpr: Expr[SemanticData] = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val callF: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        
        /* int z = call f(x); int y = z + x; return y; */
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callF, semPos)
        val yIdent = Ident("y", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val addExpr = BinaryExpr(BinaryOperator.Add, zIdent, xExpr, typedInt)
        val declY = Stmt.Decl(SyntaxType.Int, yIdent, addExpr, semPos)
        val returnY = Stmt.Return(yIdent, semPos)
        
        val body = Stmt.Block(List(declZ, declY, returnY), semPos)

        val func = Func(SyntaxType.Int,
                        funcIdent,
                        List(Param(SyntaxType.Int, Ident("x", typedInt), semPos)),
                        body, funcSignature)

        val ast = Program(Nil, List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))

        (ast, List())
    }

    /* Tailrec ast via an assignment of the call node (not declaration) */
    def assignmentTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {

        val funcSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", semPos)
        
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

        val ast = Program(Nil, List(func),
                               Stmt.Block(List(), semPos),
                               SemanticData.TopLevel(Map("f" -> funcSignature)))
        
        (ast, List(callF))
    }

    /* AST with one function with 3 recursive calls - 2 of which are tail recursive */
    def threeCallsTwoTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcSignature: SemanticData.Func = SemanticData.Func(synPos.pos, "f", KnownType.Int, List(KnownType.Int))
        val funcIdent: Ident[SemanticData] = Ident("f", funcSignature)
        val xExpr = Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        
        /* if (true) { int a = call f(x); return a; } */
        val call1: Call[SemanticData] = Call(funcIdent, List(xExpr), typedInt)
        val aIdent = Ident("a", SemanticData.Var(synPos.pos, 1, KnownType.Int))
        val declA = Stmt.Decl(SyntaxType.Int, aIdent, call1, semPos)
        val returnA = Stmt.Return(aIdent, semPos)
        val ifBlock = Stmt.Block(List(declA, returnA), semPos)
        
        /* else { int b = call f(1); int c = call f(b); return c; } */
        
        /* Non tailrec call */
        val oneExpr = IntLiter(1, typedInt)
        val call2 = Call(funcIdent, List(oneExpr), typedInt) 
        val bIdent = Ident("b", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val declB = Stmt.Decl(SyntaxType.Int, bIdent, call2, semPos)
        
        /* Tailrec call */
        val call3 = Call(funcIdent, List(bIdent), typedInt)
        val cIdent = Ident("c", SemanticData.Var(synPos.pos, 3, KnownType.Int))
        val declC = Stmt.Decl(SyntaxType.Int, cIdent, call3, semPos)
        val returnC = Stmt.Return(cIdent, semPos)

        val elseBlock = Stmt.Block(List(declB, declC, returnC), semPos)

        val trueExpr = BoolLiter(true, typedBool)
        val ifStmt = Stmt.If(trueExpr, ifBlock, elseBlock, semPos)
        val body = Stmt.Block(List(ifStmt), semPos)

        val func = Func(
            SyntaxType.Int, funcIdent, 
            List(Param(SyntaxType.Int, Ident("x", SemanticData.Var(synPos.pos, 0, KnownType.Int)), semPos)), 
            body, funcSignature
        )

        val ast = Program(
            Nil, List(func), Stmt.Block(List(), semPos), SemanticData.TopLevel(Map("f" -> funcSignature))
        )

        (ast, List(call1, call3))
    }

    /* general tailrec AST with stack args but still safe to tailrec optimise */
    def safeManyArgsGeneralTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcFSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List.fill(10)(KnownType.Int))
        val funcGSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "g", KnownType.Int, List.fill(9)(KnownType.Int))
        val funcFIdent: Ident[SemanticData] = Ident("f", funcFSignature)
        val funcGIdent: Ident[SemanticData] = Ident("g", funcGSignature)
        
        val argsG: List[Ident[SemanticData]] =
            (0 until 9).map(i => Ident(s"p$i", SemanticData.Var(synPos.pos, i, KnownType.Int))).toList
        val callG = Call(funcGIdent, argsG, typedInt)
        
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 10, KnownType.Int))
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callG, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        val body = Stmt.Block(List(declZ, returnZ), semPos)
        
        val paramsF = (0 until 10).map(i => Param(SyntaxType.Int,
                                                  Ident(s"p$i",
                                                        SemanticData.Var(synPos.pos, i, KnownType.Int)), semPos)).toList
        val funcF: Func[SemanticData] = Func(SyntaxType.Int, funcFIdent, paramsF, body, funcFSignature)
        val ast = Program(Nil, List(funcF),
                          Stmt.Block(List(), semPos),
                          SemanticData.TopLevel(Map("f" -> funcFSignature, "g" -> funcGSignature)))
        
        (ast, List(callG))
    }

    /* general tailrec AST with stack args but still unsafe to tailrec optimise */
    def unsafeManyArgsGeneralTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcFSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List.fill(2)(KnownType.Int))
        val funcGSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "g", KnownType.Int, List.fill(10)(KnownType.Int))
        val funcFIdent: Ident[SemanticData] = Ident("f", funcFSignature)
        val funcGIdent: Ident[SemanticData] = Ident("g", funcGSignature)
        
        val argsG = (0 until 10).map(i => IntLiter(i, typedInt)).toList
        val callG = Call(funcGIdent, argsG, typedInt)
        
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 2, KnownType.Int))
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callG, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        val body = Stmt.Block(List(declZ, returnZ), semPos)
        
        val paramsF = (0 until 2).map(i => Param(SyntaxType.Int,
                                                 Ident(s"p$i", SemanticData.Var(synPos.pos, i, KnownType.Int)), semPos)).toList
        val funcF = Func(SyntaxType.Int, funcFIdent, paramsF, body, funcFSignature)
        val ast = Program(Nil, List(funcF),
                          Stmt.Block(List(), semPos),
                          SemanticData.TopLevel(Map("f" -> funcFSignature, "g" -> funcGSignature)))
        
        (ast, List(callG))
    }


    /* general tailrec AST with no stack args */
    def exactlyEightArgsGeneralTailRec(): (Program[SemanticData], List[Call[SemanticData]]) = {
        val funcFSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "f", KnownType.Int, List())
        val funcGSignature: SemanticData.Func =
            SemanticData.Func(synPos.pos, "g", KnownType.Int, List.fill(8)(KnownType.Int))
        val funcFIdent: Ident[SemanticData] = Ident("f", funcFSignature)
        val funcGIdent: Ident[SemanticData] = Ident("g", funcGSignature)
        
        val argsG = (0 until 8).map(i => IntLiter(i, typedInt)).toList
        val callG = Call(funcGIdent, argsG, typedInt)
        
        val zIdent = Ident("z", SemanticData.Var(synPos.pos, 0, KnownType.Int))
        val declZ = Stmt.Decl(SyntaxType.Int, zIdent, callG, semPos)
        val returnZ = Stmt.Return(zIdent, semPos)
        val body = Stmt.Block(List(declZ, returnZ), semPos)
        
        val funcF = Func(SyntaxType.Int, funcFIdent, List(), body, funcFSignature)
        val ast = Program(Nil, List(funcF),
                          Stmt.Block(List(), semPos),
                          SemanticData.TopLevel(Map("f" -> funcFSignature, "g" -> funcGSignature)))
        
        (ast, List(callG))
    }
}