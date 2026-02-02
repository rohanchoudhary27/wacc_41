package wacc.frontend.generator

import org.scalacheck.Gen
import wacc.*

/**
 * AST Generator for randomised testing
 *
 * This generator produces syntactically valid ASTs.
 * Semantic concerns (including types, scopes, declarations) are ignored
 */
object AstGenerator {

    // Default size for generators
    val defaultSize = 100

    /* Generate a syntactic pos annotation */
    def genPos: Gen[SyntacticData] = for {
        line <- Gen.choose(1, 1000)
        col <- Gen.choose(1, 100)
    } yield SyntacticData.Pos((line, col))

    /* Generate a base type (int, bool, char, string) */
    def genBaseType: Gen[SyntaxType] =
        Gen.oneOf(SyntaxType.Int, SyntaxType.Bool, SyntaxType.Char, SyntaxType.String)

    /* Generate a pair element type (base, array, or erased pair, no nested pairs) */
    def genPairElemType: Gen[SyntaxType] = Gen.oneOf(
        genBaseType,
        genBaseType.map(SyntaxType.Array(_)),
        Gen.const(SyntaxType.ErasedPair)
    )

    /** Generate any syntax type */
    val genSyntaxType: Gen[SyntaxType] = Gen.sized {
        case 0 => genBaseType
        case n => Gen.frequency(
            4 -> genBaseType,
            2 -> Gen.resize(n - 1, genSyntaxType).map(SyntaxType.Array(_)),
            2 -> (for {
                fst <- genPairElemType
                snd <- genPairElemType
            } yield SyntaxType.Pair(fst, snd))
        )
    }

    // simple helper definition (size = 5 is reasonable for most cases I think)
    def genSimpleType: Gen[SyntaxType] = Gen.resize(5, genSyntaxType)


    /* Generate an integer literal (within 32 bit range) */
    def genIntLiter: Gen[Expr[SyntacticData]] = for {
        value <- Gen.choose(Int.MinValue, Int.MaxValue)
        pos <- genPos
    } yield IntLiter(value, pos)

    /* Generate a boolean literal */
    def genBoolLiter: Gen[Expr[SyntacticData]] = for {
        value <- Gen.oneOf(true, false)
        pos <- genPos
    } yield BoolLiter(value, pos)

    /** Generate a valid WACC character (printable ascii) */
    def genValidChar: Gen[Char] = Gen.frequency(
        10 -> Gen.choose('a', 'z'),
        10 -> Gen.choose('A', 'Z'),
        5 -> Gen.choose('0', '9'),
        4 -> Gen.oneOf(' ', '_', '-', '+', '=', '.', ',', '!', '?', '@', '#', '$',
            '%', '^', '&', '*', '(', ')', '[', ']', '{', '}', '<', '>', '/',
            '|', '~', '`', ':', ';')
    )

    /** Generate a character literal */
    def genCharLiter: Gen[Expr[SyntacticData]] = for {
        value <- genValidChar
        pos <- genPos
    } yield CharLiter(value, pos)

    /** Generate a string literal */
    def genStrLiter: Gen[Expr[SyntacticData]] = for {
        len <- Gen.choose(0, 20)
        chars <- Gen.listOfN(len, genValidChar)
        pos <- genPos
    } yield StrLiter(chars.mkString, pos)

    /** Generate a null pair literal */
    def genPairLiter: Gen[Expr[SyntacticData]] = for {
        pos <- genPos
    } yield PairLiter(pos)

    /** Generate a random identifier name */
    def genIdentName: Gen[String] = for {
        prefix <- Gen.oneOf("x", "y", "z", "a", "b", "var", "tmp", "arr", "p", "data")
        suffix <- Gen.choose(0, 99)
    } yield s"${prefix}_$suffix"

    /** Generate an identifier */
    def genIdent: Gen[Ident[SyntacticData]] = for {
        name <- genIdentName
        pos <- genPos
    } yield Ident(name, pos)

    /** All leaf expression generators */
    def genLeafExpr: Gen[Expr[SyntacticData]] = Gen.oneOf(
        genIntLiter,
        genBoolLiter,
        genCharLiter,
        genStrLiter,
        genPairLiter,
        genIdent.map(id => id: Expr[SyntacticData])
    )

    /**
     * Generate any expression using sized generation
     * At size 0, generates only leaves
     * At size n, for binary expressions, generate two trees with n/2 size, for unary generates
     * a tree with n-1 size
     */
    val genExpr: Gen[Expr[SyntacticData]] = Gen.recursive[Expr[SyntacticData]] { ge =>
        Gen.sized {
            case 0 => genLeafExpr
            case n => {
                val halfSize = Gen.resize(n / 2, ge)
                val smallerSize = Gen.resize(n - 1, ge)

                Gen.frequency(
                    2 -> genIntLiter,
                    1 -> genBoolLiter,
                    1 -> genCharLiter,
                    1 -> genStrLiter,
                    1 -> genPairLiter,
                    2 -> genIdent.map(id => id: Expr[SyntacticData]),
                    2 -> genUnaryExprWith(smallerSize),
                    2 -> genBinaryExprWith(halfSize), 
                    1 -> genArrayElemWith(smallerSize).map(ae => ae: Expr[SyntacticData])
                )
            }
        }
    }

    /** Generate a unary expression with a given child generator */
    private def genUnaryExprWith(childGen: Gen[Expr[SyntacticData]]): Gen[Expr[SyntacticData]] 
            = for {
            op <- Gen.oneOf(UnaryOperator.values.toSeq)
            expr <- childGen
            pos <- genPos
        } yield UnaryExpr(op, expr, pos)

    /** Generate a binary expression with a given child generator */
    private def genBinaryExprWith(childGen: Gen[Expr[SyntacticData]]): Gen[Expr[SyntacticData]] 
        = for {
            op <- Gen.oneOf(BinaryOperator.values.toSeq)
            left <- childGen
            right <- childGen
            pos <- genPos
        } yield BinaryExpr(op, left, right, pos)

    /** Generate an array element with a given index expression generator */
    private def genArrayElemWith(indexGen: Gen[Expr[SyntacticData]]): Gen[ArrayElem[SyntacticData]] 
        = for {
            ident <- genIdent
            indexCount <- Gen.choose(1, 3)
            indices <- Gen.listOfN(indexCount, indexGen)
            pos <- genPos
        } yield ArrayElem(ident, indices, pos)

    /**
     * Generate any lvalue using sized generation.
     * At size 0, only identifiers are generated (which correspond to leaves of lvalue)
     */
    val genLValue: Gen[LValue[SyntacticData]] = Gen.recursive[LValue[SyntacticData]] { gl =>
        Gen.sized {
            case 0 => genIdent.map(id => id: LValue[SyntacticData])
            case n => {
                val smallerLValue = Gen.resize(n - 1, gl)
                val smallerExpr = Gen.resize(n - 1, genExpr)

                Gen.frequency(
                    3 -> genIdent.map(id => id: LValue[SyntacticData]),
                    2 -> genArrayElemWith(smallerExpr).map(ae => ae: LValue[SyntacticData]),
                    2 -> genPairElemWith(smallerLValue).map(pe => pe: LValue[SyntacticData])
                )
            }
        }
    }

    /** Generate a pair element with a given lvalue generator */
    private def genPairElemWith(lvalGen: Gen[LValue[SyntacticData]]): Gen[PairElem[SyntacticData]]
         = for {
            isFst <- Gen.oneOf(true, false)
            lval <- lvalGen
            pos <- genPos
        } yield PairElem(isFst, lval, pos)


    /**
     * Generate any rvalue using sized generation.
     * At size 0, only expressions are generated
     */
    val genRValue: Gen[RValue[SyntacticData]] = Gen.recursive[RValue[SyntacticData]] {_ =>
        Gen.sized {
            case 0 => genExpr.map(e => e: RValue[SyntacticData])
            case n => {
                val smallerExpr = Gen.resize(n - 1, genExpr)
                val halfExpr = Gen.resize(n / 2, genExpr)
                val smallerLValue = Gen.resize(n - 1, genLValue)

                Gen.frequency(
                    4 -> genExpr.map(e => e: RValue[SyntacticData]),
                    2 -> genArrayLiterWith(smallerExpr).map(al => al: RValue[SyntacticData]),
                    2 -> genNewPairWith(halfExpr).map(np => np: RValue[SyntacticData]),
                    2 -> genCallWith(smallerExpr).map(c => c: RValue[SyntacticData]),
                    2 -> genPairElemWith(smallerLValue).map(pe => pe: RValue[SyntacticData])
                )
            }
        }
    }

    /** Generate an array literal with a given element generator */
    private def genArrayLiterWith(elemGen: Gen[Expr[SyntacticData]]): Gen[ArrayLiter[SyntacticData]]
         = for {
            len <- Gen.choose(0, 4)
            elems <- Gen.listOfN(len, elemGen)
            pos <- genPos
        } yield ArrayLiter(elems, pos)

    /** Generate a newpair with a given expression generator */
    private def genNewPairWith(exprGen: Gen[Expr[SyntacticData]]): Gen[NewPair[SyntacticData]] 
        = for {
            fst <- exprGen
            snd <- exprGen
            pos <- genPos
        } yield NewPair(fst, snd, pos)

    /** Generate a function call with a given argument generator */
    private def genCallWith(argGen: Gen[Expr[SyntacticData]]): Gen[Call[SyntacticData]] = for {
        ident <- genIdent
        argCount <- Gen.choose(0, 3)
        args <- Gen.listOfN(argCount, argGen)
        pos <- genPos
    } yield Call(ident, args, pos)

    /** Generate a skip statement (leaf) */
    def genSkip: Gen[Stmt[SyntacticData]] = for {
        pos <- genPos
    } yield Stmt.Skip(pos)

    /** Generate a declaration statement with given rvalue generator */
    private def genDeclWith(rvalGen: Gen[RValue[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        typ <- genSimpleType
        ident <- genIdent
        rval <- rvalGen
        pos <- genPos
    } yield Stmt.Decl(typ, ident, rval, pos)

    /** Generate an assignment statement with given lvalue and rvalue generators */
    private def genAsgnWith(
        lvalGen: Gen[LValue[SyntacticData]],
        rvalGen: Gen[RValue[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = for {
        lval <- lvalGen
        rval <- rvalGen
        pos <- genPos
    } yield Stmt.Asgn(lval, rval, pos)

    /** Generate a read statement with given lvalue generator */
    private def genReadWith(lvalGen: Gen[LValue[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        lval <- lvalGen
        pos <- genPos
    } yield Stmt.Read(lval, pos)

    /** Generate a free statement with given expression generator */
    private def genFreeWith(exprGen: Gen[Expr[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        expr <- exprGen
        pos <- genPos
    } yield Stmt.Free(expr, pos)

    /** Generate a print statement with given expression generator */
    private def genPrintWith(exprGen: Gen[Expr[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        expr <- exprGen
        pos <- genPos
    } yield Stmt.Print(expr, pos)

    /** Generate a println statement with given expression generator */
    private def genPrintlnWith(exprGen: Gen[Expr[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        expr <- exprGen
        pos <- genPos
    } yield Stmt.Println(expr, pos)

    /** Generate an exit statement with given expression generator */
    private def genExitWith(exprGen: Gen[Expr[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        expr <- exprGen
        pos <- genPos
    } yield Stmt.Exit(expr, pos)

    /** Generate a return statement with given expression generator */
    private def genReturnWith(exprGen: Gen[Expr[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        expr <- exprGen
        pos <- genPos
    } yield Stmt.Return(expr, pos)

    /** Leaf statements */
    private def genLeafStmt(
        exprGen: Gen[Expr[SyntacticData]],
        lvalGen: Gen[LValue[SyntacticData]],
        rvalGen: Gen[RValue[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = Gen.oneOf(
        genSkip,
        genDeclWith(rvalGen),
        genAsgnWith(lvalGen, rvalGen),
        genReadWith(lvalGen),
        genFreeWith(exprGen),
        genPrintWith(exprGen),
        genPrintlnWith(exprGen)
    )

    /**
     * Generate a non returning statement
     * At size 0, only leaf statements are generated
     * At size n > 0, if, while, block are possible with resized size
     */
    val genStmt: Gen[Stmt[SyntacticData]] = Gen.recursive[Stmt[SyntacticData]] { gstmt =>
        Gen.sized { n =>
            val exprGen = Gen.resize(n, genExpr)
            val lvalGen = Gen.resize(n, genLValue)
            val rvalGen = Gen.resize(n, genRValue)

            if (n <= 0) genLeafStmt(exprGen, lvalGen, rvalGen)
            else {
                // For nested statements, split budget
                val halfStmt = Gen.resize(n / 2, gstmt)

                Gen.frequency(
                    // Leaf statements (higher weight)
                    7 -> genLeafStmt(exprGen, lvalGen, rvalGen),
                    // Compound statements with reduced budget
                    1 -> genIfWith(exprGen, halfStmt),
                    1 -> genWhileWith(exprGen, halfStmt),
                    1 -> genBlockWith(halfStmt)
                )
            }
        }
    }

    /** Generate an if statement with given expression and statement generators */
    private def genIfWith(
        exprGen: Gen[Expr[SyntacticData]],
        stmtGen: Gen[Stmt[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = for {
        cond <- exprGen
        ifBlock <- genStmtBlockWith(stmtGen)
        elseBlock <- genStmtBlockWith(stmtGen)
        pos <- genPos
    } yield Stmt.If(cond, ifBlock, elseBlock, pos)

    /** Generate a while statement with given expression and statement generators */
    private def genWhileWith(
        exprGen: Gen[Expr[SyntacticData]],
        stmtGen: Gen[Stmt[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = for {
        cond <- exprGen
        body <- genStmtBlockWith(stmtGen)
        pos <- genPos
    } yield Stmt.While(cond, body, pos)

    /** Generate a block statement with given statement generator */
    private def genBlockWith(stmtGen: Gen[Stmt[SyntacticData]]): Gen[Stmt[SyntacticData]] = for {
        stmts <- genStmtListWith(stmtGen)
        pos <- genPos
    } yield Stmt.Block(stmts, pos)

    /** Generate a list of statements with given statement generator */
    private def genStmtListWith(stmtGen: Gen[Stmt[SyntacticData]]): Gen[List[Stmt[SyntacticData]]] 
        = for {
            count <- Gen.choose(1, 10) // statements length
            stmts <- Gen.listOfN(count, stmtGen)
        } yield stmts

    /** Generate a statement block */
    private def genStmtBlockWith(stmtGen: Gen[Stmt[SyntacticData]]): Gen[Stmt[SyntacticData]] 
        = for {
            stmts <- genStmtListWith(stmtGen)
            pos <- genPos
        } yield Stmt.Block(stmts, pos)

    /**
     * Generate a statement that guarantees returning.
     * Uses sized generation to control depth of returning if statements.
     */
    val genReturningStmt: Gen[Stmt[SyntacticData]] = Gen.recursive[Stmt[SyntacticData]] { gstmt =>
        Gen.sized { n =>
            val exprGen = Gen.resize(n, genExpr)

            if (n <= 0) Gen.oneOf(genReturnWith(exprGen), genExitWith(exprGen))
            else {
                val halfReturning = Gen.resize(n / 2, gstmt)

                Gen.frequency(
                    3 -> genReturnWith(exprGen),
                    2 -> genExitWith(exprGen),
                    2 -> genReturningIfWith(exprGen, halfReturning)
                )
            }
        }
    }

    /** Generate a returning if with given expression and returning statement generators */
    private def genReturningIfWith(
        exprGen: Gen[Expr[SyntacticData]],
        returningGen: Gen[Stmt[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = for {
        cond <- exprGen
        ifBranch <- genReturningBlockWith(returningGen)
        elseBranch <- genReturningBlockWith(returningGen)
        pos <- genPos
    } yield Stmt.If(cond, ifBranch, elseBranch, pos)

    /** Generate a returning block with given returning statement generator */
    private def genReturningBlockWith(
        returningGen: Gen[Stmt[SyntacticData]]
    ): Gen[Stmt[SyntacticData]] = Gen.sized { n =>
        val stmtGen = Gen.resize(n, genStmt)
        for {
            prefixCount <- Gen.choose(0, 2)
            prefixStmts <- Gen.listOfN(prefixCount, stmtGen)
            returnStmt <- returningGen
            pos <- genPos
        } yield {
            val allStmts = prefixStmts :+ returnStmt
            Stmt.Block(allStmts, pos)
        }
    }

    /** Generate a function parameter */
    def genParam: Gen[Param[SyntacticData]] = for {
        typ <- genSimpleType
        ident <- genIdent
        pos <- genPos
    } yield Param(typ, ident, pos)

    /** Generate a function */
    val genFunc: Gen[Func[SyntacticData]] = Gen.sized { n =>
        val bodyGen = Gen.resize(n, genReturningStmt)
        for {
            retType <- genSimpleType
            ident <- genIdent
            paramCount <- Gen.choose(0, 3)
            params <- Gen.listOfN(paramCount, genParam)
            body <- genReturningBlockWith(bodyGen)
            pos <- genPos
        } yield Func(retType, ident, params, body, pos)
    }

    /**
     * Generate a complete syntactically valid program
     */
    val genProgram: Gen[Program[SyntacticData]] = Gen.sized { n =>
        val stmtGen = Gen.resize(n, genStmt)
        for {
            funcCount <- Gen.choose(0, 3)
            funcs <- Gen.listOfN(funcCount, Gen.resize(n, genFunc))
            mainStmts <- genStmtListWith(stmtGen)
            pos <- genPos
        } yield Program(funcs, Stmt.Block(mainStmts, pos), pos)
    }

    /** Generate a program with a specific size */
    def genProgramSyntactic(size: Int = defaultSize): Gen[Program[SyntacticData]] =
        Gen.resize(size, genProgram)
}
