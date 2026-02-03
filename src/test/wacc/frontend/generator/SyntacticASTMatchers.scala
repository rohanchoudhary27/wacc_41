package wacc.frontend.generator

import org.scalatest._
import matchers._
import wacc.frontend.generator._
import wacc._


trait SyntacticASTMatchers {

    /**
     * A custom ScalaTest matcher that checks whether two syntactic ASTs are structurally equal,
     * ignoring metadata such as source positions
     *
     * @param expectedAst The AST that the tested AST should match structurally
     */
    class SyntacticASTMatcher(expectedAst: Program[SyntacticData])
        extends Matcher[Program[SyntacticData]] {
        
        /**
         * Compares the left AST with the expected AST
         *
         * @param left The AST to check
         * @return A `MatchResult`` indicating whether the two programs are structurally equivalent
         */
        def apply(left: Program[SyntacticData]) = {
            val Program(lFuncs, lStmt, _) = left
            val Program(rFuncs, rStmt, _) = expectedAst
            MatchResult(
                compareFuncs(lFuncs, rFuncs) && compareStmt(lStmt, rStmt),
                s"${left} did NOT match AST-wise with program ${expectedAst}", 
                s"${shortify(left)} matches AST-wise with program ${shortify(expectedAst)}"
            )
        }

        private def shortify(p: Program[SyntacticData]) = Pretty.program(p).take(10) + "..."
        

        /**
         * Compares two lists of function definitions
         *
         * @param lFuncs Functions from the left AST
         * @param rFuncs Functions from the expected AST
         * @return true if all functions match structurally
         */
        def compareFuncs(lFuncs: List[Func[SyntacticData]],
                         rFuncs: List[Func[SyntacticData]]): Boolean =
            compareListsWith(lFuncs, rFuncs, compareFunc)

        /**
         * Compares two function definitions
         *
         * @param lFunc Function from the left AST
         * @param rFunc Function from the expected AST
         * @return true if return types, identifiers, parameters, and bodies match
         */
        def compareFunc(lFunc: Func[SyntacticData], rFunc: Func[SyntacticData]): Boolean = {
            (lFunc, rFunc) match {
                case (Func(lTyp, lId, lParams, lBody, _), Func(rTyp, rId, rParams, rBody, _)) => 
                    (lTyp == rTyp) && compareExpr(lId, rId) && compareParams(lParams, rParams) &&
                        compareStmt(lBody, rBody)
            }
        }

        /**
         * Compares two lists of function parameters
         *
         * @param lParams Parameters from the left function
         * @param rParams Parameters from the expected function
         * @return true if parameters match pairwise.
         */
        def compareParams(lParams: List[Param[SyntacticData]], 
                          rParams: List[Param[SyntacticData]]): Boolean =
            compareListsWith(lParams, rParams, compareParam)

        /**
         * Compares two individual parameters
         *
         * @param lParam Parameter from the left AST
         * @param rParam Parameter from the expected AST
         * @return true if parameter types and identifiers match
         */
        def compareParam(lParam: Param[SyntacticData], rParam: Param[SyntacticData]) = {
            (lParam, rParam) match {
                case (Param(lTyp, lId, _), Param(rTyp, rId, _)) =>
                    (lTyp == rTyp) && compareExpr(lId, rId)
            }
        }

        /**
         * Compares two lists of statements
         *
         * @param lStmts Statements from the left AST
         * @param rStmts Statements from the expected AST
         * @return true if all statements match pairwise
         */
        def compareStmts(lStmts: List[Stmt[SyntacticData]], 
                         rStmts: List[Stmt[SyntacticData]]): Boolean = 
            compareListsWith(lStmts, rStmts, compareStmt)
        

        /**
         * Compares two statements structurally
         *
         * @param lStmt Statement from the left AST
         * @param rStmt Statement from the expected AST
         * @return true if both statements are of the same type and their components match
         */
        def compareStmt(lStmt: Stmt[SyntacticData], rStmt: Stmt[SyntacticData]): Boolean = {
            (lStmt, rStmt) match {
                case (Stmt.Skip(_), Stmt.Skip(_)) => true
                case (Stmt.Decl(lTyp, lIdent, lRval, _), Stmt.Decl(rTyp, rIdent, rRval, _)) => 
                    (lTyp == rTyp) && compareExpr(lIdent, rIdent) && compareRValue(lRval, rRval)
                case (Stmt.Asgn(lLval, lRval, _), Stmt.Asgn(rLval, rRval, _)) => 
                    compareLValue(lLval, rLval) && compareRValue(lRval, rRval)
                case (Stmt.Read(lLval, _), Stmt.Read(rLval, _)) => 
                    compareLValue(lLval, rLval)
                case (Stmt.Free(lExpr, _), Stmt.Free(rExpr, _)) => 
                    compareExpr(lExpr, rExpr)
                case (Stmt.Return(lExpr, _), Stmt.Return(rExpr, _)) =>
                    compareExpr(lExpr, rExpr)
                case (Stmt.Exit(lExpr, _), Stmt.Exit(rExpr, _)) => 
                    compareExpr(lExpr, rExpr)
                case (Stmt.Print(lExpr, _), Stmt.Print(rExpr, _)) => 
                    compareExpr(lExpr, rExpr)
                case (Stmt.Println(lExpr, _), Stmt.Println(rExpr, _)) => 
                    compareExpr(lExpr, rExpr)
                case (Stmt.If(lCond, lIfBlock, lElseBlock, _), 
                      Stmt.If(rCond, rIfBlock, rElseBlock, _)) => 
                    compareExpr(lCond, rCond) && compareStmt(lIfBlock, rIfBlock) && 
                    compareStmt(lElseBlock, rElseBlock)
                case (Stmt.While(lCond, lStmt, _), Stmt.While(rCond, rStmt, _)) => 
                    compareExpr(lCond, rCond) && compareStmt(lStmt, rStmt)
                case (Stmt.Block(lBlockStmts, _), Stmt.Block(rBlockStmts, _)) =>
                    compareStmts(lBlockStmts, rBlockStmts)
                case _ => false
            }
        }

        /**
         * Compares two expressions structurally
         * 
         * There's a case where a negative integer literal may be represented as either
         * IntLiteral(<a negative value>, _) or UnaryExpr(Neg, IntLiteral(<a positive value>, ...))
         * This matcher does not differentiate them and accepts one for another
         *
         * @param lExpr Expression from the left AST
         * @param rExpr Expression from the expected AST
         * @return true if expressions are equivalent in structure
         */
        def compareExpr(lExpr: Expr[SyntacticData], rExpr: Expr[SyntacticData]): Boolean = {
            (lExpr, rExpr) match {
                case (Ident(lname, _), Ident(rname, _)) => lname == rname
                case (IntLiter(lv, _), IntLiter(rv, _)) => lv == rv
                case (BoolLiter(lv, _), BoolLiter(rv, _)) => lv == rv
                case (CharLiter(lv, _), CharLiter(rv, _)) => lv == rv
                case (StrLiter(lv, _), StrLiter(rv, _)) => lv == rv
                case (PairLiter(_), PairLiter(_)) => true
                case (UnaryExpr(lop, lexpr, _), UnaryExpr(rop, rexpr, _)) =>
                    (lop == rop) && compareExpr(lexpr, rexpr)
                case (BinaryExpr(lop, llexpr, lrexpr, _), BinaryExpr(rop, rlexpr, rrexpr, _)) =>
                    (lop == rop) && compareExpr(llexpr, rlexpr) && compareExpr(lrexpr, rrexpr)
                case (ArrayElem(li, lIdx, _), ArrayElem(ri, rIdx, _)) =>
                    compareExpr(li, ri) && compareListsWith(lIdx, rIdx, compareExpr)
                
                case (IntLiter(negVal, _), UnaryExpr(UnaryOperator.Neg, IntLiter(posVal, _), _)) =>
                    negVal == (-1 * posVal)
                case (UnaryExpr(UnaryOperator.Neg, IntLiter(posVal, _), _), IntLiter(negVal, _)) =>
                    negVal == (-1 * posVal)

                case _ =>
                    false
                }
        }

        /**
         * Compares two lvalues
         *
         * @param lLval lvalue from the left AST
         * @param rLval  lvalue from the expected AST
         * @return true if both lvalues refer to the same structure
         */
        def compareLValue(lLval: LValue[SyntacticData], rLval: LValue[SyntacticData]): Boolean = {
            (lLval, rLval) match {
                case (li: Ident[SyntacticData], ri: Ident[SyntacticData]) => compareExpr(li, ri)
                case (lArr: ArrayElem[SyntacticData], rArr: ArrayElem[SyntacticData]) =>
                    compareExpr(lArr, rArr) 
                case (PairElem(lFst, lLv, _), PairElem(rFst, rLv, _)) =>
                    (lFst == rFst) && compareLValue(lLv, rLv)
                case _ => false
                }
        }

        /**
         * Compares two rvalues
         *
         * @param lRval rvalue from the left AST
         * @param rRval rvalue from the expected AST
         * @return true if both rvalues are structurally equivalent
         */
        def compareRValue(lRval: RValue[SyntacticData], rRval: RValue[SyntacticData]): Boolean = {
            (lRval, rRval) match {
                case (lexpr: Expr[SyntacticData], rexpr: Expr[SyntacticData]) => 
                    compareExpr(lexpr, rexpr)
                case (ArrayLiter(ls, _), ArrayLiter(rs, _)) => compareListsWith(ls, rs, compareExpr)
                case (NewPair(l1, l2, _), NewPair(r1, r2, _)) =>
                    compareExpr(l1, r1) && compareExpr(l2, r2)
                case (Call(li, lArgs, _), Call(ri, rArgs, _)) =>
                    compareExpr(li, ri) && compareListsWith(lArgs, rArgs, compareExpr)
                case (PairElem(lFst, lLv, _), PairElem(rFst, rLv, _)) =>
                    (lFst == rFst) && compareLValue(lLv, rLv)
                case _ => false
            }
        }

        /**
         * Compares two lists elementwise using a provided comparison function
         *
         * @param listLeft left list
         * @param listRight right list
         * @param action predicate used to compare corresponding elements
         * @return true if lists are the same length and all paired elements satisfy the predicate
         */
        def compareListsWith[A](listLeft: List[A], 
                                listRight: List[A], 
                                action: (A, A) => Boolean): Boolean =
            listLeft.length == listRight.length 
                && listLeft.zip(listRight).forall {case (l, r) => action(l, r)}
    }

    def matchProgram(expectedAst: Program[SyntacticData]) = new SyntacticASTMatcher(expectedAst)

}

object SyntacticASTMatchers extends SyntacticASTMatchers
