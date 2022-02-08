package wacc

import parsley.Parsley, Parsley._
import ast._
import parser._
import wacc.types._
import wacc.symbolTable._

object semanticAnalyser {

	def analyse(node: Node, st: SymbolTable): Boolean = {
		node match {
			case Begin(func, stat) => true
            
			case Function(t, id, vars, stats) => true
				
			case Parameter(t, id) => true
			
			case Skip() => true

			case AssignType(t, id, rhs) => analyseRHS(rhs, st)

			case Assign(lhs, rhs) => analyseLHS(lhs, st) && analyseRHS(rhs, st)

			case Read(lhs) => analyseLHS(lhs, st)

			case Free(expr) => analyseExpr(expr, st)

			case Return(expr) => analyseExpr(expr, st)
				
			case Exit(expr) => analyseExpr(expr, st)

			case Print(expr) => analyseExpr(expr, st)

			case Println(expr) => analyseExpr(expr, st)

			case If(cond, trueStat, falseStat) => true

			case While(cond, stat) => true

			case NestedBegin(stat) => true
			
			case _ => false
		}
	}

    def analyseRHS(assignRHS: AssignRHS, st: SymbolTable): Boolean = {
        assignRHS match {
            case expr: Expr => analyseExpr(expr, st)

            case ArrayLiter(array) => true
                
            case NewPair(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)
 
            case Fst(expr) => analyseExpr(expr, st)

            case Snd(expr) => analyseExpr(expr, st)

            case Call(id, args) => true
        }
    }

    def analyseLHS(assignLHS: AssignLHS, st: SymbolTable): Boolean = {
        assignLHS match {
            case Ident(variable) => true

            case Fst(expr) => analyseExpr(expr, st)

            case Snd(expr) => analyseExpr(expr, st)

            case ArrayElem(id, exprs) => true
        }
    }

    def analyseExpr(expr: Expr, st: SymbolTable): Boolean = {
        expr match {
            case IntLiter(x) => true

            case BoolLiter(bool) => true

            case CharLiter(char) => true

            case StrLiter(string) => true

            case PairLiter() => true

            case Ident(variable) => true

            case ArrayElem(id, exprs) => true    
            
            case Not(innerExpr) => analyseExpr(innerExpr, st)

            case Neg(innerExpr) => analyseExpr(innerExpr, st)

            case Len(innerExpr) => analyseExpr(innerExpr, st)

            case Ord(innerExpr) => analyseExpr(innerExpr, st)

            case Chr(innerExpr) => analyseExpr(innerExpr, st)

            case Mul(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case Div(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case Mod(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case Add(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case Sub(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case GT(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case GTE(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case LT(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case LTE(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case EQ(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case NEQ(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case And(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)

            case Or(expr1, expr2) => analyseExpr(expr1, st) && analyseExpr(expr2, st)
        }
    }
}