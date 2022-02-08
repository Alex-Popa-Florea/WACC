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

			case AssignType(t, id, rhs) => true

			case Assign(lhs, rhs) => true

			case Read(lhs) => true

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

    def analyseExpr(expression: Expr, st: SymbolTable): Boolean = {
        expression match {
            case IntLiter(x) => true

            case BoolLiter(bool) => true

            case CharLiter(char) => true

            case StrLiter(string) => true

            case PairLiter() => true

            case Ident(variable) => true

            case ArrayElem(id, exprs) => true    
            
            case Not(expr1) => true

            case Neg(expr1) => true

            case Len(expr1) => true

            case Ord(expr1) => true

            case Chr(expr1) => true

            case Mul(expr1, expr2) => true

            case Div(expr1, expr2) => true

            case Mod(expr1, expr2) => true

            case Add(expr1, expr2) => true

            case Sub(expr1, expr2) => true

            case GT(expr1, expr2) => true

            case GTE(expr1, expr2) => true

            case LT(expr1, expr2) => true

            case LTE(expr1, expr2) => true

            case EQ(expr1, expr2) => true

            case NEQ(expr1, expr2) => true

            case And(expr1, expr2) => true

            case Or(expr1, expr2) => true
        }
    }
}