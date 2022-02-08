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

			case AssignType(t, id, rhs) =>
                val checkedRHS = analyseRHS(rhs, st, extractType(t))
                checkedRHS

			case Assign(lhs, rhs) => true

			case Read(lhs) =>
                val checkedLHS = analyseLHS(lhs, st)
                checkedLHS._1 && (checkedLHS._2 == Some(IntCheck(0)) || checkedLHS._2 == Some(CharCheck(0)))

			case Free(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1

			case Return(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1
				
			case Exit(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1 && (checkedExpr._2 == Some(IntCheck(0)))

			case Print(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1

			case Println(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1

			case If(cond, trueStat, falseStat) => true

			case While(cond, stat) => true

			case NestedBegin(stat) => true
			
			case _ => false
		}
	}

    def analyseRHS(assignRHS: AssignRHS, st: SymbolTable, lhsType: TypeCheck): Boolean = {
        assignRHS match {
            case expr: Expr => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._1 && (checkedExpr._2 == Some(lhsType))

            case ArrayLiter(array) => true
                
            case NewPair(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1)
 
            case Fst(expr) => analyseExpr(expr, st)._1

            case Snd(expr) => analyseExpr(expr, st)._1

            case Call(id, args) => true
        }
    }

    def analyseLHS(assignLHS: AssignLHS, st: SymbolTable): (Boolean, Option[TypeCheck]) = {
        assignLHS match {
            case Fst(expr) => analyseExpr(expr, st)
            
            case Snd(expr) => analyseExpr(expr, st)
            
            case ArrayElem(id, exprs) => (true, None)

            case ident: Ident => analyseExpr(ident, st)
        }
    }

    def analyseExpr(expr: Expr, st: SymbolTable): (Boolean, Option[TypeCheck]) = {
        expr match {
            case IntLiter(x) => (true, Some(IntCheck(0)))

            case BoolLiter(bool) => (true, Some(BoolCheck(0)))

            case CharLiter(char) => (true, Some(CharCheck(0)))

            case StrLiter(string) => (true, Some(StrCheck(0)))

            case PairLiter() => (true, Some(EmptyPairCheck()))

            case Ident(variable) => (true, None)

            case ArrayElem(id, exprs) => (true, None)    
            
            case Not(innerExpr) => analyseExpr(innerExpr, st)

            case Neg(innerExpr) => analyseExpr(innerExpr, st)

            case Len(innerExpr) => analyseExpr(innerExpr, st)

            case Ord(innerExpr) => analyseExpr(innerExpr, st)

            case Chr(innerExpr) => analyseExpr(innerExpr, st)

            case Mul(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(IntCheck(0)))

            case Div(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(IntCheck(0)))

            case Mod(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(IntCheck(0)))

            case Add(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(IntCheck(0)))

            case Sub(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(IntCheck(0)))

            case GT(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case GTE(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case LT(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case LTE(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case EQ(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case NEQ(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, None)

            case And(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(BoolCheck(0)))

            case Or(expr1, expr2) => (analyseExpr(expr1, st)._1 && analyseExpr(expr2, st)._1, Some(BoolCheck(0)))
        }
    }
}