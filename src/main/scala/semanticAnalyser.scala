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
                val addedVariable = st.add((id.variable, false), extractType(t))
                checkedRHS && addedVariable

			case Assign(lhs, rhs) => 
                val checkedLHS = analyseLHS(lhs, st)
                checkedLHS._2 match {
                    case None => false
                    case Some(foundType) => (checkedLHS._1 && analyseRHS(rhs, st, foundType))
                }
                
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

            case Ident(variable) =>
                var foundVariableType = st.find((variable, false))
                (foundVariableType != None, foundVariableType)

            case ArrayElem(id, exprs) => (true, None)    
            
            case Not(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == BoolCheck(0)), Some(BoolCheck(0)))

			case Neg(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == IntCheck(0)), Some(IntCheck(0)))

			case Len(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				checkedInnerExpr._2 match {
                    case None => (false, None)
                    case Some(extractedCheckedInnerExpr) => extractedCheckedInnerExpr match {
                        case IntCheck(nested) => (checkedInnerExpr._1 && (nested > 0), Some(IntCheck(0)))
                        case BoolCheck(nested) => (checkedInnerExpr._1 && (nested > 0), Some(IntCheck(0)))
                        case CharCheck(nested) => (checkedInnerExpr._1 && (nested > 0), Some(IntCheck(0)))
                        case StrCheck(nested) => (checkedInnerExpr._1 && (nested > 0), Some(IntCheck(0)))
                        case PairCheck(type1, type2, nested) => (checkedInnerExpr._1 && (nested > 0), Some(IntCheck(0)))
                        case EmptyPairCheck() => (false, None)
                    }
                }

			case Ord(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == Some(CharCheck(0))), Some(IntCheck(0)))

			case Chr(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == Some(CharCheck(0))), Some(IntCheck(0)))

			case Mul(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))

			case Div(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))

			case Mod(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))

			case Add(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))

			case Sub(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))
               
			case GT(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))

			case GTE(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))

			case LT(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))

			case LTE(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))

			case EQ(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
                (checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2), Some(BoolCheck(0)))

			case NEQ(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
                (checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2), Some(BoolCheck(0)))

			case And(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
                (checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(BoolCheck(0))), Some(BoolCheck(0)))

			case Or(expr1, expr2) => 
				var checkedExpr1 = analyseExpr(expr1, st)
				var checkedExpr2 = analyseExpr(expr2, st)
                (checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(BoolCheck(0))), Some(BoolCheck(0)))
				
		}
	}
}