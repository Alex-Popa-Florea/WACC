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
            
			case Function(t, id, vars, stats) => 
                var nst = new SymbolTable(Option(st))
				val addedFunction = st.add((id.variable, true), extractType(t))
				val checkedParams = vars.forall(x => analyse(x, nst))
				val checkedStats = stats.forall(x => analyse(x, nst))
				addedFunction && checkedParams && checkedStats
				
			case Parameter(t, id) => 
                st.add((id.variable, false), extractType(t))
			
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

			case If(cond, trueStat, falseStat) =>
				var trueNst = new SymbolTable(Option(st))
				var falseNst = new SymbolTable(Option(st))
				val conditionCheck = analyseExpr(cond, st)
				val trueStatCheck = trueStat.forall(x => analyse(x, trueNst))
				val falseStatCheck = falseStat.forall(x => analyse(x, falseNst))
			 	conditionCheck._1 && (conditionCheck._2 == Some(BoolCheck(0))) && trueStatCheck && falseStatCheck

			case While(cond, stat) => 
                var nst = new SymbolTable(Option(st))
				val conditionCheck = analyseExpr(cond, st)
				conditionCheck._1 && (conditionCheck._2 == Some(BoolCheck(0))) && stat.forall(x => analyse(x, nst))

			case NestedBegin(stat) => 
                var nst = new SymbolTable(Option(st))
				stat.forall(x => analyse(x, nst))
			
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
            case Fst(expr) =>
                var checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case Some(foundType) => foundType match {
                        case PairCheck(type1, _, 0) => (checkedExpr._1, Some(type1))
					    case _ => (false, None)
                    }
                    case None => (false, None)
                }
            
            case Snd(expr) =>
                var checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case Some(foundType) => foundType match {
                        case PairCheck(_, type2, 0) => (checkedExpr._1, Some(type2))
					    case _ => (false, None)
                    }
                    case None => (false, None)
                }
            
            case ArrayElem(id, exprs) => 
				var variable = st.find((id.variable, false))
				variable match {
					case None => (false, None)
					case Some(foundType) =>
						foundType match {
							case IntCheck(nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(IntCheck(nested - exprs.size)))
								} else {
									(false, None)
								}
							case BoolCheck(nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(BoolCheck(nested - exprs.size)))
								} else {
									(false, None)
								}
							case CharCheck(nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(CharCheck(nested - exprs.size)))
								} else {
									(false, None)
								}
							case StrCheck(nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(StrCheck(nested - exprs.size)))
								} else {
									(false, None)
								}
							case PairCheck(type1, type2, nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(PairCheck(type1, type2, nested - exprs.size)))
								} else {
									(false, None)
								}
							case EmptyPairCheck() => (false, None)
						}					
				}

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