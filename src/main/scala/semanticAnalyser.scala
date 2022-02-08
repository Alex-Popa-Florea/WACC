package wacc

import parsley.Parsley, Parsley._
import ast._
import parser._
import wacc.types._
import wacc.symbolTable._
import wacc.functionTable._

object semanticAnalyser {
    var functionTable = new FunctionTable()
	def analyse(node: Node, st: SymbolTable, returnType: Option[TypeCheck]): (Boolean, Boolean) = {
		node match {
			case Begin(func, stat) =>
                func.map(function => functionTable.add(function.id.variable, extractType(function.t), function.vars.map(x => extractType(x.t))))
				val functionsChecked = func.map(x => analyse(x, st, None))
                functionsChecked.reduceOption((x, y) => ((x._1 && y._1), false)) match {
					case None => (stat.forall(x => analyse(x, st, None)._1), true)
					case Some(value) => (value._1 && stat.forall(x => analyse(x, st, None)._1), functionsChecked.last._2)
				}
            
			case Function(t, id, vars, stats) => 
                var nst = new SymbolTable(Option(st))
                st.children = nst :: st.children
				val addedFunction = st.add((id.variable, true), extractType(t))
				val checkedParams = vars.forall(x => analyse(x, nst, None)._1)
				val checkedStats = stats.map(x => analyse(x, nst, Some(extractType(t))))
				(addedFunction && checkedParams && checkedStats.reduce((x, y) => ((x._1 && y._1), false))._1, checkedStats.last._2)
				
			case Parameter(t, id) => 
                (st.add((id.variable, false), extractType(t)), false)
			
			case Skip() => (true, false)

			case AssignType(t, id, rhs) =>
                val checkedRHS = analyseRHS(rhs, st, extractType(t))
                val addedVariable = st.add((id.variable, false), extractType(t))
                (checkedRHS && addedVariable, false)

			case Assign(lhs, rhs) => 
                val checkedLHS = analyseLHS(lhs, st)
                checkedLHS._2 match {
                    case None => (false, false)
                    case Some(foundType) => ((checkedLHS._1 && analyseRHS(rhs, st, foundType)), false)
                }
                
			case Read(lhs) =>
                val checkedLHS = analyseLHS(lhs, st)
                (checkedLHS._1 && (checkedLHS._2 == Some(IntCheck(0)) || checkedLHS._2 == Some(CharCheck(0))), false)

			case Free(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._2 match {
                    case None => (false, false)
                    case Some(foundType) => foundType match {
                        case IntCheck(nested) => (nested > 0, false)
						case BoolCheck(nested) => (nested > 0, false)
						case CharCheck(nested) => (nested > 0, false)
						case StrCheck(nested) => (nested > 0, false)
						case PairCheck(_, _, _) => (true, false)
						case EmptyPairCheck() => (false, false)
                    }
                }

			case Return(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                returnType match {
					case Some(foundReturnType) => 
						foundReturnType match {
							case EmptyPairCheck() => (false, true)
                            case PairCheck(_, _, _) =>
								(checkedExpr._2 == returnType || checkedExpr._2 == Some(EmptyPairCheck()), true)
                            case _ => (checkedExpr._2 == returnType, true)
						}
					case None => (false, false)
				}
				
			case Exit(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1 && (checkedExpr._2 == Some(IntCheck(0))), true)

			case Print(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)

			case Println(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)

			case If(cond, trueStat, falseStat) =>
				var trueNst = new SymbolTable(Option(st))
                st.children = trueNst :: st.children
				var falseNst = new SymbolTable(Option(st))
                st.children = falseNst :: st.children
				val conditionCheck = analyseExpr(cond, st)
				val trueStatCheck = trueStat.map(x => analyse(x, trueNst, returnType))
				val falseStatCheck = falseStat.map(x => analyse(x, falseNst, returnType))
			 	(conditionCheck._1 && (conditionCheck._2 == Some(BoolCheck(0))) && 
                 trueStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1 && 
                 falseStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1, 
                 trueStatCheck.last._2 && falseStatCheck.last._2)

			case While(cond, stat) => 
                var nst = new SymbolTable(Option(st))
                st.children = nst :: st.children
				val conditionCheck = analyseExpr(cond, st)
				(conditionCheck._1 && (conditionCheck._2 == Some(BoolCheck(0))) && 
                 stat.forall(x => analyse(x, nst, returnType)._1), false)

			case NestedBegin(stat) => 
                var nst = new SymbolTable(Option(st))
                st.children = nst :: st.children
				(stat.forall(x => analyse(x, nst, returnType)._1), false)
			
			case _ => (false, false)
		}
	}

    def analyseRHS(assignRHS: AssignRHS, st: SymbolTable, lhsType: TypeCheck): Boolean = {
        assignRHS match {
            case expr: Expr => 
                val checkedExpr = analyseExpr(expr, st)
                lhsType match {
                    case PairCheck(type1, type2, nested) => 
                        checkedExpr._1 && (checkedExpr._2 == Some(lhsType) || checkedExpr._2 == Some(EmptyPairCheck()))
                    case _ => checkedExpr._1 && (checkedExpr._2 == Some(lhsType))                      
                }

            case ArrayLiter(indices) => 
                lhsType match {
					case IntCheck(nested) => indices.forall(x => analyseExpr(x, st) == (true, Some(IntCheck(nested - 1))))
					case BoolCheck(nested) => indices.forall(x => analyseExpr(x, st) == (true, Some(BoolCheck(nested - 1))))
					case CharCheck(nested) => indices.forall(x => analyseExpr(x, st) == (true, Some(CharCheck(nested - 1))))
					case StrCheck(nested) => indices.forall(x => analyseExpr(x, st) == (true, Some(StrCheck(nested - 1))))
					case PairCheck(type1, type2, nested) => 
						indices.forall(x => {
							val checkedExpr = analyseExpr(x, st)
							checkedExpr == (true, Some(PairCheck(type1, type2, nested - 1))) || checkedExpr == (true, Some(EmptyPairCheck()))
						})
					case _ => false
				} 
                
            case NewPair(expr1, expr2) => 
                val checkedExpr1 = analyseExpr(expr1, st)
				val checkedExpr2 = analyseExpr(expr2, st)
                if (checkedExpr1._2 != None && checkedExpr2._2 != None) {
                    val foundType1 = checkedExpr1._2.get
                    val foundType2 = checkedExpr2._2.get
                    lhsType match {			
                        case PairCheck(EmptyPairCheck(), EmptyPairCheck(), 0) => checkedExpr1._1 && checkedExpr2._1 && 
                            (foundType1 match {
                                case EmptyPairCheck() => true
                                case PairCheck(_, _, 0) => true
                                case _ => false
                            }) &&
                            (foundType2 match {
                                case EmptyPairCheck() => true
                                case PairCheck(_, _, 0) => true
                                case _ => false
                            })	
                        case PairCheck(EmptyPairCheck(), foundType2, 0) => checkedExpr1._1 && checkedExpr2._1 && 
                            (foundType1 match {
                                case EmptyPairCheck() => true
                                case PairCheck(_, _, 0) => true
                                case _ => false
                            })
                        case PairCheck(foundType1, EmptyPairCheck(), 0) => checkedExpr1._1 && checkedExpr2._1 &&
                            (foundType2 match {
                                case EmptyPairCheck() => true
                                case PairCheck(_, _, 0) => true
                                case _ => false
                            })					
                        case PairCheck(foundType1, foundType2, 0) => checkedExpr1._1 && checkedExpr2._1
                        case _ => false
                    }
                } else {
                    false
                }
 
            case Fst(expr) => 
                val checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case None => false
                    case Some(foundType) => foundType match {
                        case PairCheck(EmptyPairCheck(), _, 0) => 
                            foundType match {
                                case PairCheck(_, _, _) => checkedExpr._1
                                case _ => false
                            }
                        case PairCheck(type1, _, 0) => checkedExpr._1 && (type1 == lhsType)
                        case _ => false
                    } 
				}

            case Snd(expr) => 
                val checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case None => false
                    case Some(foundType) => foundType match {
                        case PairCheck(_, EmptyPairCheck(), 0) => 
                            foundType match {
                                case PairCheck(_, _, _) => checkedExpr._1
                                case _ => false
                            }
                        case PairCheck(_, type2, 0) => checkedExpr._1 && (type2 == lhsType)
                        case _ => false
                    } 
				}

            case Call(id, args) => 
                val checkedArgs = args.map(x => analyseExpr(x, st))
				functionTable.funcMap.get(id.variable) match {
					case Some(foundFuncType) =>
                        if (lhsType == EmptyPairCheck()) {
                            foundFuncType._1 match {
									case PairCheck(type1, type2, nested) => checkedArgs.forall(x => x._1) && functionTable.check(id.variable, checkedArgs.map(x => x._2.get))
									case _ => foundFuncType._1 == lhsType && checkedArgs.forall(x => x._1) && functionTable.check(id.variable, checkedArgs.map(x => x._2.get))
							}
                        } else {
						    foundFuncType._1 == lhsType && checkedArgs.forall(x => x._1) && functionTable.check(id.variable, checkedArgs.map(x => x._2.get))	
                        }
					case _ => false
				}
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
            
            case arrayElem: ArrayElem => analyseExpr(arrayElem, st)

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

            case ArrayElem(id, exprs) =>
                var foundType = st.find((id.variable, false))
				foundType match {
					case None => (false, None)
					case Some(array) =>
						array match {
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
            
            case Not(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == Some(BoolCheck(0))), Some(BoolCheck(0)))

			case Neg(innerExpr) => 
				var checkedInnerExpr = analyseExpr(innerExpr, st)
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == Some(IntCheck(0))), Some(IntCheck(0)))

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
				(checkedInnerExpr._1 && (checkedInnerExpr._2 == Some(IntCheck(0))), Some(CharCheck(0)))
            
            case binOpInt: BinOpInt => 
                var checkedExpr1 = analyseExpr(binOpInt.expr1, st)
				var checkedExpr2 = analyseExpr(binOpInt.expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(IntCheck(0))), Some(IntCheck(0)))
            
            case binOpComp: BinOpComp => 
                var checkedExpr1 = analyseExpr(binOpComp.expr1, st)
				var checkedExpr2 = analyseExpr(binOpComp.expr2, st)
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))
            
            case binOpEqs: BinOpEqs => 
                var checkedExpr1 = analyseExpr(binOpEqs.expr1, st)
				var checkedExpr2 = analyseExpr(binOpEqs.expr2, st)
                if (checkedExpr1._2 == None || checkedExpr2._2 == None) {
                    (false, None)
                } else {
                    (checkedExpr1._1 && checkedExpr2._1 && 
                    (checkedExpr1._2.get match {
                        case PairCheck(type1, type2, nested) => checkedExpr2._2.get match {
                            case EmptyPairCheck() => true
                            case PairCheck(type1, type2, nested) => true
                            case _ => false
                        }
                        case EmptyPairCheck() => checkedExpr1._2.get match {
                            case EmptyPairCheck() => true
                            case PairCheck(type1, type2, nested) => true
                            case _ => false
                        }
                        case _ => checkedExpr1._2 == checkedExpr2._2
                    }), Some(BoolCheck(0)))
                }
            
            case binOpBool: BinOpBool => 
                var checkedExpr1 = analyseExpr(binOpBool.expr1, st)
				var checkedExpr2 = analyseExpr(binOpBool.expr2, st)
                (checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && (checkedExpr1._2 == Some(BoolCheck(0))), Some(BoolCheck(0)))
				
		}
	}
}