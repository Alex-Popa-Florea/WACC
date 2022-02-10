package wacc

import parsley.Parsley, Parsley._
import ast._
import parser._
import wacc.types._
import wacc.symbolTable._
import wacc.functionTable._

object semanticAnalyser {
    var errors: List[(String, (Int, Int))] = List()
    var returnTypeError: Option[(String, (Int, Int))] = None
    val undeclared = "undeclared: "
    val expression = "Error in expression"
    val mismatch = "Type mismatch"
    val argsIncorrect = "Wrong number of arguments"
    val ranks = "Incorrect number of ranks in array access: "


	def analyse(node: Node, st: SymbolTable, ft: FunctionTable, returnType: Option[TypeCheck]): (Boolean, Boolean) = {
		node match {
			case Begin(func, stat) =>
                var addedFunctions = true
                for (function <- func){
                    val addedFunction = ft.add(function.id.variable, extractType(function.t), function.vars.map(x => extractType(x.t)))
                    if (!addedFunction) {
                        addedFunctions = false
                        errors = errors :+ ((s"Function ${function.id.variable} has already been defined, sorry!", node.pos))
                    }
                }
				val functionsChecked = func.map(x => analyse(x, st, ft, None))
                functionsChecked.reduceOption((x, y) => ((x._1 && y._1), false)) match {
					case None => (stat.forall(x => analyse(x, st, ft, None)._1), true)
					case Some(value) => (addedFunctions && value._1 && stat.forall(x => analyse(x, st, ft, None)._1), functionsChecked.last._2)
				}
            
			case Function(t, id, vars, stats) => 
                var nst = new SymbolTable(s"Function ${id.variable}", Option(st))
                st.children = st.children :+ nst
				val checkedParams = vars.forall(x => analyse(x, nst, ft, None)._1)
				val checkedStats = stats.map(x => analyse(x, nst, ft, Some(extractType(t))))
				(checkedParams && checkedStats.reduce((x, y) => ((x._1 && y._1), false))._1, checkedStats.last._2)
				
			case Parameter(t, id) => 
                val addedParam = st.add(id.variable, extractType(t))
                if (!addedParam) {
                    errors = errors :+ ((s"Cannot duplicate parameter name: ${id.variable}", node.pos))
                }
                (addedParam, false)
			
			case Skip() => (true, false)

			case AssignType(t, id, rhs) =>
                val addedVariable = st.add(id.variable, extractType(t))
                if (!addedVariable) {
                    errors = errors :+ ((s"Variable already declared", node.pos))
                }
                val checkedRHS = analyseRHS(rhs, st, ft, extractType(t))
                (checkedRHS && addedVariable, false)

			case Assign(lhs, rhs) => 
                val checkedLHS = analyseLHS(lhs, st)
                checkedLHS._2 match {
                    case None => (false, false)
                    case Some(foundType) => ((checkedLHS._1 && analyseRHS(rhs, st, ft, foundType)), false)
                }
                
			case Read(lhs) =>
                val checkedLHS = analyseLHS(lhs, st)
                val correctType = checkedLHS._2 == Some(IntCheck(0)) || checkedLHS._2 == Some(CharCheck(0))
                if (correctType) {
                    errors = errors :+ ((s"Expression of type int or char expected in read statement, but expression of type ${checkedLHS._2} found!", node.pos))
                }
                (checkedLHS._1 && correctType, false)

			case Free(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._2 match {
                    case None => (false, false)
                    case Some(foundType) => foundType match {
                        case baseTypeCheck: BaseTypeCheck => 
                            val correctType = baseTypeCheck.nested > 0
                            if (!correctType) {
                                errors = errors :+ ((s"Array or Pair expected in free statement, but expression of type ${checkedExpr._2} found!", node.pos))
                            }
                            (correctType, false)
						case PairCheck(_, _, _) => (true, false)
						case EmptyPairCheck() => (true, false)
                    }
                }

			case Return(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                returnType match {
					case Some(foundReturnType) => 
						foundReturnType match {
							case EmptyPairCheck() => 
                                errors = errors :+ (s"Return type cannot be empty pair literal!", node.pos)
                                (false, true)
                            case PairCheck(_, _, _) =>
                                val correctType = checkedExpr._2 == returnType || checkedExpr._2 == Some(EmptyPairCheck())
                                if (!correctType) {
                                    errors = errors :+ ((s"Expression does not match return type of function, expected ${foundReturnType} but expression of type ${checkedExpr._2} found!", node.pos))
                                }
								(checkedExpr._1 && correctType, true)
                            case _ => 
                                val correctType = checkedExpr._2 == returnType
                                if (!correctType) {
                                    errors = errors :+ ((s"Expression does not match return type of function, expected ${foundReturnType} but expression of type ${checkedExpr._2} found!", node.pos))
                                }
                                (checkedExpr._1 && correctType, true)
						}
					case None => 
                        errors = errors :+ (s"Cannot have return statement in main", node.pos)
                        (false, false)
				}
				
			case Exit(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                val correctType = checkedExpr._2 == Some(IntCheck(0))
                if (!correctType) {
                    errors = errors :+ ((s"Expression of type int expected in exit call, but expression of type ${checkedExpr._2} found!" , node.pos))
                }
                (checkedExpr._1 && correctType, true)

			case Print(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)

			case Println(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)

			case If(cond, trueStat, falseStat) =>
				var trueNst = new SymbolTable("True branch of if statement", Option(st))
                st.children = st.children :+ trueNst
				var falseNst = new SymbolTable("False branch of if statement", Option(st))
                st.children = st.children :+ falseNst
				val conditionCheck = analyseExpr(cond, st)
				val trueStatCheck = trueStat.map(x => analyse(x, trueNst, ft, returnType))
				val falseStatCheck = falseStat.map(x => analyse(x, falseNst, ft, returnType))
                val correctType = conditionCheck._2 == Some(BoolCheck(0))
                if (!correctType) {
                    errors = errors :+ ((s"Expression of type bool expected in if statement condition, but expression of type ${conditionCheck._2} found!", node.pos))
                }
			 	(conditionCheck._1 && correctType && 
                 trueStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1 && 
                 falseStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1, 
                 trueStatCheck.last._2 && falseStatCheck.last._2)

			case While(cond, stat) => 
                var nst = new SymbolTable("Statements inside while loop", Option(st))
                st.children = st.children :+ nst
				val conditionCheck = analyseExpr(cond, st)
                val correctType = conditionCheck._2 == Some(BoolCheck(0))
                if (!correctType) {
                    errors = errors :+ ((s"Expression of type bool expected in while statement condition, but expression of type ${conditionCheck._2} found!", node.pos))
                }
				(conditionCheck._1 && correctType && 
                 stat.forall(x => analyse(x, nst, ft, returnType)._1), false)

			case NestedBegin(stat) => 
                var nst = new SymbolTable("Statements inside nested begin", Option(st))
                st.children = st.children :+ nst
				(stat.forall(x => analyse(x, nst, ft, returnType)._1), false)
			
			case _ => (false, false)
		}
	}

    def analyseRHS(assignRHS: AssignRHS, st: SymbolTable, ft: FunctionTable, lhsType: TypeCheck): Boolean = {
        assignRHS match {
            case expr: Expr => 
                val checkedExpr = analyseExpr(expr, st)
                lhsType match {
                    case PairCheck(type1, type2, nested) => 
                        val correctType =  checkedExpr._2 == Some(lhsType) || checkedExpr._2 == Some(EmptyPairCheck())
                        if (!correctType) {
                            errors = errors :+ ((s"Expression of type pair expected in right hand side of assignment, but expression of type ${checkedExpr._2} found!", assignRHS.pos))
                        }
                        (checkedExpr._1 && correctType)
                    case _ => 
                        val correctType = checkedExpr._2 == Some(lhsType)
                        if (!correctType) {
                            errors = errors :+ ((s"Expression of type ${lhsType} expected in right hand side of assignment, but expression of type ${checkedExpr._2} found!", assignRHS.pos))
                        }
                        (checkedExpr._1 && correctType)                      
                }

            case ArrayLiter(elements) => 
                lhsType match {
                    case baseTypeCheck: BaseTypeCheck =>
                        val correctType = elements.forall(x => analyseExpr(x, st) == (true, baseTypeCheck match {
                            case IntCheck(nested) => Some(IntCheck(nested - 1))
                            case BoolCheck(nested) => Some(BoolCheck(nested - 1))
                            case CharCheck(nested) => Some(CharCheck(nested - 1))
                            case StrCheck(nested) => Some(StrCheck(nested - 1))
                        }))
                        if (!correctType) {
                            errors = errors :+ ((s"Elements in array must all be of type ${baseTypeCheck.nested - 1}!", assignRHS.pos))
                        }
                        correctType
					case PairCheck(type1, type2, nested) => 
						val correctType = elements.forall(x => {
							val checkedExpr = analyseExpr(x, st)
							checkedExpr == (true, Some(PairCheck(type1, type2, nested - 1))) || checkedExpr == (true, Some(EmptyPairCheck()))
						})
                        if (!correctType) {
                            errors = errors :+ ((s"Elements in array must all be of type ${PairCheck(type1, type2, nested - 1)}!", assignRHS.pos))
                        }
                        correctType
					case _ => false
				} 
                
            case NewPair(expr1, expr2) => 
                val checkedExpr1 = analyseExpr(expr1, st)
				val checkedExpr2 = analyseExpr(expr2, st)
                if (checkedExpr1._2 != None && checkedExpr2._2 != None) {
                    val foundType1 = checkedExpr1._2.get
                    val foundType2 = checkedExpr2._2.get
                    lhsType match {						
                        case PairCheck(lhsFoundType1, lhsFoundType2, 0) => 
                            val correctType = checkedExpr1._1 && checkedExpr2._1 && 
                            (foundType1 match {
                                case EmptyPairCheck() | PairCheck(_, _, 0) => lhsFoundType1 match {
                                    case EmptyPairCheck() => true
                                    case _ => false
                                }
                                case _ => (foundType1 == lhsFoundType1)
                            }) &&
                            (foundType2 match {
                                case EmptyPairCheck() | PairCheck(_, _, 0) => lhsFoundType2 match {
                                    case EmptyPairCheck() => true
                                    case _ => false
                                }
                                case _ => (foundType2 == lhsFoundType2)
                            })
                            if (!correctType) {
                                errors = errors :+ ((s"New pair must be of type ${lhsType} but is of type pair($foundType1, $foundType2)!", assignRHS.pos))
                            }	
                            correctType
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
                            lhsType match {
                                case PairCheck(_, _, 0) => checkedExpr._1
                                case _ => 
                                    errors = errors :+ ((s"First element of input pair should be of type: $lhsType, but found: pair!", assignRHS.pos))
                                    false
                            }
                        case PairCheck(type1, _, 0) =>
                            val correctType = type1 == lhsType
                            if (!correctType) {
                                errors = errors :+ ((s"First element of input pair should be of type: $lhsType, but found: $type1!", assignRHS.pos))
                            }
                            checkedExpr._1 && correctType
                        case _ => 
                            errors = errors :+ ((s"Input of fst must be of type pair but found type: $foundType!", assignRHS.pos))
                            false
                    }

				}

            case Snd(expr) => 
                val checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case None => false
                    case Some(foundType) => foundType match {
                        case PairCheck(_, EmptyPairCheck(), 0) => 
                            lhsType match {
                                case PairCheck(_, _, 0) => checkedExpr._1
                                case _ => 
                                    errors = errors :+ ((s"Second element of input pair should be of type: $lhsType, but found: pair!", assignRHS.pos))
                                    false
                            }
                        case PairCheck(_, type2, 0) => 
                            val correctType = type2 == lhsType
                            if (!correctType) {
                                errors = errors :+ ((s"Second element of input pair should be of type: $lhsType, but found: $type2!", assignRHS.pos))
                            }
                            checkedExpr._1 && (type2 == lhsType)
                        case _ => 
                            errors = errors :+ ((s"Input of snd must be of type pair but found type: $foundType!", assignRHS.pos))
                            false
                    } 
				}

            case Call(id, args) => 
                val checkedArgs = args.map(x => analyseExpr(x, st))
				ft.funcMap.get(id.variable) match {
					case Some(foundFuncType) =>
                        if (lhsType == EmptyPairCheck()) {
                            foundFuncType._1 match {
									case PairCheck(type1, type2, nested) => 
                                        val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                                        if (!checkedNumArgs) {
                                            errors = errors :+ ((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                                        }
                                        val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                        if (checkedNumArgs && !checkArgs) {
                                            errors = errors :+ ((s"Wrong argument types ja ja ja in call to function ${id.variable}!", assignRHS.pos)) //ja ja ja
                                        }
                                        checkedArgs.forall(x => x._1) && ft.check(id.variable, checkedArgs.map(x => x._2.get))
									case _ =>
                                        val correctType = foundFuncType._1 == lhsType
                                        if (!correctType) {
                                            errors = errors :+ ((s"Expected type: $lhsType, actual type of function return: ${foundFuncType._1}!", assignRHS.pos))
                                        }
                                        val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                                        if (!checkedNumArgs) {
                                            errors = errors :+ ((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                                        }
                                        val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                        if (checkedNumArgs && !checkArgs) {
                                            errors = errors :+ ((s"Wrong argument types ja ja ja in call to function ${id.variable}!", assignRHS.pos)) //ja ja ja
                                        }
                                        correctType && checkedArgs.forall(x => x._1) && ft.check(id.variable, checkedArgs.map(x => x._2.get))
							}
                        } else {
                            val correctType = foundFuncType._1 == lhsType
                            if (!correctType) {
                                errors = errors :+ ((s"Expected type: $lhsType, actual type of function return: ${foundFuncType._1}!", assignRHS.pos))
                            }
                            val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                            if (!checkedNumArgs) {
                                errors = errors :+ ((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                            }
                            val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                            if (checkedNumArgs && !checkArgs) {
                                errors = errors :+ ((s"Wrong argument types ja ja ja in call to function ${id.variable}!", assignRHS.pos)) //ja ja ja
                            }
                            correctType && checkedArgs.forall(x => x._1) && checkArgs	
                        }
					case None => 
                        errors = errors :+ ((s"Function ${id.variable} not declared, sorry!", assignRHS.pos))
                        false
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
            case IntLiter(_) => (true, Some(IntCheck(0)))

            case BoolLiter(_) => (true, Some(BoolCheck(0)))

            case CharLiter(_) => (true, Some(CharCheck(0)))

            case StrLiter(_) => (true, Some(StrCheck(0)))

            case PairLiter() => (true, Some(EmptyPairCheck()))

            case Ident(variable) => 
                val foundVariableType = st.find(variable)
                val isFound = foundVariableType != None
                if (!isFound) {
                    errors = errors :+ (variable + " " + undeclared, expr.pos)
                }
                (isFound, foundVariableType)
                    
            case ArrayElem(id, exprs) =>
                val foundType = st.find(id.variable)
				foundType match {
					case None => 
                        errors = errors :+ (id.variable + undeclared, expr.pos)
                        (false, None)
					case Some(array) =>
						array match {
                            case baseTypeCheck: BaseTypeCheck => 
                                if (baseTypeCheck.nested >= exprs.size) {
                                    (exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), baseTypeCheck match {
                                        case IntCheck(nested) => Some(IntCheck(nested - exprs.size))
                                        case BoolCheck(nested) => Some(BoolCheck(nested - exprs.size))
                                        case CharCheck(nested) => Some(CharCheck(nested - exprs.size))
                                        case StrCheck(nested) => Some(StrCheck(nested - exprs.size))
                                    })  
                                } else {
                                    errors = errors :+ (s"${id.variable} has type: $array, which does not have ${exprs.size} ranks!" , expr.pos)
                                    (false, None)
                                }
							case PairCheck(type1, type2, nested) =>
								if (nested >= exprs.size) {
									(exprs.forall(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))), Some(PairCheck(type1, type2, nested - exprs.size)))
								} else {
                                    errors = errors :+ (s"${id.variable} has type: $array, which does not have ${exprs.size} ranks!" , expr.pos)
									(false, None)
								}
							case _ => (false, None)
						}					
				}    
            
            case Not(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(BoolCheck(0))
                if (checkedInnerExpr._1 && !correctType) {
                    errors = errors :+ (s"Expression of type bool expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                }
				(checkedInnerExpr._1 && correctType, Some(BoolCheck(0)))

			case Neg(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(IntCheck(0))
                if (checkedInnerExpr._1 && !correctType) {
                    errors = errors :+ (s"Expression of type int expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                }
				(checkedInnerExpr._1 && correctType, Some(IntCheck(0)))

			case Len(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
				checkedInnerExpr._2 match {
                    case None => (false, None)
                    case Some(extractedCheckedInnerExpr) => extractedCheckedInnerExpr match {
                        case baseTypeCheck: BaseTypeCheck =>
                            val correctNesting = baseTypeCheck.nested > 0
                            if (checkedInnerExpr._1 && !correctNesting) {
                                errors = errors :+ (s"Array  expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                            } 
                            (checkedInnerExpr._1 && correctNesting, Some(IntCheck(0)))
                        case PairCheck(type1, type2, nested) => 
                            val correctNesting = nested > 0
                            if (checkedInnerExpr._1 && !correctNesting) {
                                errors = errors :+ (s"Array expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                            }
                            (checkedInnerExpr._1 && correctNesting, Some(IntCheck(0)))
                        case EmptyPairCheck() => (false, None)
                    }
                }

			case Ord(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(CharCheck(0))
                if (checkedInnerExpr._1 && !correctType) {
                    errors = errors :+ (s"Expression of type char expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                }
				(checkedInnerExpr._1 && correctType, Some(IntCheck(0)))

			case Chr(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(IntCheck(0))
                if (checkedInnerExpr._1 && !correctType) {
                    errors = errors :+ (s"Expression of type int expected, but expression of type ${checkedInnerExpr._2} found!" , expr.pos)
                }
				(checkedInnerExpr._1 && correctType, Some(CharCheck(0)))
            
            case binOpInt: BinOpInt => 
                val checkedExpr1 = analyseExpr(binOpInt.expr1, st)
				val checkedExpr2 = analyseExpr(binOpInt.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(IntCheck(0))
                if (checkedExpr1._1 && !correctType1) {
                    errors = errors :+ (s"Expression of type int expected in ${expr}, but expression of type ${checkedExpr1._2} found!" , expr.pos)
                }
                val correctType2 = checkedExpr2._2 == Some(IntCheck(0))
                if (checkedExpr2._1 && !correctType2) {
                    errors = errors :+ (s"Expression of type int expected in ${expr}, but expression of type ${checkedExpr2._2} found!" , expr.pos)
                }
				(checkedExpr1._1 && checkedExpr2._1 && correctType1 && correctType2, Some(IntCheck(0)))
            
            case binOpComp: BinOpComp => 
                val checkedExpr1 = analyseExpr(binOpComp.expr1, st)
				val checkedExpr2 = analyseExpr(binOpComp.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(IntCheck(0)) || checkedExpr1._2 == Some(CharCheck(0))
                if (checkedExpr1._1 && !correctType1) {
                    errors = errors :+ (s"Expression of type int or char expected in ${expr}, but expression of type ${checkedExpr1._2} found!" , expr.pos)
                }
                val correctType2 = checkedExpr2._2 == Some(IntCheck(0)) || checkedExpr2._2 == Some(CharCheck(0))
                if (checkedExpr2._1 && !correctType2) {
                    errors = errors :+ (s"Expression of type int or char expected in ${expr}, but expression of type ${checkedExpr2._2} found!" , expr.pos)
                }
                val matchingType = checkedExpr1._2 == checkedExpr2._2
                if (correctType1 && correctType2 && !matchingType) {
                    errors = errors :+ (s"Expressions in ${expr} have missmatched types: ${checkedExpr1._2} and ${checkedExpr2._2}!", expr.pos)
                }
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
                            case _ => 
                                errors = errors :+ (s"Expressions in ${expr} have missmatched types: ${checkedExpr1._2} and ${checkedExpr2._2}!", expr.pos)
                                false
                        }
                        case EmptyPairCheck() => checkedExpr1._2.get match {
                            case EmptyPairCheck() => true
                            case PairCheck(type1, type2, nested) => true
                            case _ => 
                                errors = errors :+ (s"Expressions in ${expr} have missmatched types: ${checkedExpr1._2} and ${checkedExpr2._2}!", expr.pos)
                                false
                        }
                        case _ => 
                            val matchingType = checkedExpr1._2 == checkedExpr2._2
                            if (checkedExpr1._1 && checkedExpr2._1 && !matchingType) {
                                errors = errors :+ (s"Expressions in ${expr} have missmatched types: ${checkedExpr1._2} and ${checkedExpr2._2}!", expr.pos)
                            }
                            matchingType
                    }), Some(BoolCheck(0)))
                }
            
            case binOpBool: BinOpBool => 
                var checkedExpr1 = analyseExpr(binOpBool.expr1, st)
				var checkedExpr2 = analyseExpr(binOpBool.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(BoolCheck(0))
                if (checkedExpr1._1 && !correctType1) {
                    errors = errors :+ (s"Expression of type bool expected in ${expr}, but expression of type ${checkedExpr1._2} found!" , expr.pos)
                }
                val correctType2 = checkedExpr2._2 == Some(BoolCheck(0))
                if (checkedExpr2._1 && !correctType2) {
                    errors = errors :+ (s"Expression of type bool expected in ${expr}, but expression of type ${checkedExpr2._2} found!" , expr.pos)
                }
                (checkedExpr1._1 && checkedExpr2._1 && correctType1 && correctType2, Some(BoolCheck(0)))
        }
    }
}