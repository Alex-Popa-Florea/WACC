package frontend

import parsley.Parsley
import wacc.ast._
import wacc.functionTable._
import wacc.symbolTable._
import wacc.types._
import wacc.section._
import wacc.arrayBounds._
import wacc.main.ARRAY_BOUNDS

import scala.collection.mutable.ListBuffer

import Parsley._
import parser._

object semanticAnalyser {
    var errors: ListBuffer[(String, (Int, Int))] = ListBuffer.empty
    var returnTypeError: Option[(String, (Int, Int))] = None
    val undeclared = "undeclared: "
    val expression = "Error in expression"
    val mismatch = "Type mismatch"
    val argsIncorrect = "Wrong number of arguments"
    val ranks = "Incorrect number of ranks in array access: "
    
    /*
        Function that takes in a node, symbol table, function table and an optional return type (used
        for checked that return statements within functions return the correct type) as
        parameters, and returns a pair of booleans. The first boolean represents whether the AST is correct
        semantically, while the second boolean represents whether all functions in the AST have a return or exit
        statement, and that main does not have return statements.

        This function is first called on a Begin node of the AST, and then called recursivelly on other
        inner function and statement nodes within the AST.

        It builds the symbol table and function tables throughout the recursive calls.
    */
	def analyse(node: Node, st: SymbolTable, ft: FunctionTable, returnType: Option[TypeCheck]): (Boolean, Boolean) = {
        /*
            We pattern match over nodes in the AST
        */
		node match {
            /*
                For the begin node, we check all functions have distinct name and add them to 
                the function table, then recursively call analyse on the functions and 
                statements inside the two lists of begin (func and stat) 
            */
			case Begin(func, stat) =>
                var addedFunctions = true
                for (function <- func){
                    val addedFunction = ft.add(function.id.variable, extractType(function.t), function.vars.map(x => extractType(x.t)))
                    if (!addedFunction) {
                        addedFunctions = false
                        errors.addOne((s"Function ${function.id.variable} has already been defined, sorry!", node.pos))
                    }
                }
				val functionsChecked = func.map(x => analyse(x, st, ft, None))
                val statsChecked = stat.map(x => analyse(x, st, ft, None)._1)
                functionsChecked.reduceOption((x, y) => ((x._1 && y._1), false)) match {
					case None => (statsChecked.reduce((x, y) => x && y), true)
					case Some(value) => (addedFunctions && value._1 && statsChecked.reduce((x, y) => x && y), functionsChecked.last._2)
				}
            /*
                For the function node, we create a new symbol table whose parent becomes
                the old symbol table, then recursively check its parameters and statements, 
                passing the return type of the function to the statement analysis, 
                and checking that the last statement analysed for the function returns
                true as its second parameter if a return statement is present
            */
			case functionStat: Function => 
                var nst = new SymbolTable(FunctionSection(functionStat.id.variable), Some(st))
                st.addChildSt(nst)
                functionStat.semanticTable = Some(nst)
				val checkedParams = functionStat.vars.reverse.map(x => analyse(x, nst, ft, None)._1).reduceOption((x, y) => x && y)
                nst.updateSize(nst, 4)
				val checkedStats = functionStat.stat.map(x => (analyse(x, nst, ft, Some(extractType(functionStat.t))), x.pos))
                if (!checkedStats.last._1._2) {
                    if (returnTypeError == None) {
                        returnTypeError = Some((s"Function ${functionStat.id.variable} missing return statement", checkedStats.last._2))
                    }
                }
                checkedParams match {
                    case None => (checkedStats.reduce((x, y) => (((x._1._1 && y._1._1), false), y._2))._1._1, checkedStats.last._1._2)
                    case Some(value) => (value && checkedStats.reduce((x, y) => (((x._1._1 && y._1._1), false), y._2))._1._1, checkedStats.last._1._2)
                }
	        /*
                We ensure parameters have unique names
            */			
			case Parameter(t, id) => 
                val addedParam = st.add(id, extractType(t))
                if (!addedParam) {
                    errors.addOne((s"Cannot duplicate parameter name: ${id.variable}", node.pos))
                }
                (addedParam, false)
            /*
                Skip is automatically correct semantically
            */			
			case Skip() => (true, false)
            /*
                An AssignType node adds its identifier to the symbol table with
                the given type, ensuring no identifier of the same name already 
                exists within its symbol table, and calls analyseRHS on the right hand
                side of the assignment, giving it the type of the identifier on
                the left hand side
            */
			case AssignType(t, id, rhs) =>
                val addedVariable = st.add(id, extractType(t))
                if (!addedVariable) {
                    errors.addOne((s"Variable already declared", node.pos))
                }
                val checkedRHS = analyseRHS(rhs, st, ft, extractType(t), id)
                (checkedRHS && addedVariable, false)
             /*
                An Assign node analyses the left hand side of the assignment,
                returning the type of the left hand side if correct semantically
                and calls analyseRHS on the right hand side of the assignment, giving 
                it the type found on the right hand side
            */  
			case Assign(lhs, rhs) => 
                val checkedLHS = analyseLHS(lhs, st)
                checkedLHS._2 match {
                    case None => (false, false)
                    case Some(foundType) => 
                        val checkedRHS = analyseRHS(rhs, st, ft, foundType, lhs)
                        ((checkedLHS._1 && checkedRHS), false)
                }
             /*
                For a Read node we analyse the inner "left hand side" expression
                and ensure it is of the correct types
            */            
			case Read(lhs) =>
                val checkedLHS = analyseLHS(lhs, st)
                val correctType = checkedLHS._2 == Some(IntCheck(0)) || checkedLHS._2 == Some(CharCheck(0))
                if (!correctType && checkedLHS._2 != None) {
                    errors.addOne((s"Expression of type int or char expected in read statement, " +
                      s"but expression of type ${typeCheckToString(checkedLHS._2.get)} found!", node.pos))
                }
                (checkedLHS._1 && correctType, false)
            /*
                For a Free node we analyse the inner expression
                and ensure it is of the correct types
            */
			case Free(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                checkedExpr._2 match {
                    case None => (false, false)
                    case Some(foundType) => foundType match {
                        case baseTypeCheck: BaseTypeCheck => 
                            val correctType = baseTypeCheck.nested > 0
                            if (!correctType && checkedExpr._2 != None) {
                                errors.addOne((s"Array or Pair expected in free statement, " +
                                  s"but expression of type ${typeCheckToString(checkedExpr._2.get)} found!", node.pos))
                            }
                            (correctType, false)
						case PairCheck(_, _, _) => (true, false)
						case EmptyPairCheck() => (true, false)
                    }
                }
            /*
                For a Return node we analyse the inner expression
                and ensure it is of the correct type based on the return type
                of the function we recursed from. If no return type exists, 
                a syntactic error is thrown as return statements are not allowed
                outside functions
            */
			case Return(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                returnType match {
					case Some(foundReturnType) => 
						foundReturnType match {
							case EmptyPairCheck() => 
                                errors.addOne((s"Return type cannot be empty pair literal!", node.pos))
                                (false, true)
                            case PairCheck(_, _, _) =>
                                val correctType = checkedExpr._2 == returnType || checkedExpr._2 == Some(EmptyPairCheck())
                                if (!correctType && checkedExpr._2 != None) {
                                    errors.addOne((s"Expression does not match return type of function, " +
                                      s"expected ${typeCheckToString(foundReturnType)} but expression of type " +
                                      s"${typeCheckToString(checkedExpr._2.get)} found!", node.pos))
                                }
								(checkedExpr._1 && correctType, true)
                            case _ => 
                                val correctType = checkedExpr._2 == returnType
                                if (!correctType && checkedExpr._2 != None) {
                                    errors.addOne((s"Expression does not match return type of function, " +
                                      s"expected ${typeCheckToString(foundReturnType)} but " +
                                      s"expression of type ${typeCheckToString(checkedExpr._2.get)} found!", node.pos))
                                }
                                (checkedExpr._1 && correctType, true)
						}
					case None => 
                        if (returnTypeError == None) {
                            returnTypeError = Some(s"Cannot have return statement in main", node.pos)
                        }
                        (false, false)
				}
            /*
                For an Exit node we analyse the inner expression
                and ensure it is of the correct types
            */
			case Exit(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                val correctType = checkedExpr._2 == Some(IntCheck(0))
                if (!correctType && checkedExpr._2 != None) {
                    errors.addOne((s"Expression of type int expected in exit call, " +
                      s"but expression of type ${typeCheckToString(checkedExpr._2.get)} found!" , node.pos))
                }
                (checkedExpr._1 && correctType, true)
            /*
                For an Print node we analyse the inner expression
            */
			case Print(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)

            /*
                For an Println node we analyse the inner expression
            */
			case Println(expr) => 
                val checkedExpr = analyseExpr(expr, st)
                (checkedExpr._1, false)
            /*
                For an If node we analyse the condition, ensuring
                it is a bool, then recursively analyse the statements in the true 
                and false branches, ensuring they are sematically valid
            */
			case ifStat: If =>
				var trueNst = new SymbolTable(TrueIfSection(), Some(st))
                st.addChildSt(trueNst)
                ifStat.trueSemanticTable = Some(trueNst)
				var falseNst = new SymbolTable(FalseIfSection(), Some(st))
                st.addChildSt(falseNst)
                ifStat.falseSemanticTable = Some(falseNst)
				val conditionCheck = analyseExpr(ifStat.cond, st)
				val trueStatCheck = ifStat.trueStat.map(x => analyse(x, trueNst, ft, returnType))
                val falseStatCheck = ifStat.falseStat.map(x => analyse(x, falseNst, ft, returnType))
                val correctType = conditionCheck._2 == Some(BoolCheck(0))
                if (!correctType && conditionCheck._2 != None) {
                    errors.addOne((s"Expression of type bool expected in if statement condition, " +
                      s"but expression of type ${typeCheckToString(conditionCheck._2.get)} found!", node.pos))
                }
			 	(conditionCheck._1 && correctType && 
                 trueStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1 && 
                 falseStatCheck.reduce((x, y) => ((x._1 && y._1), false))._1, 
                 trueStatCheck.last._2 && falseStatCheck.last._2)
            /*
                For a While node we analyse the condition, ensuring
                it is a bool, then recursively analyse the statements
                in its body, ensuring they are sematically valid
            */
			case whileStat: While => 
                var nst = new SymbolTable(WhileSection(), Some(st))
                st.addChildSt(nst)
                whileStat.semanticTable = Some(nst)
				val conditionCheck = analyseExpr(whileStat.cond, st)
                val correctType = conditionCheck._2 == Some(BoolCheck(0))
                if (!correctType && conditionCheck._2 != None) {
                    errors.addOne((s"Expression of type bool expected in while statement condition," +
                      s" but expression of type ${typeCheckToString(conditionCheck._2.get)} found!", node.pos))
                }
                val correctStats = whileStat.stat.map(x => analyse(x, nst, ft, returnType)._1).reduce((x, y) => x && y)
				(conditionCheck._1 && correctType && 
                 correctStats, false)
            
            /*
                For a NestedBegin node we recursively analyse the statements
                in its body, ensuring they are sematically valid
            */
			case nestedBegin: NestedBegin => 
                var nst = new SymbolTable(NestedProgramSection(), Some(st))
                st.addChildSt(nst)
                nestedBegin.semanticTable = Some(nst)
                val correctStats = nestedBegin.stat.map(x => analyse(x, nst, ft, returnType)._1).reduce((x, y) => x && y)
				(correctStats, false)
			
			case _ => (false, false)
		}
	}

    /*
        Function called within analyse, to analyse the right hand side of assignment
        statements. It takes in the AssignRHS node, the symboltable, the function table
        and the type of the left hand side of the assignment, and returns a boolean if the right
        hand sign is valid semantically and has the same type as the left hand side.
    */
    def analyseRHS(assignRHS: AssignRHS, st: SymbolTable, ft: FunctionTable, lhsType: TypeCheck, arrayIdent: AssignLHS): Boolean = {
         /*
            For a expressions, we call analyseExpr, and check the type of 
            that expr is the same as the given left hand side type.
        */       
        assignRHS match {
            case expr: Expr => 
                val checkedExpr = analyseExpr(expr, st)
                val result = lhsType match {
                    case PairCheck(type1, type2, nested) => 
                        val correctType =  checkedExpr._2 == Some(lhsType) || checkedExpr._2 == Some(EmptyPairCheck())
                        if (!correctType && checkedExpr._2 != None) {
                            errors.addOne((s"Expression of type pair expected in right hand side of assignment, " +
                              s"but expression of type ${typeCheckToString(checkedExpr._2.get)} found!", assignRHS.pos))
                        }
                        (checkedExpr._1 && correctType)
                    case _ => 
                        val correctType = checkedExpr._2 == Some(lhsType)
                        if (!correctType && checkedExpr._2 != None) {
                            errors.addOne((s"Expression of type ${typeCheckToString(lhsType)} expected in right " +
                              s"hand side of assignment, but expression of type ${typeCheckToString(checkedExpr._2.get)} found!", assignRHS.pos))
                        }
                        (checkedExpr._1 && correctType)                      
                }
                if (ARRAY_BOUNDS) {
                    expr match {
                        case arrayElem: ArrayElem => 
                            setArraySize(arrayIdent, st, getArraySize(arrayElem, st))
                        case ident: Ident =>
                            setArraySize(arrayIdent, st, getArraySize(ident, st))
                        case _ =>
                    }
                }
                result
            /*
                For a array liter, we call analyseExpr on the elements within
                the array liter, ensuring they are of the same type as the expected 
                elements within the left hand side type. This also ensures the left
                hand side type is an array
            */
            case ArrayLiter(elements) => 
                lhsType match {
                    case baseTypeCheck: BaseTypeCheck =>
                        if (ARRAY_BOUNDS) {
                            if (baseTypeCheck.nested == 1) {
                                setArraySize(arrayIdent, st, LeafArraySize(elements.size))
                            } else {
                                val sizes = elements.map(expr => expr match {
                                    case ident: Ident => getArraySize(ident, st)
                                    case arrayElem: ArrayElem => getArraySize(arrayElem, st)
                                    case _ => Unknown()
                                })
                                setArraySize(arrayIdent, st, NestedArraySize(elements.size, sizes))
                            }
                        }
                        val correctType = elements.map(x => analyseExpr(x, st) == (true, baseTypeCheck match {
                            case IntCheck(nested) => Some(IntCheck(nested - 1))
                            case BoolCheck(nested) => Some(BoolCheck(nested - 1))
                            case CharCheck(nested) => Some(CharCheck(nested - 1))
                            case StrCheck(nested) => Some(StrCheck(nested - 1))
                        })).reduceOption((x, y) => x && y)
                        correctType match {
                            case None => true
                            case Some(value) => 
                                if (!value ) {
                                    errors.addOne((s"Elements in array must all be of type ${typeCheckToString(baseTypeCheck)}!", assignRHS.pos))
                                }
                                value
                        }
                        
					case PairCheck(type1, type2, nested) => 
                        if (ARRAY_BOUNDS) {
                            val sizes = elements.map(expr => expr match {
                                case ident: Ident => getArraySize(ident, st)
                                case arrayElem: ArrayElem => getArraySize(arrayElem, st)
                                case _ => Unknown()
                            })
                            setArraySize(arrayIdent, st, NestedArraySize(elements.size, sizes))
                        }
						val correctType = elements.map(x => {
							val checkedExpr = analyseExpr(x, st)
							checkedExpr == (true, Some(PairCheck(type1, type2, nested - 1))) || checkedExpr == (true, Some(EmptyPairCheck()))
						}).reduceOption((x, y) => x && y)
                        correctType match {
                            case None => true
                            case Some(value) => 
                                if (!value) {
                                    errors.addOne((s"Elements in array must all be of " +
                                      s"type ${typeCheckToString(PairCheck(type1, type2, nested - 1))}!", assignRHS.pos))
                                }
                                value      
                        }

					case _ => false
				} 
             /*
                For a NewPair node, the inner expresssions are analysed, and
                checked that they are the same as the 
                left hand side type's inner expressions
            */               
            case NewPair(expr1, expr2) => 
                if (ARRAY_BOUNDS) {
                    val expr1Size = expr1 match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem, st)
                        case ident: Ident =>
                            getArraySize(ident, st)
                        case _ =>
                            Unknown()
                    }
                    val expr2Size = expr2 match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem, st)
                        case ident: Ident =>
                            getArraySize(ident, st)
                        case _ =>
                            Unknown()
                    }
                    setArraySize(arrayIdent, st, NodeArraySize(expr1Size, expr2Size))
                }
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
                                errors.addOne((s"New pair must be of type ${typeCheckToString(lhsType)} but is " +
                                  s"of type ${typeCheckToString(PairCheck(foundType1, foundType2, 0))}!", assignRHS.pos)) 
                            }	
                            correctType
                        case _ => false
                    }
                } else {
                    false
                }
            /*
                For a Fst node, the inner expresssion is analysed, and
                checked that the type of its first element is that of the 
                left hand side
            */ 
            case Fst(expr) => 
                if (ARRAY_BOUNDS) {
                    val exprSize = expr match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem, st)
                        case ident: Ident =>
                            getArraySize(ident, st)
                        case _ =>
                            Unknown()
                    }
                    exprSize match {
                        case NodeArraySize(fstArray, sndArray) =>
                            setArraySize(arrayIdent, st, fstArray)
                        case _ => Unknown()
                    }
                }
                val checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case None => false
                    case Some(foundType) => foundType match {
                        case PairCheck(EmptyPairCheck(), _, 0) => 
                            lhsType match {
                                case PairCheck(_, _, 0) => checkedExpr._1
                                case _ => 
                                    errors.addOne((s"First element of input pair should be of type: ${typeCheckToString(lhsType)}, " +
                                      s"but found: pair!", assignRHS.pos))
                                    false
                            }
                        case PairCheck(type1, _, 0) =>
                            val correctType = type1 == lhsType
                            if (!correctType) {
                                errors.addOne((s"First element of input pair should be of type: ${typeCheckToString(lhsType)}, " +
                                  s"but found: ${typeCheckToString(type1)}!", assignRHS.pos))
                            }
                            checkedExpr._1 && correctType
                        case _ => 
                            errors.addOne((s"Input of fst must be of type pair but" +
                              s" found type: ${typeCheckToString(foundType)}!", assignRHS.pos))
                            false
                    }

				}
            /*
                For a Snd node, the inner expresssion is analysed, and
                checked that the type of its second element is that of the 
                left hand side
            */
            case Snd(expr) => 
                if (ARRAY_BOUNDS) {
                    val exprSize = expr match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem, st)
                        case ident: Ident =>
                            getArraySize(ident, st)
                        case _ =>
                            Unknown()
                    }
                    exprSize match {
                        case NodeArraySize(fstArray, sndArray) =>
                            setArraySize(arrayIdent, st, sndArray)
                        case _ => Unknown()
                    }
                }
                val checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case None => false
                    case Some(foundType) => foundType match {
                        case PairCheck(_, EmptyPairCheck(), 0) => 
                            lhsType match {
                                case PairCheck(_, _, 0) => checkedExpr._1
                                case _ => 
                                    errors.addOne((s"Second element of input pair should be of type: ${typeCheckToString(lhsType)}, " +
                                      s"but found: pair!", assignRHS.pos))
                                    false
                            }
                        case PairCheck(_, type2, 0) => 
                            val correctType = type2 == lhsType
                            if (!correctType) {
                                errors.addOne((s"Second element of input pair should be of type: ${typeCheckToString(lhsType)}, " +
                                  s"but found: ${typeCheckToString(type2)}!", assignRHS.pos))
                            }
                            checkedExpr._1 && (type2 == lhsType)
                        case _ => 
                            errors.addOne((s"Input of snd must be of type pair but found type: ${typeCheckToString(foundType)}!", assignRHS.pos))
                            false
                    } 
				}
            /*
                For a Call node, the identifier is found in the function
                table, returning a semantic error if not.
                If it is found, the return type of that function is checked 
                to be equal to the left hand side type, and the given arguments
                are checked to be the same number, and types as the ones found
                for that function in its function table
            */
            case Call(id, args) => 
                if (ARRAY_BOUNDS) {
                    setArraySize(arrayIdent, st, Unknown())
                }
                val checkedArgs = args.map(x => analyseExpr(x, st))
                if (checkedArgs.forall(x => x._1)) {
                    ft.getFunction(id.variable) match {
                        case Some(foundFuncType) =>
                            if (lhsType == EmptyPairCheck()) {
                                foundFuncType._1 match {
                                        case PairCheck(type1, type2, nested) => 
                                            val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                                            if (!checkedNumArgs) {
                                                errors.addOne((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                                            }
                                            val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                            if (checkedNumArgs && !checkArgs) {
                                                errors.addOne((s"Expected argument types: ${foundFuncType._2.map(x=> typeCheckToString(x))}, " +
                                                s"but found: ${checkedArgs.map(x => typeCheckToString(x._2.get))}in call to function ${id.variable}!", assignRHS.pos))
                                            }
                                            val checkedArgsResult = checkedArgs.map(x => x._1).reduceOption((x, y) => x && y)
                                            checkedArgsResult match {
                                                case Some(value) => value && ft.check(id.variable, checkedArgs.map(x => x._2.get))	
                                                case None => ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                            }
                                            
                                        case _ =>
                                            val correctType = foundFuncType._1 == lhsType
                                            if (!correctType) {
                                                errors.addOne((s"Expected type: ${typeCheckToString(lhsType)}, " +
                                                s"actual type of function return: ${typeCheckToString(foundFuncType._1)}!", assignRHS.pos))
                                            }
                                            val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                                            if (!checkedNumArgs) {
                                                errors.addOne((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                                            }
                                            val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                            if (checkedNumArgs && !checkArgs) {
                                                errors.addOne((s"Expected argument types: ${foundFuncType._2.map(x=> typeCheckToString(x))}, " +
                                                s"but found: ${checkedArgs.map(x => typeCheckToString(x._2.get))}in call to function ${id.variable}!", assignRHS.pos))
                                                
                                            }
                                            val checkedArgsResult = checkedArgs.map(x => x._1).reduceOption((x, y) => x && y)
                                            checkedArgsResult match {
                                                case Some(value) => correctType && value && ft.check(id.variable, checkedArgs.map(x => x._2.get))	
                                                case None => correctType && ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                            }
                                }
                            } else {
                                val correctType = foundFuncType._1 == lhsType
                                if (!correctType) {
                                    errors.addOne((s"Expected type: ${typeCheckToString(lhsType)}, " +
                                    s"actual type of function return: ${typeCheckToString(foundFuncType._1)}!", assignRHS.pos))
                                }
                                val checkedNumArgs = ft.checkLength(id.variable, checkedArgs.map(x => x._2.get))
                                if (!checkedNumArgs) {
                                    errors.addOne((s"Wrong number of arguments in call to function ${id.variable}!", assignRHS.pos))
                                }
                                val checkArgs = ft.check(id.variable, checkedArgs.map(x => x._2.get))
                                if (checkedNumArgs && !checkArgs) {
                                    errors.addOne((s"Expected argument types: ${foundFuncType._2.map(x=> typeCheckToString(x))}, " +
                                    s"but found: ${checkedArgs.map(x => typeCheckToString(x._2.get))}in call to function ${id.variable}!", assignRHS.pos))
                                }
                                val checkedArgsResult = checkedArgs.map(x => x._1).reduceOption((x, y) => x && y)
                                checkedArgsResult match {
                                    case Some(value) => correctType && value && checkArgs	
                                    case None => correctType && checkArgs
                                }
                            }
                        case None => 
                            errors.addOne((s"Function ${id.variable} not declared!", assignRHS.pos))
                            false
                    }
                } else {
                    false
                }
        }
    }

    /*
        Function called within analyse, to analyse the left hand side of assignment
        statements. It takes in the AssignLHS node and the symboltable and returns a boolean
        describing whther the node is valid semantically, as well as an optional type
        to then pass to the analyseRHS function to check for matching types.
    */
    def analyseLHS(assignLHS: AssignLHS, st: SymbolTable): (Boolean, Option[TypeCheck]) = {
        assignLHS match {
            /*
                For a Fst node, the inner expresssion is analysed, and
                its type is returned if present
            */
            case Fst(expr) =>
                var checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case Some(foundType) => foundType match {
                        case PairCheck(type1, _, 0) => (checkedExpr._1, Some(type1))
					    case _ => (false, None)
                    }
                    case None => (false, None)
                }
             /*
                For a Snd node, the inner expresssion is analysed, and
                its type is returned if present
            */           
            case Snd(expr) =>
                var checkedExpr = analyseExpr(expr, st)
				checkedExpr._2 match {
                    case Some(foundType) => foundType match {
                        case PairCheck(_, type2, 0) => (checkedExpr._1, Some(type2))
					    case _ => (false, None)
                    }
                    case None => (false, None)
                }
             /*
                For an arrayElem node, the we call analyseExpr on the arrayelem
            */           
            case arrayElem: ArrayElem => analyseExpr(arrayElem, st)
            /*
                For an ident node, the we call ident on the arrayelem
            */
            case ident: Ident => analyseExpr(ident, st)
        }
    }

    /*
        Function called within other analyse functions, as well as recursively, to analyse
        and expression node. It takes in the node and a symbol table, and returns a boolean
        describing whther the node is valid semantically, as well as an optional type
        to then pass to the analyseRHS function to check for matching types.
    */
    def analyseExpr(expr: Expr, st: SymbolTable): (Boolean, Option[TypeCheck]) = {
        expr match {
            // Returning true and the int type
            case IntLiter(_) => (true, Some(IntCheck(0)))
            // Returning true and the bool type
            case BoolLiter(_) => (true, Some(BoolCheck(0)))
            // Returning true and the char type
            case CharLiter(_) => (true, Some(CharCheck(0)))
            // Returning true and the str type
            case StrLiter(_) => (true, Some(StrCheck(0)))
            // Returning true and the empty pair type
            case PairLiter() => (true, Some(EmptyPairCheck()))
            /*
                We check whether the given variable is declared by finding
                it in the current symbol table, and return its found type
            */
            case ident: Ident => 
                val foundVariableType = st.find(ident)
                val isFound = foundVariableType != None
                if (!isFound) {
                    errors.addOne((ident.variable + " " + undeclared, ident.pos))
                }
                (isFound, foundVariableType)
                     
            /*
                We check whether the given array is declared by finding
                it in the current symbol table. We then ensure that all expressions
                are integers, and only ints can index an array, and that the number 
                of indeces is less or equal to the dimension of the array ,
                returning the type of the element found at those indeces
            */                   
            case arrayElem: ArrayElem =>
                    val foundType = st.find(arrayElem.id)
                    foundType match {
                        case None => 
                            errors.addOne((arrayElem.id.variable + undeclared, expr.pos))
                            (false, None)
                        case Some(array) =>
                            if(!(updateArrayElemCheckedBounds(arrayElem, st).forall(identity))) {
                                errors.addOne((s"Out of bounds error in ${arrayElem.id.variable}", arrayElem.exprs(arrayElem.checked._2.indexWhere(x => !x)).pos))
                                                    (false, None)
                                (false, None)
                            } else {
                                array match {
                                    case baseTypeCheck: BaseTypeCheck => 
                                        if (baseTypeCheck.nested >= arrayElem.exprs.size) {
                                            (arrayElem.exprs.map(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))).reduce((x, y) => x && y), baseTypeCheck match {
                                                case IntCheck(nested) => Some(IntCheck(nested - arrayElem.exprs.size))
                                                case BoolCheck(nested) => Some(BoolCheck(nested - arrayElem.exprs.size))
                                                case CharCheck(nested) => Some(CharCheck(nested - arrayElem.exprs.size))
                                                case StrCheck(nested) => Some(StrCheck(nested - arrayElem.exprs.size))
                                            })  
                                        } else {
                                            errors.addOne((s"${arrayElem.id.variable} has type: ${typeCheckToString(array)}, " +
                                              s"which does not have ${arrayElem.exprs.size} ranks!" , expr.pos))
                                            (false, None)
                                        }
                                    case PairCheck(type1, type2, nested) =>
                                        if (nested >= arrayElem.exprs.size) {
                                            (arrayElem.exprs.map(x => (analyseExpr(x, st) == (true, Some(IntCheck(0))))).reduce((x, y) => x && y), Some(PairCheck(type1, type2, nested - arrayElem.exprs.size)))
                                        } else {
                                            errors.addOne((s"${arrayElem.id.variable} has type: ${typeCheckToString(array)}," +
                                              s" which does not have ${arrayElem.exprs.size} ranks!" , expr.pos))
                                            (false, None)
                                        }
                                    case _ => (false, None)
                                }					
                            }
                    }    
                
             /*
                For a Not node, we analyse the inner expression and ensure it is
                a bool, and return a bool
            */           
            case Not(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(BoolCheck(0))
                if (!correctType && checkedInnerExpr._2 != None) {
                    errors.addOne((s"Expression of type bool expected, but expression of " +
                      s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                }
				(checkedInnerExpr._1 && correctType, Some(BoolCheck(0)))
            /*
                For a Neg node, we analyse the inner expression and ensure it is
                an int, and return an int
            */
			case Neg(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(IntCheck(0))
                if (!correctType && checkedInnerExpr._2 != None) {
                    errors.addOne((s"Expression of type int expected, but expression of " +
                      s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                }
				(checkedInnerExpr._1 && correctType, Some(IntCheck(0)))
            /*
                For a Len node, we analyse the inner expression and ensure it is
                an array, and return an int
            */
			case Len(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
				checkedInnerExpr._2 match {
                    case None => (false, None)
                    case Some(extractedCheckedInnerExpr) => extractedCheckedInnerExpr match {
                        case baseTypeCheck: BaseTypeCheck =>
                            val correctNesting = baseTypeCheck.nested > 0
                            if (!correctNesting && checkedInnerExpr._2 != None) {
                                errors.addOne((s"Array  expected, but expression of " +
                                  s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                            } 
                            (checkedInnerExpr._1 && correctNesting, Some(IntCheck(0)))
                        case PairCheck(type1, type2, nested) => 
                            val correctNesting = nested > 0
                            if (!correctNesting && checkedInnerExpr._2 != None) {
                                errors.addOne((s"Array expected, but expression of " +
                                  s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                            }
                            (checkedInnerExpr._1 && correctNesting, Some(IntCheck(0)))
                        case EmptyPairCheck() => (false, None)
                    }
                }
            
            /*
                For a Ord node, we analyse the inner expression and ensure it is
                a char, and return an int
            */
			case Ord(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(CharCheck(0))
                if (!correctType && checkedInnerExpr._2 != None) {
                    errors.addOne((s"Expression of type char expected, but expression of " +
                      s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                }
				(checkedInnerExpr._1 && correctType, Some(IntCheck(0)))
            /*
                For a Chr node, we analyse the inner expression and ensure it is
                an int, and return an char
            */
			case Chr(innerExpr) => 
				val checkedInnerExpr = analyseExpr(innerExpr, st)
                val correctType = checkedInnerExpr._2 == Some(IntCheck(0))
                if (!correctType && checkedInnerExpr._2 != None) {
                    errors.addOne((s"Expression of type int expected, but expression of " +
                      s"type ${typeCheckToString(checkedInnerExpr._2.get)} found!" , expr.pos))
                }
				(checkedInnerExpr._1 && correctType, Some(CharCheck(0)))
            /*
                For a binary operation on ints, we analyse the inner expressions and ensure they are
                ints, and return an int
            */          
            case binOpInt: BinOpInt => 
                val checkedExpr1 = analyseExpr(binOpInt.expr1, st)
				val checkedExpr2 = analyseExpr(binOpInt.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(IntCheck(0))
                if (!correctType1 && checkedExpr1._2 != None) {
                    errors.addOne((s"Expression of type int expected in ${expr}, but expression of " +
                      s"type ${typeCheckToString(checkedExpr1._2.get)} found!" , expr.pos))
                }
                val correctType2 = checkedExpr2._2 == Some(IntCheck(0))
                if (!correctType2 && checkedExpr2._2 != None) {
                    errors.addOne((s"Expression of type int expected in ${expr}, but expression of " +
                      s"type ${typeCheckToString(checkedExpr2._2.get)} found!" , expr.pos))
                }
				(checkedExpr1._1 && checkedExpr2._1 && correctType1 && correctType2, Some(IntCheck(0)))
            /*
                For a comaprison operations, we analyse the inner expressions and ensure they are
                ints or chars, that they are all of the same type, and return an bool
            */       
            case binOpComp: BinOpComp => 
                val checkedExpr1 = analyseExpr(binOpComp.expr1, st)
				val checkedExpr2 = analyseExpr(binOpComp.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(IntCheck(0)) || checkedExpr1._2 == Some(CharCheck(0))
                if (!correctType1 && checkedExpr1._2 != None)   {
                    errors.addOne((s"Expression of type int or char expected in ${expr}, but expression of" +
                      s" type ${typeCheckToString(checkedExpr1._2.get)} found!" , expr.pos))
                }
                val correctType2 = checkedExpr2._2 == Some(IntCheck(0)) || checkedExpr2._2 == Some(CharCheck(0))
                if (!correctType2 && checkedExpr2._2 != None) {
                    errors.addOne((s"Expression of type int or char expected in ${expr}, but expression of " +
                      s"type ${typeCheckToString(checkedExpr2._2.get)} found!" , expr.pos))
                }
                val matchingType = checkedExpr1._2 == checkedExpr2._2
                if (!matchingType && checkedExpr1._2 != None && checkedExpr2._2 != None) {
                    errors.addOne((s"Expressions in ${expr} have missmatched types: ${typeCheckToString(checkedExpr1._2.get)} " +
                      s"and ${typeCheckToString(checkedExpr2._2.get)}!", expr.pos))
                }
				(checkedExpr1._1 && checkedExpr2._1 && (checkedExpr1._2 == checkedExpr2._2) && ((checkedExpr1._2 == Some(IntCheck(0))) || (checkedExpr1._2 == Some(CharCheck(0)))), Some(BoolCheck(0)))
             /*
                For a comaprison operations, we analyse the inner expressions and ensure 
                that they are all of the same type, and return an bool
            */           
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
                                errors.addOne(s"Expressions in ${expr} have missmatched types: " +
                                  s"${typeCheckToString(checkedExpr1._2.get)} and ${typeCheckToString(checkedExpr2._2.get)}!", expr.pos)
                                false
                        }
                        case EmptyPairCheck() => checkedExpr1._2.get match {
                            case EmptyPairCheck() => true
                            case PairCheck(type1, type2, nested) => true
                            case _ => 
                                errors.addOne(s"Expressions in ${expr} have missmatched types: " +
                                  s"${typeCheckToString(checkedExpr1._2.get)} and ${typeCheckToString(checkedExpr2._2.get)}!", expr.pos)
                                false
                        }
                        case _ => 
                            val matchingType = checkedExpr1._2 == checkedExpr2._2
                            if (!matchingType) {
                                errors.addOne(s"Expressions in ${expr} have missmatched types: " +
                                  s"${typeCheckToString(checkedExpr1._2.get)} and ${typeCheckToString(checkedExpr2._2.get)}!", expr.pos)
                            }
                            matchingType
                    }), Some(BoolCheck(0)))
                }
            /*
                For a binary operation on bools, we analyse the inner expressions and ensure they are
                bools, and return an bool
            */               
            case binOpBool: BinOpBool => 
                var checkedExpr1 = analyseExpr(binOpBool.expr1, st)
				var checkedExpr2 = analyseExpr(binOpBool.expr2, st)
                val correctType1 = checkedExpr1._2 == Some(BoolCheck(0))
                if (!correctType1 && checkedExpr1._2 != None) {
                    errors.addOne(s"Expression of type bool expected in ${expr}, but expression of" +
                      s" type ${typeCheckToString(checkedExpr1._2.get)}  found!" , expr.pos)
                }
                val correctType2 = checkedExpr2._2 == Some(BoolCheck(0))
                if (!correctType2 && checkedExpr2._2 != None) {
                    errors.addOne(s"Expression of type bool expected in ${expr}, but expression of " +
                      s"type ${typeCheckToString(checkedExpr2._2.get)}  found!" , expr.pos)
                }
                (checkedExpr1._1 && checkedExpr2._1 && correctType1 && correctType2, Some(BoolCheck(0)))
        }
    }
}
