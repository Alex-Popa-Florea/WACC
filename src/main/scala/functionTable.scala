package wacc

import parsley.Parsley, Parsley._
import ast._
import types._

object functionTable {
    class FunctionTable() {
        var funcMap: Map[String, (TypeCheck, List[TypeCheck])] = Map()

        def add(funcName: String, funcType: TypeCheck, paramTypes: List[TypeCheck]): Boolean = {
            funcMap.get(funcName) match {
                case None => 
                    funcMap += (funcName -> (funcType, paramTypes))
                    true
                case _ => false
            }
            
        }

        def check(funcName: String, argTypes: List[TypeCheck]): Boolean = {
            funcMap.get(funcName) match {
                case None => false
                case Some(foundArgs) =>
                    var equality = true
                    if (foundArgs._2.length != argTypes.length) {
                        return false
                    }
                    for (i <- 0 to (foundArgs._2.length - 1)) {
                        foundArgs._2.lift(i) match {
                            case None => false
                            case Some(arg) => 
                                arg match {
                                    case PairCheck(_, _, _) => equality &= (argTypes.lift(i) == Some(arg) || (argTypes.lift(i) == Some(EmptyPairCheck())))
                                    case _ => equality &= (argTypes.lift(i) == Some(arg))
                            }
                        }
                    }
                    equality
            }
        }

        def printFunctionTables(): Unit = {
            println(s"-- Function Table:")
            println(s"--- Functions: ")
            println("")
            funcMap.foreach({ case (k, v) => 
                println(s"---- \"$k\":")
                println(s"----- Return Type: ")
                println(s"     - ${v._1}")
                println(s"----- Parameter Types: ")
                v._2.foreach(x => 
                    println(s"     - ${x}")    
                )
                println("")
            })
        }
    }
}