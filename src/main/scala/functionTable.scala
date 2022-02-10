package wacc

import parsley.Parsley, Parsley._
import ast._
import types._

object functionTable {
    /*
        The function table stores a map of function names, to return type and a
        list of types of the parameters for that function.

        It is a unique global table 
    */
    class FunctionTable() {
        var funcMap: Map[String, (TypeCheck, List[TypeCheck])] = Map()
        /*
            The add method adds a function to the function table, returning true
            if the addition succeded, as in there was no function of that
            name previously added, false otherwise.
        */
        def add(funcName: String, funcType: TypeCheck, paramTypes: List[TypeCheck]): Boolean = {
            funcMap.get(funcName) match {
                case None => 
                    funcMap += (funcName -> (funcType, paramTypes))
                    true
                case _ => false
            }
            
        }
        
        /*
            Method that checks the number of arguments passed is the same as
            the expected number of arguments for a given function
        */
        def checkLength(funcName: String, argTypes: List[TypeCheck]): Boolean = {
            funcMap.get(funcName) match {
                case None => false
                case Some(foundArgs) => (foundArgs._2.length == argTypes.length)
            }
        }
        /*
            Method that checks the arguments passed correspond to the types
            of the arguments stored for the given function in the function table
        */
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
        /*
            Method that prints the function table
        */
        def printFunctionTables(): Unit = {
            println(s"  - Functions: ")
            println("")
            funcMap.zip(0 until funcMap.size).foreach({ case ((k, v), i) => 
                println(s"    ${i + 1}. \"$k\":")
                println(s"\t(i) Return Type: ")
                println(s"\t - ${v._1}")
                println(s"\t(ii) Parameter Types: ")
                v._2.foreach(x => 
                    println(s"\t - ${x}")    
                )
                println("")
            })
        }
    }
}