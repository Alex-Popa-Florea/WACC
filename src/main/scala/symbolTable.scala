package wacc

import parsley.Parsley, Parsley._
import ast._
import types._
import wacc.functionTable._

object symbolTable {
    class SymbolTable(_scope: String, _parent: Option[SymbolTable]) {
        var variableMap: Map[String, TypeCheck] = Map()
        var children: List[SymbolTable] = List()
        var scope: String = _scope
        var parent: Option[SymbolTable] = _parent

        def add(variable: String, varType: TypeCheck): Boolean = {
            variableMap.get(variable) match {
                case None => 
                    variableMap += (variable -> varType)
                    true
                case _ => false
            }
        }

        def find(variable: String): Option[TypeCheck] = {
            var foundType = variableMap.get(variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case _ => parent.get.find(variable)
                }
                case _ => foundType
            }
        }

        def printSymbolTables(st: SymbolTable, nest: Int): Unit = {
            for (i <- 0 to nest) {
                print("  ")
            }
            println(s"- Symbol Table ${st.scope} - ${st}")
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.parent != None) {
                println(s" (i) Symbol Table Parent: ${st.parent.get.scope} - ${st.parent.get}")
            } else {
                println(s" (i) Symbol Table Parent: ${st.parent}")
            }
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.variableMap.size > 0) {
                println(s"(ii) Variables:")
                st.variableMap.zip(0 until st.variableMap.size).foreach { case ((k, x), i) => 
                    for (i <- 0 to nest) {
                        print("  ")
                    }
                    println(s" ${i + 1}. \"$k\":")
                }
            }
            st.children.map(x => {
                printSymbolTables(x, nest + 1)
            })
        }
    }
}