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
            println("")
            for (i <- 0 to nest) {
                print("|--")
            }
            println(s" Symbol Table: ${st.scope} - ${st}")
            for (i <- 0 to nest) {
                print("|--")
            }
            if (st.parent != None) {
                println(s"- Symbol Table Parent: ${st.parent.get.scope} - ${st.parent.get}")
            } else {
                println(s"- Symbol Table Parent: ${st.parent}")
            }
            for (i <- 0 to nest) {
                print("|--")
            }
            println(s"- Variables:")
            if (st.variableMap.size == 0) {
                println(s"      - None")
            }
            st.variableMap.foreach { case (k, x) => 
                println(s"      - $k -> $x")
            }
            st.children.map(x => {
                printSymbolTables(x, nest + 1)
            })
        }
    }
}