package wacc

import parsley.Parsley, Parsley._
import ast._
import types._
import wacc.functionTable._

object symbolTable {
    /*
        The symbol table takes in its kind as a string, and an option
        of a parent, which will be none for the outer most
        symbol table. The table stores a map of variable names to types
        and a list of its children symbol tables.

        We only store variables within the symbol tables, functions are stored
        in the function table
    */
    class SymbolTable(_scope: String, _parent: Option[SymbolTable]) {
        var variableMap: Map[String, TypeCheck] = Map()
        var children: List[SymbolTable] = List()
        var scope: String = _scope
        var parent: Option[SymbolTable] = _parent

        /*
            The add method adds a variable to the symbol table, returning true
            if the addition succeded, as in there was no variable of that
            name previously added, false otherwise.
        */
        def add(variable: String, varType: TypeCheck): Boolean = {
            variableMap.get(variable) match {
                case None => 
                    variableMap += (variable -> varType)
                    true
                case _ => false
            }
        }

        /*
            The find method finds a variable of a given name in the symbol table,
            searching recursively within the ancestor symbol tables until finding
            the most inner declaration, or returning false is no variable of that
            name is found
        */
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
        /*
            Method that prints the symbol table and its children recursively
        */
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