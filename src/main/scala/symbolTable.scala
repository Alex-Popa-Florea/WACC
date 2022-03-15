package wacc

import parsley.Parsley
import wacc.functionTable._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import Parsley._
import ast._
import types._
import section._
import arrayBounds._

object symbolTable {
    /*
        The symbol table takes in its kind as a string, and an option
        of a parent, which will be none for the outer most
        symbol table. The table stores a map of variable names to types
        and a list of its children symbol tables.

        We only store variables within the symbol tables, functions are stored
        in the function table
    */
    class SymbolTable(private var section: Section, private var parent: Option[SymbolTable]) {
        private var variableMap: Map[String, (TypeCheck, Int, ListBuffer[(Section, ArraySize, Boolean)])] = Map.empty
        private var children: ListBuffer[SymbolTable] = ListBuffer.empty
        private var size: Int = 0

        /*
            The add method adds a variable to the symbol table, returning true
            if the addition succeded, as in there was no variable of that
            name previously added, false otherwise.
        */
        def add(ident: Ident, varType: TypeCheck): Boolean = {
            ident.symbolTable = Some(this)
            variableMap.get(ident.variable) match {
                case None => 
                    varType match {
                        case IntCheck(nested) => 
                            updateSize(this, 4)
                        case BoolCheck(nested) => 
                            if (nested != 0) {
                                updateSize(this, 4)
                            } else {
                                updateSize(this, 1)
                            }  
                        case CharCheck(nested) => 
                            if (nested != 0) {
                                updateSize(this, 4)
                            } else {
                                updateSize(this, 1)
                            }
                        case StrCheck(nested) => 
                            updateSize(this, 4)
                        case PairCheck(type1, type2, nested) => 
                            updateSize(this, 4)
                        case _ =>
                        }
                    variableMap(ident.variable) = (varType, size, ListBuffer((section, Unknown(), true)))
                    true
                case _ => false
            }
        }

        def addChildSt(st: SymbolTable) = {
            children.addOne(st)
        }

        /*
            The find method finds a variable of a given name in the symbol table,
            searching recursively within the ancestor symbol tables until finding
            the most inner declaration, or returning None if no variable of that
            ident is found
        */
        def find(ident: Ident): Option[TypeCheck] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case Some(value) => value.find(ident)
                }
                case _ => 
                    ident.symbolTable = Some(this)
                    Some(foundType.get._1)
            }
        }

        def findAll(ident: Ident, scope: Int, sections: List[Section]): Option[(SymbolTable, (TypeCheck, Int, ListBuffer[(Section, ArraySize, Boolean)]), (Int, List[Section]))] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case Some(value) => 
                        value.findAll(ident, scope + 1, List(this.section)) match {
                            case None => None
                            case Some(nestedFind) => Some(nestedFind._1, nestedFind._2, (nestedFind._3._1, this.section :: nestedFind._3._2))
                        }
                }
                case _ => 
                    Some(this, foundType.get, (scope, List(this.section)))
            }
        }

        /*
            The findId method finds the stack id of a given ident in the symbol table,
            searching recursively within the ancestor symbol tables until finding
            the most inner declaration, or returning None if no variable of that
            ident is found
        */
        def findId(ident: Ident): Option[Int] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case Some(value) => value.findId(ident)
                }
                case _ => 
                    Some(foundType.get._2)
            }
        }

        /*
            Method to return the variable map
        */
        def getVariableMap(): collection.immutable.Map[String, (TypeCheck, Int, ListBuffer[(Section, ArraySize, Boolean)])] = {
            variableMap.toMap
        }

        def getSection(): Section = {
            section
        }

        /*
            Method to return the children symbol tables
        */
        def getChildren(): List[SymbolTable] = {
            children.toList
        }

        /*
            Method to return the number of bytes of the assignments in the symbol tables
        */
        def getSize(): Int = {
            size
        }
        
        /*
            Method to return the size of the symbol table of an ident, accounting for
            offsets caused by inner symbol tables
        */
        def getSizeWithIdent(ident: Ident): Option[Int] = {
            val foundIdent = if (ident.symbolTable == Some(this)) {
                variableMap.get(ident.variable)
            } else {
                None
            }
            foundIdent match {
            case None => parent match {
                case None => None
                case Some(parentExtracted) => parentExtracted.getSizeWithIdent(ident) match {
                    case Some(value) => Some(value + size)
                    case None => None
                }
            }
            case _ => Some(size)
            }
        }

        /*
            Method to increase the size of a symbol table
        */
        def updateSize(symbolTable: SymbolTable, incr: Int): Unit = {
            symbolTable.size += incr
        }


        /*
            Function to update the array bounds of idents in the symbol table, for each scope
        */
        def setArrayScope(ident: Ident, oldValue: (TypeCheck, Int, ListBuffer[(Section, ArraySize, Boolean)]), newArraySize: ListBuffer[(Section, ArraySize, Boolean)], unknownEnd: Boolean): Unit = {
            if (unknownEnd) {
                var i = newArraySize.size - 2
                var ifStatement = false
                while (i >= 0 && !ifStatement) {
                    if (oldValue._3(i)._1 == TrueIfSection()) {
                        ifStatement = true
                    } 
                    newArraySize(i) = (newArraySize(i)._1, Unknown(), false)
                    i -= 1
                }
            }
            variableMap(ident.variable) = (oldValue._1, oldValue._2, newArraySize)
        }

        /*
            Method that prints the symbol table and its children recursively
        */
        def printSymbolTables(st: SymbolTable, nest: Int): Unit = {
            for (i <- 0 to nest) {
                print("  ")
            }
            println(s"- Symbol Table ${st.section.toString()} - ${st}")
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.parent != None) {
                println(s" (i) Symbol Table Parent: ${st.parent.get.section.toString()} - ${st.parent.get}")
            } else {
                println(s" (i) Symbol Table Parent: ${st.parent}")
            }
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.variableMap.size > 0) {
                println(s"(ii) Variables:")
                st.variableMap.zip(0 until st.variableMap.size).foreach { case ((k, (x, _, _)), i) => 
                    for (i <- 0 to nest) {
                        print("  ")
                    }
                    println(s" ${i + 1}. \"$k\": \"${typeCheckToString(x)}\"")
                }
            }
            st.children.map(x => {
                printSymbolTables(x, nest + 1)
            })
        }

        def printSymbolTables2(st: SymbolTable, nest: Int): Unit = {
            for (i <- 0 to nest) {
                print("  ")
            }
            println(s"- Symbol Table ${st.section.toString()} - ${st}")
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.parent != None) {
                println(s" (i) Symbol Table Parent: ${st.parent.get.section.toString()} - ${st.parent.get}")
            } else {
                println(s" (i) Symbol Table Parent: ${st.parent}")
            }
            for (i <- 0 to nest) {
                print("  ")
            }
            if (st.variableMap.size > 0) {
                println(s"(ii) Variables:")
                st.variableMap.zip(0 until st.variableMap.size).foreach { case ((k, (x, _, b)), i) => 
                    for (i <- 0 to nest) {
                        print("  ")
                    }
                    println(s" ${i + 1}. \"$k\": \"${typeCheckToString(x)}\", ${b}")
                }
            }
            st.children.map(x => {
                printSymbolTables(x, nest + 1)
            })
        }
    }
}