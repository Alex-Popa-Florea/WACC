package wacc

import parsley.Parsley
import wacc.functionTable._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

import Parsley._
import ast._
import types._
import section._

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
        private var variableMap: Map[String, (TypeCheck, Int)] = Map.empty
        private var children: ListBuffer[SymbolTable] = ListBuffer.empty
        private var size: Int = section match {
            case FunctionSection(name) => 0
            case _ => parent match {
                case Some(value) => value.getSize()
                case None => 0
            }
        }

        /*
            The add method adds a variable to the symbol table, returning true
            if the addition succeded, as in there was no variable of that
            name previously added, false otherwise.
        */
        def add(ident: Ident, varType: TypeCheck): Boolean = {
            ident.semanticTable = Some(this)
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
                    variableMap(ident.variable) = (varType, size)
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
            the most inner declaration, or returning false is no variable of that
            name is found
        */
        def find(ident: Ident): Option[TypeCheck] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case _ => parent.get.find(ident)
                }
                case _ => 
                    ident.semanticTable = Some(this)
                    Some(foundType.get._1)
            }
        }

        def findId(ident: Ident): Option[Int] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case _ => parent.get.findId(ident)
                }
                case _ => 
                    ident.semanticTable = Some(this)
                    Some(foundType.get._2)
            }
        }

        def getSection(): Section = {
            section
        }

        def getVariableMap(): collection.immutable.Map[String, (TypeCheck, Int)] = {
            variableMap.toMap
        }

        def getChildren(): List[SymbolTable] = {
            children.toList
        }

        def getSize(): Int = {
            size
        }

        def updateSize(symbolTable: SymbolTable, incr: Int): Unit = {
            symbolTable.size += incr
            symbolTable.getSection() match {
                case FunctionSection(name) =>
                case _ => 
                    symbolTable.parent match {
                        case Some(parent) => 
                            updateSize(parent, incr)
                        case None => 
                    }
            }
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
                st.variableMap.zip(0 until st.variableMap.size).foreach { case ((k, (x, _)), i) => 
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
    }
}