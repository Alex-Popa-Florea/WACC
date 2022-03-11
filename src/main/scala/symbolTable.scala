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
        private var variableMap: Map[String, (TypeCheck, Int, ArraySize)] = Map.empty
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
                    variableMap(ident.variable) = (varType, size, Unknown())
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

        def findAll(ident: Ident): Option[(Map[String, (TypeCheck, Int, ArraySize)], (TypeCheck, Int, ArraySize))] = {
            var foundType = variableMap.get(ident.variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case Some(value) => value.findAll(ident)
                }
                case _ => 
                    Some(variableMap, foundType.get)
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
        def getVariableMap(): collection.immutable.Map[String, (TypeCheck, Int, ArraySize)] = {
            variableMap.toMap
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
        

        def setArraySize(arrayIdent: AssignLHS, newArraySize: Int): (Boolean, Boolean) = {
            arrayIdent match {
                case ArrayElem(id, exprs) => 
                    val indexes = exprs.map(expr => expr match {
                        case IntLiter(x) => Some(x)
                        case _ => None
                    })
                    if (!indexes.contains(None)) {
                        val checkedBounds = checkBounds(id, indexes.map(optionInt => optionInt.get), None)
                        if (checkedBounds._1) {
                            if (checkedBounds._2) {
                                generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), None)
                            }
                        }
                        checkedBounds
                    } else {
                        (false, true)
                    }
                case ident: Ident =>
                    findAll(ident) match {
                        case None => (false, false)
                        case Some(foundValue) => 
                            foundValue._1(ident.variable) = (foundValue._2._1, foundValue._2._2, LeafArraySize(newArraySize)) 
                            (false, true)
                    }
                case Fst(expr) => expr match {
                    case ArrayElem(id, exprs) =>
                        val indexes = exprs.map(expr => expr match {
                            case IntLiter(x) => Some(x)
                            case _ => None
                        })
                        if (!indexes.contains(None)) {
                            val checkedBounds = checkBounds(id, indexes.map(optionInt => optionInt.get), Some(true))
                            if (checkedBounds._1) {
                                if (checkedBounds._2) {
                                    generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), Some(true))
                                }
                            }
                            checkedBounds
                        } else {
                            (false, true)
                        }
                    case ident: Ident =>
                        findAll(ident) match {
                        case None => (false, false)
                            case Some(foundValue) => 
                                foundValue._2._3 match {
                                    case NodeArraySize(fstArray, sndArray) =>
                                        foundValue._1(ident.variable) = (foundValue._2._1, foundValue._2._2, NodeArraySize(LeafArraySize(newArraySize), sndArray)) 
                                    case _ =>
                                }
                                (false, true)
                        }
                    case _ => (false, true)
                }
                case Snd(expr) => expr match {
                    case ArrayElem(id, exprs) =>
                        val indexes = exprs.map(expr => expr match {
                            case IntLiter(x) => Some(x)
                            case _ => None
                        })
                        if (!indexes.contains(None)) {
                            val checkedBounds = checkBounds(id, indexes.map(optionInt => optionInt.get), Some(false))
                            if (checkedBounds._1) {
                                if (checkedBounds._2) {
                                    generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), Some(false))
                                }
                            }
                            checkedBounds
                        } else {
                            (false, true)
                        }
                    case ident: Ident =>
                        findAll(ident) match {
                        case None => (false, false)
                            case Some(foundValue) => 
                                foundValue._2._3 match {
                                    case NodeArraySize(fstArray, sndArray) =>
                                        foundValue._1(ident.variable) = (foundValue._2._1, foundValue._2._2, NodeArraySize(fstArray, LeafArraySize(newArraySize))) 
                                    case _ =>
                                }
                                (false, true)
                        }
                    case _ => (false, true)
                }
            }
        }

        def checkBounds(ident: Ident, indexes: List[Int], fstSnd: Option[Boolean]): (Boolean, Boolean) = {
            var foundItemOption = variableMap.get(ident.variable)
            foundItemOption match {
                case None => parent match {
                    case None => (false, false)
                    case Some(value) => value.checkBounds(ident, indexes, fstSnd)
                }
                case Some(foundItem) => 
                    fstSnd match {
                        case None => checkBoundsFromType(foundItem._3, indexes)
                        case Some(fst) =>
                            
                            if (fst) {
                                foundItem._3 match {
                                    case NodeArraySize(fstArray, sndArray) => checkBoundsFromType(fstArray, indexes)
                                    case _ => (false, true)
                                }
                                
                            } else {
                                foundItem._3 match {
                                    case NodeArraySize(fstArray, sndArray) => checkBoundsFromType(sndArray, indexes)
                                    case _ => (false, true)
                                }
                            }
                    }
                    
            }
        }

        def checkBoundsFromType(arraySize: ArraySize, indexes: List[Int]): (Boolean, Boolean) = {
            arraySize match {
                case LeafArraySize(size) => (true, size > indexes(0) && indexes.size == 1 && indexes(0) >= 0)
                case NestedArraySize(size, innerNests) => 
                    if (size > indexes(0) && indexes(0) >= 0) {
                        checkBoundsFromType(innerNests(indexes(0)), indexes.tail)
                    } else {
                        (true, false)
                    }
                case _ => (false, true)
            }
        }

        def generateBounds(ident: Ident, newArraySize: Int, indexes: List[Int], fstSnd: Option[Boolean]): Unit = {
            var foundItemOption = variableMap.get(ident.variable)
            foundItemOption match {
                case None => parent match {
                    case None =>
                    case Some(value) => value.generateBounds(ident, newArraySize, indexes, fstSnd)
                }
                case Some(foundItem) => 
                    fstSnd match {
                        case None => variableMap(ident.variable) = (foundItem._1, foundItem._2, generateBoundsFromType(foundItem._3, newArraySize, indexes))
                        case Some(fst) =>
                            variableMap(ident.variable) = if (fst) {
                                foundItem._3 match {
                                    case NodeArraySize(fstArray, sndArray) =>
                                        (foundItem._1, foundItem._2, NodeArraySize(generateBoundsFromType(fstArray, newArraySize, indexes), sndArray))
                                    case _ =>
                                        (foundItem._1, foundItem._2, NodeArraySize(generateBoundsFromType(Unknown(), newArraySize, indexes), Unknown()))
                                }
                                
                            } else {
                                foundItem._3 match {
                                    case NodeArraySize(fstArray, sndArray) =>
                                        (foundItem._1, foundItem._2, NodeArraySize(fstArray, generateBoundsFromType(sndArray, newArraySize, indexes)))
                                    case _ =>
                                        (foundItem._1, foundItem._2, NodeArraySize(Unknown(), generateBoundsFromType(foundItem._3, newArraySize, indexes)))
                                }
                            }
                    }
                    
            }
        }

        def generateBoundsFromType(arraySize: ArraySize, newArraySize: Int, indexes: List[Int]): ArraySize = {
            arraySize match {
                case LeafArraySize(size) => LeafArraySize(newArraySize)
                case NestedArraySize(size, innerNests) => 
                    innerNests(indexes(0)) = generateBoundsFromType(innerNests(indexes(0)), newArraySize, indexes.tail)
                    NestedArraySize(size, innerNests)
                case _ => Unknown()
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
    }
}