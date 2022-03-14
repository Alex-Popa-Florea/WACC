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

        def checkBounds(ident: Ident, indexes: List[Option[Int]], fstSnd: Option[Boolean]): (List[Boolean], List[Boolean]) = {
            updateArrayScope(ident, this, None, false)
            var foundTableOption = findAll(ident, 0, List.empty)
            foundTableOption match {
                case None => (List(false), List(false))
                case Some(foundTable) =>
                    fstSnd match {
                        case None => 
                            checkBoundsFromType(foundTable._2._3.last._2, indexes)
                        case Some(fst) =>
                            if (fst) {
                                foundTable._2._3.last._2 match {
                                    case NodeArraySize(fstArray, sndArray) => checkBoundsFromType(fstArray, indexes)
                                    case _ => (List(false), List(false))
                                }
                                
                            } else {
                                foundTable._2._3.last._2 match {
                                    case NodeArraySize(fstArray, sndArray) => checkBoundsFromType(sndArray, indexes)
                                    case _ => (List(false), List(false))
                                }
                            }
                    }
                    
            }
        }

        def checkBoundsFromType(arraySize: ArraySize, indexes: List[Option[Int]]): (List[Boolean], List[Boolean]) = {
            if (indexes.nonEmpty) {
                indexes(0) match {
                    case None => 
                        (List(false), List(true))
                    case Some(index) =>
                        arraySize match {
                        case LeafArraySize(size) => 
                            (List(true), List(size > index && indexes.size == 1 && index >= 0))
                        case NestedArraySize(size, innerNests) => 
                            if (size > index && index >= 0) {
                                val innerCheck = checkBoundsFromType(innerNests(index), indexes.tail)
                                (true :: innerCheck._1, true :: innerCheck._2)
                            } else {
                                (List(true), List(false))
                            }
                        case _ => 
                            (List(false), List(true))
                    }
                }
            } else {
                (List.empty, List.empty)
            }
        }

        def getArraySize(arrayIdent: AssignLHS): ArraySize = {
            arrayIdent match {
                case ident: Ident =>
                    getBounds(ident, 0, List.empty)
                case ArrayElem(id, exprs) => 
                    val indexes = exprs.map(expr => expr match {
                        case IntLiter(x) => Some(x)
                        case _ => None
                    })
                    if (!indexes.contains(None)) {
                        getBounds(id, 0, indexes.map(optionInt => optionInt.get))
                    } else {
                        Unknown()
                    }
                case Fst(expr) => 
                    val exprSize = expr match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem)
                        case ident: Ident =>
                            getArraySize(ident)
                        case _ =>
                            Unknown()
                    }
                    exprSize match {
                        case NodeArraySize(fstArray, sndArray) => fstArray
                        case _ => Unknown()
                    }
                case Snd(expr) => 
                    val exprSize = expr match {
                        case arrayElem: ArrayElem => 
                            getArraySize(arrayElem)
                        case ident: Ident =>
                            getArraySize(ident)
                        case _ =>
                            Unknown()
                    }
                    exprSize match {
                        case NodeArraySize(fstArray, sndArray) => sndArray
                        case _ => Unknown()
                    }
            }
        }

        def getBounds(ident: Ident, scope: Int, indexes: List[Int]): ArraySize = {
            var foundTableOption = findAll(ident, 0, List.empty)
            foundTableOption match {
                case None => Unknown()
                case Some(foundTable) =>
                    val oldArraySize = foundTable._2._3.last
                    getBoundsFromType(oldArraySize._2, foundTable._2._1, 0, indexes)
            }
        }

        def getBoundsFromType(arraySize: ArraySize, typeCheck: TypeCheck, nest: Int, indexes: List[Int]): ArraySize = {
            if (nest != indexes.size) {
                arraySize match {
                    case NestedArraySize(size, innerNests) => 
                        getBoundsFromType(innerNests(indexes(nest)), typeCheck, nest + 1, indexes)
                    case _ => Unknown()
                }
            } else {
                arraySize
            }
        }

        def setArraySize(arrayIdent: AssignLHS, newArraySize: ArraySize): Unit = {
            arrayIdent match {
                case ident: Ident =>
                    generateBounds(ident, newArraySize, List.empty, None)
                case ArrayElem(id, exprs) => 
                    val indexes = exprs.map(expr => expr match {
                        case IntLiter(x) => Some(x)
                        case _ => None
                    })
                    if (!indexes.contains(None)) {
                        generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), None)
                    }
                case Fst(expr) => 
                    expr match {
                        case ident: Ident =>
                            generateBounds(ident, newArraySize, List.empty, Some(true))
                        case ArrayElem(id, exprs) => 
                            val indexes = exprs.map(expr => expr match {
                                case IntLiter(x) => Some(x)
                                case _ => None
                            })
                            if (!indexes.contains(None)) {
                                generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), Some(true))
                            }
                        case _ =>
                            Unknown()
                    }
                case Snd(expr) => 
                    expr match {
                        case ident: Ident =>
                            generateBounds(ident, newArraySize, List.empty, Some(false))
                        case ArrayElem(id, exprs) => 
                            val indexes = exprs.map(expr => expr match {
                                case IntLiter(x) => Some(x)
                                case _ => None
                            })
                            if (!indexes.contains(None)) {
                                generateBounds(id, newArraySize, indexes.map(optionInt => optionInt.get), Some(false))
                            }
                        case _ =>
                            Unknown()
                    }
            }
        }

        def generateBounds(ident: Ident, newArraySize: ArraySize, indexes: List[Int], fstSnd: Option[Boolean]): Unit = {
            updateArrayScope(ident, this, None, true)
            var foundTableOption = findAll(ident, 0, List.empty)
            foundTableOption match {
                case None => 
                case Some(foundTable) =>
                    fstSnd match {
                        case None => 
                            val oldArraySize = foundTable._2._3.last
                            val generatedBounds = generateBoundsFromType(oldArraySize._2, foundTable._2._1, 0, newArraySize, indexes)
                            updateArrayScope(ident, this, Some(generatedBounds), true)
                        case Some(fst) =>
                            if (fst) {
                                foundTable._2._1 match {
                                    case PairCheck(type1, _, nested) =>
                                        val oldArraySize = foundTable._2._3.last
                                        val generatedBounds = oldArraySize._2 match {
                                            case NodeArraySize(fstArray, sndArray) => 
                                                NodeArraySize(generateBoundsFromType(fstArray, type1, 0, newArraySize, indexes), sndArray)
                                                
                                            case _ => 
                                                NodeArraySize(generateBoundsFromType(Unknown(), type1, 0, newArraySize, indexes), Unknown())
                                        }
                                        updateArrayScope(ident, this, Some(generatedBounds), true)
                                    case _ =>
                                }
                            } else {
                                foundTable._2._1 match {
                                    case PairCheck(_, type2, nested) =>
                                        val oldArraySize = foundTable._2._3.last
                                        val generatedBounds = oldArraySize._2 match {
                                            case NodeArraySize(fstArray, sndArray) => 
                                                NodeArraySize(fstArray, generateBoundsFromType(sndArray, type2, 0, newArraySize, indexes))
                                            case _ => 
                                                NodeArraySize(Unknown(), generateBoundsFromType(Unknown(), type2, 0, newArraySize, indexes))
                                        }
                                        updateArrayScope(ident, this, Some(generatedBounds), true)
                                    case _ =>
                                }
                            }
                    }
            }
        }

        def generateBoundsFromType(arraySize: ArraySize, typeCheck: TypeCheck, nest: Int, newArraySize: ArraySize, indexes: List[Int]): ArraySize = {
            if (nest != indexes.size) {
                arraySize match {
                    case NestedArraySize(size, innerNests) => 
                        val newInnerSize: ListBuffer[ArraySize] = ListBuffer.empty
                        newInnerSize.addAll(innerNests)
                        newInnerSize(indexes(nest)) = generateBoundsFromType(innerNests(indexes(nest)), typeCheck, nest + 1, newArraySize, indexes)
                        NestedArraySize(size, newInnerSize.toList)
                    case _ => Unknown()
                }
            } else {
                typeCheck match {
                    case baseTypeCheck: BaseTypeCheck =>
                        if (baseTypeCheck.nested <= nest + 1) {
                            newArraySize
                        } else {
                            newArraySize match {
                                case Unknown() => Unknown()
                                case LeafArraySize(size) => NestedArraySize(size, List.fill(size)(Unknown()))
                                case NestedArraySize(size, innerNests) => NestedArraySize(size, innerNests)
                                case NodeArraySize(fstArray, sndArray) => Unknown()
                            }
                        }
                    case PairCheck(type1, type2, nested) =>
                        if (nested <= nest + 1) {
                            newArraySize
                        } else {
                            newArraySize match {
                                case Unknown() => Unknown()
                                case LeafArraySize(size) => NestedArraySize(size, List.fill(size)(Unknown()))
                                case NestedArraySize(size, innerNests) => NestedArraySize(size, innerNests)
                                case NodeArraySize(fstArray, sndArray) => Unknown()
                            }
                        }
                    case _ => Unknown()
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