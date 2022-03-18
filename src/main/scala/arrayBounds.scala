package wacc

import ast._
import symbolTable._
import types._
import scala.collection.mutable.ListBuffer
import wacc.section._

object arrayBounds {

    /*
        Sealed trait to store the size of an array, as well as the size of inner arrays and arrays within pairs
    */
    sealed trait ArraySize

    case class Unknown() extends ArraySize

    case class LeafArraySize(size: Int) extends ArraySize 
    
    case class NestedArraySize(size: Int, innerNests: List[ArraySize]) extends ArraySize

    case class NodeArraySize(fstArray: ArraySize, sndArray: ArraySize) extends ArraySize

    /*
        Function to update array bounds when entering and exiting scopes
    */
    def updateArrayScope(ident: Ident, st: SymbolTable, newArraySizeOption: Option[ArraySize], set: Boolean): Unit = {
        var setUnknown = false
        val foundTable = st.findAll(ident, 0, List.empty)
        foundTable match {
            case None => 
            case Some(table) =>
                val newArraySizeArray = table._2._3
                var lastIndex = newArraySizeArray.size - 1
                val index = table._3._1
                val sections = table._3._2
                if (lastIndex > index) {
                    newArraySizeArray.remove(index + 1, lastIndex - index)
                    lastIndex = newArraySizeArray.size - 1
                }
                if (st.getSection() == FalseIfSection() && newArraySizeArray(lastIndex)._1 == TrueIfSection()) {
                    if (newArraySizeArray(lastIndex)._3) {
                        setUnknown = true
                    }
                    newArraySizeArray.remove(lastIndex)
                    lastIndex = newArraySizeArray.size - 1
                }
                var j = sections.size - index
                if (lastIndex < index) {
                    for (i <- (lastIndex + 1) to (index - 1)) {
                        val lastElem = newArraySizeArray(lastIndex)
                        newArraySizeArray.addOne((sections(j), lastElem._2, lastElem._3))
                        j -= 1
                    }
                }
                newArraySizeOption match {
                    case None => 
                        if (lastIndex < index) {
                            val lastElem = newArraySizeArray(lastIndex)
                            newArraySizeArray.addOne((sections(j), lastElem._2, lastElem._3))
                        }
                    case Some(newArraySize) =>
                        if (lastIndex < index) {
                            newArraySizeArray.addOne((st.getSection(), newArraySize, set))
                        } else if (lastIndex == index) {
                            newArraySizeArray(index) = (st.getSection(), newArraySize, set)
                        }
                }
                if (set || setUnknown) {
                    st.getSection() match {
                        case FalseIfSection() =>
                            table._1.setArrayScope(ident, table._2, newArraySizeArray, true)
                        case WhileSection() =>
                            table._1.setArrayScope(ident, table._2, newArraySizeArray, true)
                        case _ =>
                            table._1.setArrayScope(ident, table._2, newArraySizeArray, false)
                    }
                } else {
                    table._1.setArrayScope(ident, table._2, newArraySizeArray, false)
                }
        }
    }

    /*
        Function to update whether an array access is valid
    */
    def updateArrayElemCheckedBounds(arrayElem: ArrayElem, st: SymbolTable): List[Boolean] = {
        val indexes = arrayElem.exprs.map(expr => expr match {
            case IntLiter(x) => Some(x)
            case _ => None
        })
        val checkedBounds = checkBounds(arrayElem.id, st, indexes, None)
        val checks: ListBuffer[Boolean] = ListBuffer.empty
        for (i <- 0 to checkedBounds._1.size - 1) {
            checks.addOne(!checkedBounds._1(i) || checkedBounds._2(i))
        }
        arrayElem.checked = checkedBounds
        checks.toList
    }

    /*
        Function to check the bounds of an ident
    */
    def checkBounds(ident: Ident, st: SymbolTable, indexes: List[Option[Int]], fstSnd: Option[Boolean]): (List[Boolean], List[Boolean]) = {
        updateArrayScope(ident, st, None, false)
        var foundTableOption = st.findAll(ident, 0, List.empty)
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

    /*
        Recursive function to check the bounds from an arraysize
    */
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
                        case _ => (List(false), List(true))
                }
            }
        } else {
            (List.empty, List.empty)
        }
    }

    /*
        Function to get the arraysize of an array ident
    */
    def getArraySize(arrayIdent: AssignLHS, st: SymbolTable): ArraySize = {
        arrayIdent match {
            case ident: Ident =>
                getBounds(ident, st, List.empty)
            case ArrayElem(id, exprs) => 
                val indexes = exprs.map(expr => expr match {
                    case IntLiter(x) => Some(x)
                    case _ => None
                })
                if (!indexes.contains(None)) {
                    getBounds(id, st, indexes.map(optionInt => optionInt.get))
                } else {
                    Unknown()
                }
            case Fst(expr) => 
                val exprSize = expr match {
                    case arrayElem: ArrayElem => 
                        getArraySize(arrayElem, st)
                    case ident: Ident =>
                        getArraySize(ident, st)
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
                        getArraySize(arrayElem, st)
                    case ident: Ident =>
                        getArraySize(ident, st)
                    case _ =>
                        Unknown()
                }
                exprSize match {
                    case NodeArraySize(fstArray, sndArray) => sndArray
                    case _ => Unknown()
                }
        }
    }

    /*
        Function to get the bounds from an ident
    */
    def getBounds(ident: Ident, st: SymbolTable, indexes: List[Int]): ArraySize = {
        var foundTableOption = st.findAll(ident, 0, List.empty)
        foundTableOption match {
            case None => Unknown()
            case Some(foundTable) =>
                val oldArraySize = foundTable._2._3.last
                getBoundsFromType(oldArraySize._2, foundTable._2._1, 0, indexes)
        }
    }

    /*
        Recursive function to get the bounds from an arraysize
    */
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

    /*
        Function to set the arraysize of an array ident
    */
    def setArraySize(arrayIdent: AssignLHS, st: SymbolTable, newArraySize: ArraySize): Unit = {
        arrayIdent match {
            case ident: Ident =>
                setBounds(ident, st, newArraySize, List.empty, None)
            case ArrayElem(id, exprs) => 
                val indexes = exprs.map(expr => expr match {
                    case IntLiter(x) => Some(x)
                    case _ => None
                })
                if (!indexes.contains(None)) {
                    setBounds(id, st, newArraySize, indexes.map(optionInt => optionInt.get), None)
                }
            case Fst(expr) => 
                expr match {
                    case ident: Ident =>
                        setBounds(ident, st, newArraySize, List.empty, Some(true))
                    case ArrayElem(id, exprs) => 
                        val indexes = exprs.map(expr => expr match {
                            case IntLiter(x) => Some(x)
                            case _ => None
                        })
                        if (!indexes.contains(None)) {
                            setBounds(id, st, newArraySize, indexes.map(optionInt => optionInt.get), Some(true))
                        }
                    case _ =>
                        Unknown()
                }
            case Snd(expr) => 
                expr match {
                    case ident: Ident =>
                        setBounds(ident, st, newArraySize, List.empty, Some(false))
                    case ArrayElem(id, exprs) => 
                        val indexes = exprs.map(expr => expr match {
                            case IntLiter(x) => Some(x)
                            case _ => None
                        })
                        if (!indexes.contains(None)) {
                            setBounds(id, st, newArraySize, indexes.map(optionInt => optionInt.get), Some(false))
                        }
                    case _ =>
                        Unknown()
                }
        }
    }

    /*
        Function to set the arraysize of an ident
    */
    def setBounds(ident: Ident, st: SymbolTable, newArraySize: ArraySize, indexes: List[Int], fstSnd: Option[Boolean]): Unit = {
        updateArrayScope(ident, st, None, true)
        var foundTableOption = st.findAll(ident, 0, List.empty)
        foundTableOption match {
            case None => 
            case Some(foundTable) =>
                fstSnd match {
                    case None => 
                        val oldArraySize = foundTable._2._3.last
                        val generatedBounds = setBoundsFromType(oldArraySize._2, foundTable._2._1, 0, newArraySize, indexes)
                        updateArrayScope(ident, st, Some(generatedBounds), true)
                    case Some(fst) =>
                        if (fst) {
                            foundTable._2._1 match {
                                case PairCheck(type1, _, nested) =>
                                    val oldArraySize = foundTable._2._3.last
                                    val generatedBounds = oldArraySize._2 match {
                                        case NodeArraySize(fstArray, sndArray) => 
                                            NodeArraySize(setBoundsFromType(fstArray, type1, 0, newArraySize, indexes), sndArray)
                                            
                                        case _ => 
                                            NodeArraySize(setBoundsFromType(Unknown(), type1, 0, newArraySize, indexes), Unknown())
                                    }
                                    updateArrayScope(ident, st, Some(generatedBounds), true)
                                case _ =>
                            }
                        } else {
                            foundTable._2._1 match {
                                case PairCheck(_, type2, nested) =>
                                    val oldArraySize = foundTable._2._3.last
                                    val generatedBounds = oldArraySize._2 match {
                                        case NodeArraySize(fstArray, sndArray) => 
                                            NodeArraySize(fstArray, setBoundsFromType(sndArray, type2, 0, newArraySize, indexes))
                                        case _ => 
                                            NodeArraySize(Unknown(), setBoundsFromType(Unknown(), type2, 0, newArraySize, indexes))
                                    }
                                    updateArrayScope(ident, st, Some(generatedBounds), true)
                                case _ =>
                            }
                        }
                }
        }
    }

    /*
        Recursive function to return the new array size from the old arraysize
    */
    def setBoundsFromType(arraySize: ArraySize, typeCheck: TypeCheck, nest: Int, newArraySize: ArraySize, indexes: List[Int]): ArraySize = {
        if (nest != indexes.size) {
            arraySize match {
                case NestedArraySize(size, innerNests) => 
                    val newInnerSize: ListBuffer[ArraySize] = ListBuffer.empty
                    newInnerSize.addAll(innerNests)
                    newInnerSize(indexes(nest)) = setBoundsFromType(innerNests(indexes(nest)), typeCheck, nest + 1, newArraySize, indexes)
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

}
