package wacc

import ast._
import symbolTable._
import types._
import scala.collection.mutable.ListBuffer
import wacc.section._

object arrayBounds {

    sealed trait ArraySize

    case class Unknown() extends ArraySize

    case class LeafArraySize(size: Int) extends ArraySize 
    
    case class NestedArraySize(size: Int, innerNests: List[ArraySize]) extends ArraySize

    case class NodeArraySize(fstArray: ArraySize, sndArray: ArraySize) extends ArraySize

    def updateArrayElemCheckedBounds(arrayElem: ArrayElem, st: SymbolTable): List[Boolean] = {
        val indexes = arrayElem.exprs.map(expr => expr match {
            case IntLiter(x) => Some(x)
            case _ => None
        })
        val checkedBounds = st.checkBounds(arrayElem.id, indexes, None)
        val checks: ListBuffer[Boolean] = ListBuffer.empty
        for (i <- 0 to checkedBounds._1.size - 1) {
            checks.addOne(!checkedBounds._1(i) || checkedBounds._2(i))
        }
        arrayElem.checked = checkedBounds
        checks.toList
    }

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

}
