package wacc

import ast._
import scala.collection.mutable.ListBuffer

object arrayBounds {

    sealed trait ArraySize

    case class Unknown() extends ArraySize

    case class LeafArraySize(size: Int) extends ArraySize 
    
    case class NestedArraySize(size: Int, innerNests: ListBuffer[ArraySize]) extends ArraySize

    case class NodeArraySize(fstArray: ArraySize, sndArray: ArraySize) extends ArraySize
}
