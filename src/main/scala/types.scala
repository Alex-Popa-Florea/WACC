package wacc

object types {
    sealed trait TypeCheck

    case class IntCheck(nested: Int) extends TypeCheck
    case class BoolCheck(nested: Int) extends TypeCheck
    case class CharCheck(nested: Int) extends TypeCheck
    case class StrCheck(nested: Int) extends TypeCheck
    case class PairCheck(type1: TypeCheck, type2: TypeCheck, nested: Int) extends TypeCheck
    case class EmptyPairCheck() extends TypeCheck
}