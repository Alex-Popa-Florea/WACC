package wacc

import wacc.ast._

object types {
    sealed trait TypeCheck

    case class IntCheck(nested: Int) extends TypeCheck
    case class BoolCheck(nested: Int) extends TypeCheck
    case class CharCheck(nested: Int) extends TypeCheck
    case class StrCheck(nested: Int) extends TypeCheck
    case class PairCheck(type1: TypeCheck, type2: TypeCheck, nested: Int) extends TypeCheck
    case class EmptyPairCheck() extends TypeCheck

    def extractType(astType: Type): TypeCheck = {
		astType match {
			case IntType() => IntCheck(0)
			case BoolType() => BoolCheck(0)
			case CharType() => CharCheck(0)
			case StrType() => StrCheck(0)
			case Pair() => EmptyPairCheck()
			case PairType(elemtype1, elemtype2) => 
				PairCheck(extractType(elemtype1), extractType(elemtype2), 0)
			case ArrayType(arrayType, count) => 
				arrayType match {
					case IntType() => IntCheck(count)
					case BoolType() => BoolCheck(count)
					case CharType() => CharCheck(count)
					case StrType() => StrCheck(count)
					case PairType(elemtype1, elemtype2) => PairCheck(extractType(elemtype1), extractType(elemtype2), count)
					case _ => null
				}
		}
	}
}