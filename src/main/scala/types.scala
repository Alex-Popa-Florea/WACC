package wacc

import wacc.ast._

object types {
    sealed trait TypeCheck

    sealed trait BaseTypeCheck extends TypeCheck {
        val nested: Int
    }
    case class IntCheck(nested: Int) extends BaseTypeCheck
    case class BoolCheck(nested: Int) extends BaseTypeCheck
    case class CharCheck(nested: Int) extends BaseTypeCheck
    case class StrCheck(nested: Int) extends BaseTypeCheck
    
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

	def typeCheckToString(typeCheck: TypeCheck): String = {
		var typeString = ""
		typeCheck match {
			case baseType: BaseTypeCheck => 
				baseType match {
					case IntCheck(nested) => 
						typeString = "int"
					case BoolCheck(nested) =>
						typeString = "bool"
					case CharCheck(nested) =>
						typeString = "char"
					case StrCheck(nested) =>
						typeString = "string"
				}
				for(i <- 0 to baseType.nested - 1) {
					typeString += "[]"
				}
			case PairCheck(type1, type2, nested) => 
				typeString = s"pair(${typeCheckToString(type1)}, ${typeCheckToString(type2)})"
				for(i <- 0 to nested - 1) {
					typeString += "[]"
				}
			case EmptyPairCheck() => typeString = "pair"
		}
		typeString
	}
}