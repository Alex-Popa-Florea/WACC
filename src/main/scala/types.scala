package wacc

import wacc.ast._
import classTable._

object types {
	/*
		The trait type check is used to store the type of symbols in the symbol table.
		TypeChecks are used to perform semantic analysis. 
	*/
    sealed trait TypeCheck
	/* 
		Type checks for the base types.
		The parameter nested allows you to represent arrays
		Nested 0 refers to a normal base type, 1 refers to an array
		and 2 refers to an array for arrays, etc.
	*/
    sealed trait BaseTypeCheck extends TypeCheck {
        val nested: Int
    }
    case class IntCheck(nested: Int) extends BaseTypeCheck
    case class BoolCheck(nested: Int) extends BaseTypeCheck
    case class CharCheck(nested: Int) extends BaseTypeCheck
    case class StrCheck(nested: Int) extends BaseTypeCheck

	case class ClassCheck(name: String, nested: Int) extends BaseTypeCheck

	/*
		TypeChecks for a pair and empty pair. 
	*/
    
    case class PairCheck(type1: TypeCheck, type2: TypeCheck, nested: Int) extends TypeCheck
    case class EmptyPairCheck() extends TypeCheck

	/*
		Converts an AST node type to a TypeCheck
		For pairs and arrays, the function is called recursively to find the base type.
	*/
    def extractType(astType: Type): TypeCheck = {
        astType match {
            case IntType() => IntCheck(0)
            case BoolType() => BoolCheck(0)
            case CharType() => CharCheck(0)
            case StrType() => StrCheck(0)
            case ClassType(className) => ClassCheck(className.variable, 0)
            case Pair() => EmptyPairCheck()
            case PairType(elemtype1, elemtype2) => 
                PairCheck(extractType(elemtype1), extractType(elemtype2), 0)
            case ArrayType(arrayType, count) => 
                arrayType match {
                    case IntType() => IntCheck(count)
                    case BoolType() => BoolCheck(count)
                    case CharType() => CharCheck(count)
                    case StrType() => StrCheck(count)
                    case ClassType(className) => ClassCheck(className.variable, count)
                    case PairType(elemtype1, elemtype2) => PairCheck(extractType(elemtype1), extractType(elemtype2), count)
                    case _ => null
                }
        }
	}

	def equalTypes(ct: ClassTable, ot1: Option[TypeCheck], ot2: Option[TypeCheck]): Boolean = {
		ot1 match {
            case None => ot1 == ot2
            case Some(t1) => 
                ot2 match {
                    case None => false
                    case Some(t2) => t1 match {
                        case _ => t1 == t2
                    }
                }
                
        }
	}


	/*
		Function to be used in the future when casting is implemented in code generation
	*/	
	def equalTypesWithCasting(ct: ClassTable, ot1: Option[TypeCheck], ot2: Option[TypeCheck]): Boolean = {
		ot1 match {
            case None => ot1 == ot2
            case Some(t1) => 
                ot2 match {
                    case None => false
                    case Some(t2) => t1 match {
                        case ClassCheck(name, nested) => 
                            if (t1 != t2) {
                                ct.getParent(name) match {
                                    case Some(value) => equalTypes(ct, Some(ClassCheck(value, nested)), ot2)
                                    case None => false
                                }
                            } else {
                                true
                            }
                        case PairCheck(t2type1, t2type2, t2nested) => 
                            t1 match {
                                case PairCheck(t1type1, t1type2, t1nested) =>
                                    equalTypes(ct, Some(t1type1), Some(t2type1)) && equalTypes(ct, Some(t1type2), Some(t2type2)) && t1nested == t2nested
                                case _ => false
                            }
                        
                        case _ => t1 == t2
                    }
                }
                
        }
	}

	/* 
		Function to convert a TypeCheck to a string so that types can be pretty printed
		in semantic errors. 
	*/
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
					case ClassCheck(name, nested) => 
						typeString = s"class ${name}"
				}

				/*
					Adds square brackets for arrays according to value stored in nested
				*/
				for(i <- 0 to baseType.nested - 1) {
					typeString += "[]"
				}
				/*
					Function is called recursively for pairs to find the base types
				*/
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