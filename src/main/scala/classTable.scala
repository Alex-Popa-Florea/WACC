package wacc
import wacc.types._
import scala.collection.mutable.Map
import backend.codeGenerator.getBytesFromType
object classTable {
     class ClassTable() {
        private var classMap: Map[String, (Option[String], List[(String, TypeCheck)], Map[String, TypeCheck],  Map[String, TypeCheck], Map[String, (TypeCheck, List[TypeCheck])],  Map[String, (TypeCheck, List[TypeCheck])])] = Map.empty
        /*
            The add method adds a class to the class table, returning true
            if the addition succeded, as in there was no class of that
            name previously added, false otherwise.
        */
        def add(className: String, parent: Option[String], constructors: List[(String, TypeCheck)], publicFields: Map[String, TypeCheck], privateFields: Map[String, TypeCheck], publicMethods: Map[String, (TypeCheck, List[TypeCheck])], privateMethods:  Map[String, (TypeCheck, List[TypeCheck])]): Boolean = {
            classMap.get(className) match { 
                case None => 
                    classMap.addOne(className -> ((parent, constructors, publicFields.addAll(constructors), privateFields, publicMethods, privateMethods)))
                    true
                case _ => false
            }
        }

        /*
            Method to get the element of that class name from the classTable
        */
        def getClass(className: String): Option[(Option[String], List[(String, TypeCheck)], Map[String, TypeCheck], Map[String, TypeCheck], Map[String, (TypeCheck, List[TypeCheck])],  Map[String, (TypeCheck, List[TypeCheck])])] = {
            classMap.get(className)
        }

        /*
            Method to return the class map of the class table
        */
        def getClassMap():  Map[String, (Option[String], List[(String, TypeCheck)], Map[String, TypeCheck], Map[String, TypeCheck], Map[String, (TypeCheck, List[TypeCheck])],  Map[String, (TypeCheck, List[TypeCheck])])] = {
            classMap
        }

        /*
            Method to return the constructors of a class from the class table
        */
        def getConstructors(className: String): Option[List[(String, TypeCheck)]] = {
            classMap.get(className) match {
                case Some(value) => Some(value._2)
                case None => None
            }
        }
        
        /*
            Method that checks the number of constructors passed is the same as
            the expected number of constructors for a given class
        */
        def checkLengthConstructor(className: String, argTypes: List[TypeCheck]): Boolean = {
            classMap.get(className) match {
                case None => false
                case Some(foundArgs) => (foundArgs._2.length == argTypes.length)
            }
        }

        /*
            Method to return the public fields of a class from the class table
        */
        def getPublicFields(className: String): Option[Map[String, TypeCheck]] = {
            classMap.get(className) match {
                case None => None
                case Some(found) => Some(found._3)
            }
        }

        /*
            Method to return the private fields of a class from the class table
        */
        def getPrivateFields(className: String): Option[Map[String, TypeCheck]] = {
            classMap.get(className) match {
                case None => None
                case Some(found) => Some(found._4)
            }
        }

        /*
            Method to return the public methods of a class from the class table
        */
        def getPublicMethods(className: String): Option[Map[String, (TypeCheck, List[TypeCheck])]] = {
            classMap.get(className) match {
                case None => None
                case Some(found) => Some(found._5)
            }
        }

        /*
            Method to return the private methods of a class from the class table
        */
        def getPrivateMethods(className: String): Option[Map[String, (TypeCheck, List[TypeCheck])]] = {
            classMap.get(className) match {
                case None => None
                case Some(found) => Some(found._6)
            }
        }

        /*
            Method to return the parent of a class from the class table
        */
        def getParent(className: String): Option[String] = {
            classMap.get(className) match {
                case None => None
                case Some(foundClass) => foundClass._1
            }
        }

        /*
            Method that checks the constructors passed correspond to the types
            of the constructors stored for the given function in the function table
        */
        def checkConstructor(className: String, argTypes: List[TypeCheck]): Boolean = {
            classMap.get(className) match {
                case None => false
                case Some(foundArgs) =>
                    var equality = true
                    if (foundArgs._2.length != argTypes.length) {
                        return false
                    }
                    for (i <- 0 to (foundArgs._2.length - 1)) {
                        foundArgs._2.lift(i) match {
                            case None => false
                            case Some(arg) => 
                                arg._2 match {
                                    case PairCheck(_, _, _) => equality &= (argTypes.lift(i) == Some(arg._2) || (argTypes.lift(i) == Some(EmptyPairCheck())))
                                    case _ => equality &= (argTypes.lift(i) == Some(arg._2))
                            }
                        }
                    }
                    equality
            }
        }

        /*
            Method to return the field map from a class, recursing into the anscestor tables
        */
        def getFieldMap(className: String, index: Int, fieldMap: Map[String, (TypeCheck, Int)]): Map[String, (TypeCheck, Int)] = {
            var localIndex = index
            getConstructors(className) match {
                case Some(fields) => fields.foreach( field =>
                    fieldMap.get(field._1) match {
                        case Some(value) => 
                        case None => fieldMap.addOne((field._1, (field._2, localIndex)))
                                    localIndex += 1
                    }
                    )
                case None => 
            }
            getPrivateFields(className) match {
                case Some(fields) => fields.foreach( field =>
                    fieldMap.get(field._1) match {
                        case Some(value) => 
                        case None => fieldMap.addOne((field._1, (field._2, localIndex)))
                                    localIndex += 1
                    }
                    )
                case None => 
            }

            getPublicFields(className) match {
                case Some(fields) => fields.foreach( field =>
                    fieldMap.get(field._1) match {
                        case Some(value) => 
                        case None => fieldMap.addOne((field._1, (field._2, localIndex)))
                                    localIndex += 1
                    }
                    )
                case None =>
            }

            getParent(className) match {
                case Some(foundParent) => getFieldMap(foundParent, localIndex, fieldMap)
                case None =>
            }
            fieldMap
        }

        /*
            Method to return the size of the class table
        */
        def getClassSize(className: String): Int = {
            getFieldMap(className, 0, Map.empty).size * 4
        }

        /*
            Method that prints the class table
        */
        def printClassTables(): Unit = {
            println(s"  - Classes: ")
            println("")
            classMap.zip(0 until classMap.size).foreach({ case ((k, v), i) => 
                println(s"    ${i + 1}. \"$k\":")
                println(s"\t - ${v}")
                v._2.foreach(x => 
                    println(s"\t - ${x}")
                )
                println("")
            })
        }
    }
}
