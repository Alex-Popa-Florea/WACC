package wacc

import parsley.Parsley, Parsley._
import ast._
import types._

object symbolTable {
    class SymbolTable(parent: Option[SymbolTable]) {
        var variableMap: Map[(String, Boolean), TypeCheck] = Map()
        var children: List[SymbolTable] = List()

        def add(variable: (String, Boolean), varType: TypeCheck): Boolean = {
            variableMap.get(variable) match {
                case None => 
                    variableMap += (variable -> varType)
                    true
                case _ => false
            }
        }

        def find(variable: (String, Boolean)): Option[TypeCheck] = {
            var foundType = variableMap.get(variable)
            foundType match {
                case None => parent match {
                    case None => None
                    case _ => parent.get.find(variable)
                }
                case _ => foundType
            }
        }
    }
}