package wacc

import parsley.Parsley, Parsley._
import ast._
import parser._
import wacc.types._
import wacc.symbolTable._

object semanticAnalyser {

	def analyse(node: Node, st: SymbolTable): Boolean = {
		node match {
			case Begin(func, stat) => true
            
			case Function(t, id, vars, stats) => true
				
			case Parameter(t, id) => true
			
			case Skip() => true

			case AssignType(t, id, rhs) => true

			case Assign(lhs, rhs) => true

			case Read(lhs) => true

			case Free(expr) => true

			case Return(expr) => true
				
			case Exit(expr) => true

			case Print(expr) => true

			case Println(expr) => true

			case If(cond, trueStat, falseStat) => true

			case While(cond, stat) => true

			case NestedBegin(stat) => true
			
			case expr: Expr => true
		}
	}


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