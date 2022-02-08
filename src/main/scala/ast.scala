package wacc

import parsley.Parsley, Parsley._
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

object ast {
    trait Node {
        val pos: (Int, Int)
    }

    sealed trait Program extends Node

    case class Begin(func: List[Function], stat: List[Statement])(val pos: (Int, Int)) extends Program 

    case class Function(t: Type, id: Ident, vars: List[Parameter], stat: List[Statement])(val pos: (Int, Int)) extends Node
    case class Parameter(t: Type, id: Ident)(val pos: (Int, Int)) extends Node

    sealed trait Statement extends Node
    case class Skip()(val pos: (Int, Int)) extends Statement
    case class AssignType(t: Type, id: Ident, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class Assign(lhs: AssignLHS, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class Read(lhs: AssignLHS)(val pos: (Int, Int)) extends Statement
    case class Free(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Return(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Exit(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Print(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Println(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class If(cond: Expr, trueStat: List[Statement], falseStat: List[Statement])(val pos: (Int, Int)) extends Statement
    case class While(cond: Expr, stat: List[Statement])(val pos: (Int, Int)) extends Statement
    case class NestedBegin(stat: List[Statement])(val pos: (Int, Int)) extends Statement

    sealed trait AssignLHS extends Node
    case class Ident(variable: String)(val pos: (Int, Int)) extends AssignLHS with Expr
    case class ArrayElem(id: Ident, exprs: List[Expr])(val pos: (Int, Int)) extends AssignLHS with Expr

    sealed trait AssignRHS extends Node 
    case class ArrayLiter(array: List[Expr])(val pos: (Int, Int)) extends AssignRHS
    case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends AssignRHS
    case class Call(id: Ident, args: List[Expr])(val pos: (Int, Int)) extends AssignRHS

    sealed trait PairElem extends AssignLHS with AssignRHS
    case class Fst(expr: Expr)(val pos: (Int, Int)) extends PairElem
    case class Snd(expr: Expr)(val pos: (Int, Int)) extends PairElem
    
    sealed trait Type extends Node
    
    sealed trait BaseType extends Type with PairElemType
    case class IntType()(val pos: (Int, Int)) extends BaseType
    case class BoolType()(val pos: (Int, Int)) extends BaseType
    case class CharType()(val pos: (Int, Int)) extends BaseType
    case class StrType()(val pos: (Int, Int)) extends BaseType
    case class PairType(elemtype1: PairElemType, elemtype2: PairElemType)(val pos: (Int, Int)) extends Type

    sealed trait PairElemType extends Type
    case class Pair()(val pos: (Int, Int)) extends PairElemType

    case class ArrayType(t: Type, count: Int)(val pos: (Int, Int)) extends Type with PairElemType

    sealed trait Expr extends AssignRHS

    sealed trait BinOpBool extends Expr {
        val expr1: Expr
        val expr2: Expr
    }
    sealed trait BinOpInt extends Expr {
        val expr1: Expr
        val expr2: Expr
    }
    sealed trait BinOpEqs extends Expr {
        val expr1: Expr
        val expr2: Expr
    }
    sealed trait BinOpComp extends Expr {
        val expr1: Expr
        val expr2: Expr
    }

    case class IntLiter(x: Int)(val pos: (Int, Int)) extends Expr
    case class BoolLiter(bool: Boolean)(val pos: (Int, Int)) extends Expr
    case class CharLiter(char: Any)(val pos: (Int, Int)) extends Expr
    case class StrLiter(string: String)(val pos: (Int, Int)) extends Expr
    case class PairLiter()(val pos: (Int, Int)) extends Expr
    case class Not(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Neg(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Len(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Ord(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Chr(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpInt
    case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpInt
    case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpInt
    case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpInt
    case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpInt
    case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpComp
    case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpComp
    case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpComp
    case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpComp
    case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpEqs
    case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpEqs
    case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpBool
    case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr with BinOpBool

    case class Negative()(val pos: (Int, Int)) extends Node

    // Builders:

    // Generic builders:

    trait ParserBuilder[T] {
        val parser: Parsley[T]
        final def <#(p: Parsley[_]): Parsley[T] = parser <~ p
    }
    trait ParserBuilderPos0[R] extends ParserBuilder[R] {
        def apply()(pos: (Int, Int)): R
        val parser = pos.map(p => apply()(p))
    }
    trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
        def apply(x: T1)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_)(p))
    }
    trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
        def apply(x: T1, y: T2)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_, _)(p))
    }    
    trait ParserBuilderPos3[T1, T2, T3, R] extends ParserBuilder[(T1, T2, T3) => R] {
        def apply(x: T1, y: T2, z: T3)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_, _, _)(p))
    }
    trait ParserBuilderPos4[T1, T2, T3, T4, R] extends ParserBuilder[(T1, T2, T3, T4) => R] {
        def apply(x: T1, y: T2, z: T3, d: T4)(pos: (Int, Int)): R
        val parser = pos.map(p => apply(_, _, _,_)(p))
    }

    // Specific Builders:

    // Program builder:
        
    object Begin {
        def apply(func: => Parsley[List[Function]], stat: => Parsley[List[Statement]]): Parsley[Begin] = 
            pos <**> (func, stat).zipped(Begin(_, _) _)
    }

    // Function builders:
            
    object Function {
        def apply(t: => Parsley[Type], id: => Parsley[Ident], vars: => Parsley[List[Parameter]], st: Parsley[List[Statement]]): Parsley[Function] =
            pos <**> (t, id, vars, st).zipped(Function(_, _, _, _) _)
    }

    object Parameter {
        def apply(t: => Parsley[Type], id: =>Parsley[Ident]): Parsley[Parameter] =
            pos <**> (t, id).zipped(Parameter(_,_) _)
    }

    // Call:

    object Call {
        def apply(id: => Parsley[Ident], args: => Parsley[List[Expr]]): Parsley[Call] =
            pos <**> (id, args).zipped(Call(_, _) _)
    }


    // Statements:
    
    object Skip extends ParserBuilderPos0[Skip]

    object AssignType {
        def apply(t: => Parsley[Type], id: => Parsley[Ident], rhs: Parsley[AssignRHS]): Parsley[AssignType] =
            pos <**> (t, id, rhs).zipped(AssignType(_, _, _) _)
    }

    object Assign {
        def apply(lhs: => Parsley[AssignLHS], rhs: Parsley[AssignRHS]): Parsley[Assign] =
            pos <**> (lhs, rhs).zipped(Assign(_, _) _)
    }

    object Read {
         def apply(lhs: => Parsley[AssignLHS]): Parsley[Read] = 
             pos <**> lhs.map(Read(_) _)
    }

    object Free {
         def apply(expr: => Parsley[Expr]): Parsley[Free] = 
             pos <**> expr.map(Free(_) _)
    }

    object Return {
         def apply(expr: => Parsley[Expr]): Parsley[Return] = 
             pos <**> expr.map(Return(_) _)
    }

    object Exit {
         def apply(expr: => Parsley[Expr]): Parsley[Exit] = 
             pos <**> expr.map(Exit(_) _)
    }

    object Print {
         def apply(expr: => Parsley[Expr]): Parsley[Print] = 
             pos <**> expr.map(Print(_) _)
    }

    object Println {
         def apply(expr: => Parsley[Expr]): Parsley[Println] = 
             pos <**> expr.map(Println(_) _)
    }

    object If {
        def apply(cond: => Parsley[Expr], trueStat: => Parsley[List[Statement]], falseStat: => Parsley[List[Statement]]): Parsley[If] =
            pos <**> (cond, trueStat, falseStat).zipped(If(_, _, _) _)
    }

    object While {
        def apply(cond: Parsley[Expr], stat: Parsley[List[Statement]]): Parsley[While] =
            pos <**> (cond, stat).zipped(While(_, _) _)
    }

    object NestedBegin {
         def apply(stat: => Parsley[List[Statement]]): Parsley[NestedBegin] = 
             pos <**> stat.map(NestedBegin(_) _)
    }

    
    // Types:

    object IntType extends ParserBuilderPos0[IntType]
    object StrType extends ParserBuilderPos0[StrType]
    object BoolType extends ParserBuilderPos0[BoolType]
    object CharType extends ParserBuilderPos0[CharType]

    object ArrayType {
        def apply(t: =>Parsley[Type], count: Parsley[Int]): Parsley[ArrayType] = 
            pos <**> (t, count).zipped(ArrayType(_, _) _)
    }

    object PairType {
        def apply(elemtype1: => Parsley[PairElemType], elemtype2: Parsley[PairElemType]): Parsley[PairType] =
            pos <**> (elemtype1, elemtype2).zipped(PairType(_, _) _)
    }

    // Expressions:

    object Not extends ParserBuilderPos1[Expr,Not]
    object Neg extends ParserBuilderPos1[Expr,Neg]
    object Len extends ParserBuilderPos1[Expr,Len]
    object Ord extends ParserBuilderPos1[Expr,Ord]
    object Chr extends ParserBuilderPos1[Expr,Chr]
    object Mul extends ParserBuilderPos2[Expr,Expr,Mul]
    object Div extends ParserBuilderPos2[Expr,Expr,Div]
    object Mod extends ParserBuilderPos2[Expr,Expr,Mod]
    object Add extends ParserBuilderPos2[Expr,Expr,Add]
    object Sub extends ParserBuilderPos2[Expr,Expr,Sub]
    object GT extends ParserBuilderPos2[Expr,Expr,GT]
    object GTE extends ParserBuilderPos2[Expr,Expr,GTE]
    object LT extends ParserBuilderPos2[Expr,Expr,LT]
    object LTE extends ParserBuilderPos2[Expr,Expr,LTE]
    object EQ extends ParserBuilderPos2[Expr,Expr,EQ]
    object NEQ extends ParserBuilderPos2[Expr,Expr,NEQ]
    object And extends ParserBuilderPos2[Expr,Expr,And]
    object Or extends ParserBuilderPos2[Expr,Expr,Or]

    // ArrayElem:

    object ArrayElem {
        def apply(id: => Parsley[Ident], exprs: Parsley[List[Expr]]): Parsley[ArrayElem] =
            pos <**> (id, exprs).zipped(ArrayElem(_, _) _)
    }

    // Pair:

    object Pair extends ParserBuilderPos0[Pair]

    object Fst {
        def apply(expr: =>Parsley[Expr]): Parsley[Fst] = 
            pos <**> expr.map(Fst(_) _)
    }
    object Snd {
        def apply(expr: =>Parsley[Expr]): Parsley[Snd] = 
            pos <**> expr.map(Snd(_) _)
    }

    object NewPair {
        def apply(expr1: => Parsley[Expr], expr2: Parsley[Expr]): Parsley[NewPair] =
            pos <**> (expr1, expr2).zipped(NewPair(_, _) _)
    }


    // Identifier:
    
    object Ident {
        def apply(variable: =>Parsley[String]): Parsley[Ident] = 
            pos <**> variable.map(Ident(_) _)
    }

    // Liters:
    
    object IntLiter {
         def apply(x: => Parsley[Int]): Parsley[IntLiter] = 
             pos <**> x.map(IntLiter(_) _)
    }

    object BoolLiter {
         def apply(bool: => Parsley[Boolean]): Parsley[BoolLiter] = 
             pos <**> bool.map(BoolLiter(_) _)
    }

    object CharLiter {
        def apply(char: => Parsley[Any]): Parsley[CharLiter] = 
            pos <**> char.map(CharLiter(_) _)
    }

    object StrLiter {
         def apply(string: => Parsley[String]): Parsley[StrLiter] = 
             pos <**> string.map(StrLiter(_) _)
    }

    object ArrayLiter {
        def apply(array: => Parsley[List[Expr]]): Parsley[ArrayLiter] = 
            pos <**> array.map(ArrayLiter(_) _)
    }

    object PairLiter extends ParserBuilderPos0[PairLiter]

}