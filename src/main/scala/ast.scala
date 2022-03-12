package wacc

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods
import parsley.errors.combinator._
import parsley.implicits.zipped.Zipped2
import parsley.implicits.zipped.Zipped3
import parsley.implicits.zipped.Zipped4
import parsley.lift.lift1
import wacc.symbolTable._

import Parsley._

object ast {

    /*
       All AST nodes have a position, a integer tuple that keeps track of the position
       in the file being parsed, to allow accurate error messages 
    */
    trait Node {
        val pos: (Int, Int)
    }

    /* 
       There's an AST node for every token in the program (e.g. operators, variables, keywords, literals)
    
       AST nodes are defined in a hierarchy from the whole program to individual
       types and literals. The nodes at the top are defined in terms of the nodes
       lower in the hierarchy of the AST. 
       
       The nodes store the relevent information needed to perform semantic analysis
       and build a symbol table.

       The sealed traits group the categories of nodes and allow for easier pattern
       matching in the semantic analyser.
    */

    
    sealed trait Program extends Node

    case class Begin(func: List[Function], stat: List[Statement])(val pos: (Int, Int)) extends Program 

    case class Function(t: Type, id: Ident, vars: List[Parameter], stat: List[Statement])(val pos: (Int, Int)) extends Node{
        var semanticTable: Option[SymbolTable] = None
    }
    case class Parameter(t: Type, id: Ident)(val pos: (Int, Int)) extends Node

    sealed trait Statement extends Node
    case class Skip()(val pos: (Int, Int)) extends Statement
    case class AssignType(t: Type, id: Ident, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class Assign(lhs: AssignLHS, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class Read(lhs: AssignLHS)(val pos: (Int, Int)) extends Statement
    case class Free(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Return(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Exit(expr: Expr)(val pos: (Int, Int)) extends Statement

    sealed trait PrintTrait extends Statement {
        val expr: Expr
    }
    case class Print(expr: Expr)(val pos: (Int, Int)) extends Statement with PrintTrait
    case class Println(expr: Expr)(val pos: (Int, Int)) extends Statement with PrintTrait
    case class If(cond: Expr, trueStat: List[Statement], falseStat: List[Statement])(val pos: (Int, Int)) extends Statement{
        var trueSemanticTable: Option[SymbolTable] = None
        var falseSemanticTable: Option[SymbolTable] = None
    }
    case class While(cond: Expr, stat: List[Statement])(val pos: (Int, Int)) extends Statement{
        var semanticTable: Option[SymbolTable] = None
    }
    case class NestedBegin(stat: List[Statement])(val pos: (Int, Int)) extends Statement{
        var semanticTable: Option[SymbolTable] = None
    }

    sealed trait AssignLHS extends Node
    case class Ident(variable: String)(val pos: (Int, Int)) extends AssignLHS with Expr {
        var symbolTable: Option[SymbolTable] = None
    }
    case class ArrayElem(id: Ident, exprs: List[Expr])(val pos: (Int, Int)) extends AssignLHS with Expr {
        var checked: Boolean = false
    }

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
    case class CharLiter(char: Char)(val pos: (Int, Int)) extends Expr
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


    /* 
       Generic parser builder traits
       Used to define builders for each type of AST node
       There are different builders for AST nodes with 0 to 4 arguments 
    */

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

    /* 
       Specific Builders for each AST nodes
       These are called in the parser
       They each have an apply method which takes the information from the parser
       and uses it to construct and AST node 
    */

    /*
        Program node builder 
    */
        
    object Begin {
        def apply(func: => Parsley[List[Function]], stat: => Parsley[List[Statement]]): Parsley[Begin] = 
            pos <**> (func, stat).zipped(Begin(_, _) _)
    }

    /* 
        Function node builders 
    */
            
    object Function {
        def apply(pair: => Parsley[(Type,Ident)], vars: => Parsley[List[Parameter]], st: Parsley[List[Statement]]): Parsley[Function] = {
            pos <**> (pair, vars, st).zipped((x, y, z) => Function(x._1, x._2, y, z) _)
        }
    }

    object Parameter {
        def apply(t: => Parsley[Type], id: =>Parsley[Ident]): Parsley[Parameter] =
            pos <**> (t, id).zipped(Parameter(_,_) _)
    }

    /*
        Call node builder 
    */

    object Call {
        def apply(id: => Parsley[Ident], args: => Parsley[List[Expr]]): Parsley[Call] =
            pos <**> (id, args).zipped(Call(_, _) _)
    }


    /* 
        Statement node builders 
    */
    
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

    
    /* 
        Type node builders 
    */

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

    /* 
        Operator node builders 
    */

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

    /* 
        ArrayElem node builder 
    */

    object ArrayElem {
        def apply(id: => Parsley[Ident], exprs: Parsley[List[Expr]]): Parsley[ArrayElem] =
            pos <**> (id, exprs).zipped(ArrayElem(_, _) _)
    }

    /* 
        Pair node builders 
    */

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


    /* 
        Identifier node builder 
    */
    
    object Ident {
        def apply(variable: =>Parsley[String]): Parsley[Ident] = 
            pos <**> variable.map(Ident(_) _)
    }

    /* 
        Literal node builders 
    */
    
    /*
        We check for integer overflow in the IntLiter node builder
        If the big integer is in the integer range (--2147483648 to 2147483647), its 
        converted to an integer, and the IntLiter node is constructed 
        In case of an overflow, the fail parser is used to produce an error and message 
    */
    object IntLiter {
         def apply(x: => Parsley[BigInt]): Parsley[IntLiter] = {
            val checker: PartialFunction[BigInt,String] = {
                case i if i > Int.MaxValue || i < Int.MinValue => s"Int overflow Int must be in the range of ${Int.MinValue} and ${Int.MaxValue} currently $i"
            }
            val c = x.guardAgainst(checker)
            pos <**> c.map(i => IntLiter(i.toInt) _)
         }
    }

    object BoolLiter {
         def apply(bool: => Parsley[Boolean]): Parsley[BoolLiter] = 
             pos <**> bool.map(BoolLiter(_) _)
    }

    object CharLiter {
        def apply(char: => Parsley[Char]): Parsley[CharLiter] = 
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