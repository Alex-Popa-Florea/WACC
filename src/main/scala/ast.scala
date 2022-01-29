object ast {
    trait Node {
        val pos: (Int, Int)
    }

    sealed trait Program extends Node
    case class Begin(stat: Statement, func: Function*)(val pos: (Int, Int)) extends Program

    sealed trait Function extends Program
    case class FunctionDef(t: Type, id: Ident, vars: Option[ParamList], stat: Statement)(val pos: (Int, Int)) extends Function
    
    sealed trait ParamList extends Function
    case class Parameters(params: List[Param])(val pos: (Int, Int)) extends ParamList
    
    sealed trait Param extends ParamList
    case class Parameter(t: Type, id: Ident)(val pos: (Int, Int)) extends Param
    
    sealed trait Statement extends Program
    case class Skip(val pos: (Int, Int)) extends Statement
    case class Assign(t: Type, id: Ident, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class EqStat(lhs: AssignLHS, rhs: AssignRHS)(val pos: (Int, Int)) extends Statement
    case class Read(lhs: AssignLHS)(val pos: (Int, Int)) extends Statement
    case class Free(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Return(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Exit(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Print(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class Println(expr: Expr)(val pos: (Int, Int)) extends Statement
    case class If(cond: Expr, trueStat: Statement, falseStat: Statement)(val pos: (Int, Int)) extends Statement
    case class While(expr: Expr, stat: Statement)(val pos: (Int, Int)) extends Statement
    case class NestedBegin(stat: Statement)(val pos: (Int, Int)) extends Statement
    case class Stat(stat1: Statement, stat2: Statement)(val pos: (Int, Int)) extends Statement

    sealed trait AssignLHS extends Statement

    sealed trait AssignRHS extends Statement 
    case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int))
    case class Call(id: Ident, args: Option[ArgList])(val pos: (Int, Int))

    sealed trait ArgList extends AssignRHS
    case class Args(expr1: Expr, expr2: Expr*)(val pos: (Int, Int)) extends ArgList

    sealed trait PairElem extends AssignLHS
    case class Fst(expr: Expr)(val pos: (Int, Int)) extends PairElem
    case class Snd(expr: Expr)(val pos: (Int, Int)) extends PairElem
    
    sealed trait Type
    
    sealed trait BaseType extends Type
    case object Int extends BaseType
    case object Bool extends BaseType
    case object Char extends BaseType
    case object String extends BaseType

    sealed trait ArrayType extends Type
    case class ArrayInit(t: Type)(val pos: (Int, Int)) extends ArrayType

    sealed trait PairType extends Type
    case class Pair(elemtype1: PairElemType, elemtype2: PairElemType)(val pos: (Int, Int)) extends PairType

    sealed trait PairElemType extends PairType
    case object Pair extends PairElemType

    sealed trait Expr extends AssignRHS
    case class UnaryExpr(op: UnaryOp, expr: Expr)(val pos: (Int, Int)) extends Expr
    case class BinaryExpr(expr1: Expr, op: BinaryOp, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class BracketExpr(expr: Expr)(val pos: (Int, Int)) extends Expr

    sealed trait UnaryOp extends Expr
    case class Not(val pos: (Int, Int)) extends UnaryOp
    case class Neg(val pos: (Int, Int)) extends UnaryOp
    case class Len(val pos: (Int, Int)) extends UnaryOp
    case class Ord(val pos: (Int, Int)) extends UnaryOp
    case class Chr(val pos: (Int, Int)) extends UnaryOp

    sealed trait BinaryOp extends Expr
    case class Mul(val pos: (Int, Int)) extends BinaryOp
    case class Div(val pos: (Int, Int)) extends BinaryOp
    case class Mod(val pos: (Int, Int)) extends BinaryOp
    case class Plus(val pos: (Int, Int)) extends BinaryOp
    case class Minus(val pos: (Int, Int)) extends BinaryOp
    case class GT(val pos: (Int, Int)) extends BinaryOp
    case class GTE(val pos: (Int, Int)) extends BinaryOp
    case class LT(val pos: (Int, Int)) extends BinaryOp
    case class LTE(val pos: (Int, Int)) extends BinaryOp
    case class EQ(val pos: (Int, Int)) extends BinaryOp
    case class NEQ(val pos: (Int, Int)) extends BinaryOp
    case class AND(val pos: (Int, Int)) extends BinaryOp
    case class OR(val pos: (Int, Int)) extends BinaryOp

    sealed trait Identity extends Statement with Function
    case class Ident(string: String)(val pos: (Int, Int)) extends Identity   

    sealed trait ArrayElem extends Expr with AssignLHS
    case class Array(id: Ident, expr: List[Expr])(val pos: (Int, Int)) extends ArrayElem
    
    sealed trait IntLiter extends Expr
    case class Integer(sign: IntSign, digit: Digit)(val pos: (Int, Int)) extends IntLiter

    sealed trait Digit extends IntLiter
    case class Num(num: Int)(val pos: (Int, Int))                          

    sealed trait IntSign extends IntLiter
    case class Posative(val pos: (Int, Int))   extends IntSign
    case class Negative(val pos: (Int, Int))   extends IntSign

    sealed trait BoolLiter extends Expr
    case class True(val pos: (Int, Int))   extends BoolLiter
    case class False(val pos: (Int, Int))   extends BoolLiter

    sealed trait CharLiter extends Expr
    case class CharL(char: Character)(val pos: (Int, Int)) extends Identity

    sealed trait StrLiter extends Expr
    case class Str(string: Character*)(val pos: (Int, Int)) extends Identity
    
    sealed trait Character extends CharLiter with StrLiter
    case class Char(char: Character)(val pos: (Int, Int))                         
    sealed trait EscapeChar
    case class ASCIINull(val pos: (Int, Int))   extends EscapeChar
    case class BackSpace(val pos: (Int, Int))   extends EscapeChar
    case class Tab(val pos: (Int, Int))   extends EscapeChar
    case class NewLine(val pos: (Int, Int))   extends EscapeChar
    case class FormFeed(val pos: (Int, Int))   extends EscapeChar
    case class CarriageReturn(val pos: (Int, Int))   extends EscapeChar
    case class DQuotes(val pos: (Int, Int))   extends EscapeChar
    case class SQuotes(val pos: (Int, Int))   extends EscapeChar
    case class BSlash(val pos: (Int, Int))   extends EscapeChar

    sealed trait ArrayLiter extends Expr
    case class ExprList(expr1: Expr, expr2: Expr*)(val pos: (Int, Int)) extends ArrayLiter

    sealed trait PairLiter extends Expr
    case class Null(val pos: (Int, Int)) extends PairLiter

    sealed trait Comment
    case class Comments(comment: List[CharLiter])(val pos: (Int, Int))
}