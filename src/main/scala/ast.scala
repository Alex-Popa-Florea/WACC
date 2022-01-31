import parsley.Parsley, Parsley._
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

object ast {
    trait Node {
        val pos: (Int, Int)
    }

    sealed trait Program extends Node

    case class Begin(func: List[Function], stat: List[Statement])(val pos: (Int, Int)) extends Program // change all * to List[]

    case class Function(t: Type, id: Ident, vars: ParamList, stat: List[Statement])(val pos: (Int, Int)) extends Node
    case class ParamList(ps: List[Parameter])(val pos: (Int, Int)) extends Node
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
    case class While(expr: Expr, stat: List[Statement])(val pos: (Int, Int)) extends Statement
    case class NestedBegin(stat: List[Statement])(val pos: (Int, Int)) extends Statement

    sealed trait AssignLHS extends Node
    case class Ident(variable: String)(val pos: (Int, Int)) extends AssignLHS with Expr
    case class ArrayElem(id: Ident, head:Expr, rest:List[Expr])(val pos: (Int, Int)) extends AssignLHS with Expr

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
    case class PairType()(elemtype1: PairElemType, elemtype2: PairElemType)(val pos: (Int, Int)) extends Type

    sealed trait PairElemType extends Type
    case class Pair(val pos: (Int, Int)) extends PairElemType

    case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type with PairElemType

    sealed trait Expr extends AssignRHS
    case class IntLiter(x: Int)(val pos: (Int, Int)) extends Expr // change back wiht sign 
    case class BoolLiter(bool: Boolean)(val pos: (Int, Int)) extends Expr
    case class CharLiter(char: Any)(val pos: (Int, Int)) extends Expr
    case class StrLiter(string: String)(val pos: (Int, Int)) extends Expr
    case class PairLiter()(val pos: (Int, Int)) extends Expr
    case class Not(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Neg(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Len(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Ord(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Chr(expr1: Expr)(val pos: (Int, Int)) extends Expr
    case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class GT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class GTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class LT(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class LTE(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class EQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class NEQ(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr

    case class Negative()(val pos: (Int, Int)) extends Node

}