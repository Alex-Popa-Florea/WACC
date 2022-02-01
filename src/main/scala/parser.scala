package wacc

import parsley.Parsley, Parsley._
import parsley.debug._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import ast._
    import parsley.expr.{precedence, Ops, InfixL, InfixR, NonAssoc, Prefix}
    import parsley.combinator._
    
    private def count(p: =>Parsley[_]): Parsley[Int] = p.foldLeft(0)((n, _) => n + 1)

    private lazy val ident = Ident(VARIABLE)

    private lazy val charLiter = CharLiter(CHAR)
    private lazy val intLiter = (attempt(option("+")) ~> IntLiter(INTEGER))
    private lazy val boolLiter = BoolLiter(("true" ~> pure(true)) <|> ("false" ~> pure(false)))
    private lazy val stringLiter = StrLiter(STRING)
    private lazy val arrayLiter = "[" ~> ArrayLiter(sepBy(expr, ",")) <~ "]"
    private lazy val pairLiter = (PairLiter <# "null")

    private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))

    private lazy val pairElem: Parsley[PairElem] = ("fst" ~> Fst(expr)) <|> ("snd" ~> Snd(expr))
    private lazy val newPair = "newpair" ~> NewPair("(" ~> expr, "," ~> expr <~ ")")

    private lazy val types: Parsley[Type] = attempt((baseType <|> pairType) <~ notFollowedBy("[")) <|> arrayType

    private lazy val baseType: Parsley[BaseType] = ((IntType <# "int") <|> (StrType <# "string") <|> (BoolType <# "bool") <|> (CharType <# "char"))
    private lazy val arrayType = ArrayType((baseType <|> pairType), count("[" <~ "]"))
    private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
    private lazy val pairElemType: Parsley[PairElemType] = attempt(baseType <~ notFollowedBy("[")) <|> arrayType <|> (Pair <# "pair")
    
    
    private lazy val arglist = sepBy(expr, ",")
    
    private lazy val param = Parameter(types, " " ~> Ident(VARIABLE))
    private lazy val params = sepBy(param, ",")
    private lazy val function = attempt(Function(types, Ident(VARIABLE), "(" ~> params <~ ")", "is" ~> nestedStatement))
    private lazy val functions = endBy(function, "end")
    
    private lazy val call = Call("call" ~> ident, "(" ~> arglist <~ ")")

    private lazy val assignLHS: Parsley[AssignLHS] = attempt(arrayElem) <|> ident <|> pairElem
    private lazy val assignRHS = expr <|> arrayLiter <|> newPair <|> pairElem <|> call
    
    private lazy val nestedStatement = sepBy1(statement, ";")
    private lazy val statement: Parsley[Statement] = 
        (Skip <# "skip") <|> 
        AssignType(types, ident, "=" ~> assignRHS) <|> 
        Assign(assignLHS, "=" ~> assignRHS)
        
        
    private lazy val program = "begin" ~> (Begin(functions, nestedStatement)) <~ "end"
    private lazy val atom =  
        "(" ~> expr <~ ")" <|> attempt(arrayElem) <|> ident <|> charLiter <|> intLiter <|> boolLiter <|> stringLiter <|> pairLiter 

    lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
        Ops(Prefix)(Not  <# "!"),
        Ops(Prefix)(Neg  <# "-"),
        Ops(Prefix)(Len  <# "len"),
        Ops(Prefix)(Ord  <# "ord"),
        Ops(Prefix)(Chr  <# "chr"),
        Ops(InfixL)(Mul  <# "*",  Div <# "/"),
        Ops(InfixL)(Mod  <# "%"),
        Ops(InfixL)(Add  <# "+",  Sub <# "-"),
        Ops(NonAssoc)(GT <# ">",  GTE <# ">=",
                      LT <# "<",  LTE <# "<=",
                      EQ <# "==", NEQ <# "!="),
        Ops(InfixR)(And  <# "&&"),
        Ops(InfixR)(Or   <# "||")
        
    )
    val result = fully(program)
}