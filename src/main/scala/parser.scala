package wacc

import parsley.Parsley, Parsley._
import parsley.debug._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import ast._
    import parsley.expr.{precedence, Ops, InfixL, InfixR, NonAssoc, Prefix, SOps}
    import parsley.combinator._
    import parsley.errors.combinator.ErrorMethods
    
    def makeGreen(msg:String):String = {
        Console.GREEN+msg+Console.RESET
    }

    val explainStatement = s""" Missing Statement(s):
    Statements are simply instructions.
    To do nothing: ${makeGreen("skip")}
    To assign a variable: ${makeGreen("<type> <name> = <expression>")}
    To Reassign variables: ${makeGreen("<variable> = <expression>")}
    To take in input and store in a variable from user: ${makeGreen("read <variable>")}
    To free pairs or arrays (not nested): ${makeGreen("free <variable>")}
    To give something from a function: ${makeGreen("return <expressions>")}
    To exit the program with an error number: ${makeGreen("exit <number>")}
    To print with out a new line: ${makeGreen("print <expression>")}
    To print with a new line: ${makeGreen("print <expression>")}
    To use an if statement: ${makeGreen("if <condition of type bool> <statement(s)> else <statement(s)> fi")}
    To repeat until a condition is met: ${makeGreen("While <condition> do <statement(s)> done")}
    To have nested begin statemenets: ${makeGreen("begin <statement(s)> end")}
    To write multiple statements ${makeGreen("<statement> ; <statment")}
    Variables are Expressions!
    """

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
    lazy val statement: Parsley[Statement] = 
        ((Skip <# "skip") <|> 
        AssignType(types, ident, "=" ~> assignRHS) <|> 
        Assign(assignLHS, "=" ~> assignRHS) <|>
        Read("read" ~> assignLHS) <|>
        Free("free" ~> expr) <|>
        Return("return" ~> expr) <|>
        Exit("exit" ~> expr) <|>
        Print("print" ~> expr) <|>
        Println("println" ~> expr) <|>
        If("if" ~> expr, "then" ~> nestedStatement, "else" ~> nestedStatement <~ "fi") <|>
        While("while" ~> expr, "do" ~> nestedStatement <~ "done") <|>
        NestedBegin("begin" ~> nestedStatement <~ "end")
        ).hide.explain(explainStatement)
        
    private lazy val program = "begin" ~> (Begin(functions, nestedStatement)) <~ "end"
    private lazy val atom =  
        "(" ~> expr <~ ")" <|> attempt(arrayElem) <|> ident <|> charLiter <|> intLiter <|> boolLiter <|> stringLiter <|> pairLiter 

    lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
        SOps(Prefix)(Not  <# "!"),
        SOps(Prefix)(Neg  <# "-"),
        SOps(Prefix)(Len  <# "len"),
        SOps(Prefix)(Ord  <# "ord"),
        SOps(Prefix)(Chr  <# "chr"),
        SOps(InfixL)(Mul  <# "*",  Div <# "/"),
        SOps(InfixL)(Mod  <# "%"),
        SOps(InfixL)(Add  <# "+",  Sub <# "-"),
        SOps(NonAssoc)(GT <# ">",  GTE <# ">=",
                      LT <# "<",  LTE <# "<=",
                      EQ <# "==", NEQ <# "!="),
        SOps(InfixR)(And  <# "&&"),
        SOps(InfixR)(Or   <# "||")
        
    )
    val result = fully(program)
}