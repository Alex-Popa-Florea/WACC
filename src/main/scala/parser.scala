package wacc

import parsley.Parsley, Parsley._
import parsley.debug._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import ast._
    import parsley.expr.{precedence, InfixL, InfixR, NonAssoc, Prefix, Ops}
    import parsley.combinator._
    import parsley.errors.combinator.ErrorMethods
    
    def makeGreen(msg:String):String = {
        Console.GREEN+msg+Console.RESET
    }

    val explainStatement = 
s"""Missing Statement(s):
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

    val explainExpr =s"""Expressions:
    Expressions are a combinations of values and operators.
    Some operators take 2 arguments and others just 1 describe below:
    Not, Negate a bool  ${makeGreen("!<bool>")}
    Negation, negate a number ${makeGreen("-<int>")}
    Length, get the length of an array ${makeGreen("len <array>")}
    Ordinal, gives the ASCII value of the character ${makeGreen("ord <variable>")}
    Character, gives the ASCII charcter from the given number ${makeGreen("chr <int>")}
    Multiply, gives the  ${makeGreen("<int> * <int>")}
    Divide, gives the integer divison of 2 ints  ${makeGreen("<int> / <int>")}
    Mod, gives the remainder of dividing 2 ints ${makeGreen("<int> % <int>")}
    Add, gives the addition of 2 ints ${makeGreen("<int> + <int>")}
    Sub, gives the subtraction of 2 ints ${makeGreen("<int> - <int>")}
    Greater Than, gives true if LHS greater than RHS ${makeGreen("<int> > <int>")}
    Greater Than Or Equal To, gives true if LHS greater than or equal to RHS ${makeGreen("<int> >= <int>")}
    Less Than, gives true if LHS less than RHS ${makeGreen("<int> < <int>")}
    Less Than Or Equal To, gives true if LHS less than or equal toRHS ${makeGreen("<int> <= <int>")}
    Equals, gives true if LHS is equal RHS ${makeGreen("<variable> == <variable>")}
    Not Equals, gives true if LHS is not equal RHS ${makeGreen("<variable> != <variable>")}
    Logical And,gives true if both bools are true ${makeGreen("<bool> && <bool>")}
    Logical Or,gives true if one of the bools are true ${makeGreen("<bool> || <bool>")}
    """

    val explainLHS = 
s"""Left hand side of an assignment, left of the equals:
    Can be an array element: ${makeGreen("<array>[<position>] =")}
    Can be a variable: ${makeGreen("<variable> = ")}
    Can be a pair element: ${makeGreen("fst <pair> =")} 
                           ${makeGreen("snd <pair> =")}
    """
    val explainPairElem = 
s"""Acessing pairs using fst and snd are used to access pairs:
    to get the first element ${makeGreen("fst <pair>")}
    to get the second element ${makeGreen("snd <pair>")}
    """

    private def count(p: =>Parsley[_]): Parsley[Int] = p.foldLeft(0)((n, _) => n + 1)

    private lazy val ident = Ident(VARIABLE)

    private lazy val charLiter = CharLiter(CHAR)
    private lazy val intLiter = (attempt(option("+")) ~> IntLiter(INTEGER))
    private lazy val boolLiter = BoolLiter(("true" ~> pure(true)) <|> ("false" ~> pure(false)))
    private lazy val stringLiter = StrLiter(STRING)
    private lazy val arrayLiter = ("[" ~> ArrayLiter(sepBy(expr, ",")) <~ "]").hide.explain(s"Array can be constructed by having\n  elements, if any, seperated by commas surrounded in square brackets ${makeGreen("[<expression>,<expression>...]")}\n")

    private lazy val pairLiter = (PairLiter <# "null")

    private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))

    private lazy val pairElem: Parsley[PairElem] = ("fst" ~> Fst(expr)) <|> ("snd" ~> Snd(expr)).hide
    private lazy val newPair = "newpair" ~> NewPair("(" ~> expr, "," ~> expr <~ ")").hide.explain(s"Pairs are contrusted using newpair: ${makeGreen("newpair (<expression>, <expression>)")}\n")

    lazy val types: Parsley[Type] = (attempt((baseType <|> pairType) <~ notFollowedBy("[")) <|> arrayType).hide

    private lazy val baseType: Parsley[BaseType] = ((IntType <# "int") <|> (StrType <# "string") <|> (BoolType <# "bool") <|> (CharType <# "char")).hide
    private lazy val arrayType = ArrayType((baseType <|> pairType), count("[" <~ "]"))
    private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")
    private lazy val pairElemType: Parsley[PairElemType] = (Pair <# "pair") <|> attempt(baseType <~ notFollowedBy("[")) <|> arrayType
    
    private lazy val arglist = sepBy(expr, ",")
    
    private lazy val param = Parameter(types, Ident(VARIABLE))
    private lazy val params = sepBy(param, ",")
    private lazy val function = Function(attempt(types <~> Ident(VARIABLE) <~ "("), params <~ ")", "is" ~> nestedStatement <~ "end")
    lazy val functions = many(function)

    private lazy val call = Call("call" ~> ident, "(" ~> arglist <~ ")").hide.explain(s"To call a function: ${makeGreen("call <name of function>(<argument>,<argument>)")}\n")
    
    private lazy val assignLHS: Parsley[AssignLHS] = (attempt(arrayElem) <|> ident <|> pairElem).hide
    private lazy val assignRHS = expr.explain(explainExpr) <|> arrayLiter <|> newPair <|> pairElem.explain(explainPairElem) <|> call
    
    private lazy val nestedStatement = sepBy1(statement, ";")
    lazy val statement: Parsley[Statement] = 
        ((Skip <# "skip") <|> 
        AssignType(types, ident, "=" ~> assignRHS) <|> 
        Assign(assignLHS.explain(explainLHS), "=" ~> assignRHS) <|>
        Read("read" ~> assignLHS.explain("Read stores user input into a variable/array/pair\n")) <|>
        Free("free" ~> expr.explain("Free takes in an array/pair")) <|>
        Return("return" ~> expr.explain("Return used in function to give result")) <|>
        Exit("exit" ~> expr.explain("Exit exits the program with the given number")) <|>
        Print("print" ~> expr.explain("Print outputs to the console")) <|>
        Println("println" ~> expr.explain("Print outputs to the console on a new line")) <|>
        If("if" ~> expr.explain("If is followed by a boolean condition"), "then" ~> nestedStatement, "else" ~> nestedStatement <~ "fi") <|>
        While("while" ~> expr.explain("while is followed by a boolean condition"), "do" ~> nestedStatement <~ "done") <|>
        NestedBegin("begin" ~> nestedStatement <~ "end")
        ).hide.explain(explainStatement) //.hide
        
    lazy val program = "begin" ~> (Begin(functions, nestedStatement)) <~ "end"
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
        
    ).hide.label("operators")
    val result = fully(program)
}