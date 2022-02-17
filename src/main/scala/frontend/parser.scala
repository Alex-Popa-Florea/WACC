package frontend

import parsley.Parsley
import parsley.debug._

import Parsley._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import wacc.ast._
    import parsley.character.digit
    import parsley.expr.{precedence, InfixL, InfixR, NonAssoc, Prefix, Ops}
    import parsley.combinator._
    import parsley.errors.combinator.ErrorMethods
    import explanations._
    import color._

    /* 
        Parsers for literals and identifiers, used to construct the apporopriate AST nodes 
    */
    private lazy val ident = Ident(VARIABLE).label("Variable").explain("Use to name expression")

    private lazy val charLiter = CharLiter(CHAR)
    private lazy val intLiter = IntLiter(INTEGER)
    private lazy val boolLiter = BoolLiter(("true" ~> pure(true)) <|> ("false" ~> pure(false)))
    private lazy val stringLiter = StrLiter(STRING)

    /*
        Constructs an AST ArrayLiter node by parsing a comma seperated list surrounded by square brackets 
    */
    private lazy val arrayLiter = ("[" ~> ArrayLiter(sepBy(expr, ",")) <~ "]").explain(s"Array " +
      s"can be constructed by having\n  elements, if any, seperated by commas surrounded in square " +
      s"brackets ${makeGreen("[<expression>,<expression>...]")}\n")

    /* 
        Parser for a pair literal (i.e. a null pair) 
    */
    private lazy val pairLiter = (PairLiter <# "null")

    /* 
        Parsers for array and pair elements that construct the appropriate AST nodes
        Array elements are made up of an array variable name followed by an expression 
        (index of element) in square brackets 
    */
    private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))
    /*
        Pair elements are made up of the key word fst or snd followed by an expression (identifier of a pair) 
    */
    private lazy val pairElem: Parsley[PairElem] = ("fst" ~> Fst(expr)) <|> ("snd" ~> Snd(expr))

    /* 
        Parser to construct a NewPair AST node
    */
    private lazy val newPair = "newpair" ~> NewPair("(" ~> expr, "," ~> expr <~ ")").explain(s"Pairs" +
      s" are contrusted using newpair: ${makeGreen("newpair (<expression>, <expression>)")}\n")


    /*
        The following parsers parse the type keywords and construct the appropriate AST node.
        Base types and pair types can't be followed by square brackets, as these are parsed
        in array type parser. 
    */

    lazy val types: Parsley[Type] = (attempt((baseType <|> pairType) <~ notFollowedBy("[")) <|> arrayType)

    private lazy val baseType: Parsley[BaseType] = ((IntType <# "int") <|> (StrType <# "string") <|> (BoolType <# "bool") 
                                                <|> (CharType <# "char"))
    /* 
        Constructing the ArrayType node using the count parser to store how nested the array is 
    */
    private def count(p: =>Parsley[_]): Parsley[Int] = p.foldLeft(0)((n, _) => n + 1)
    private lazy val arrayType = ArrayType((baseType <|> pairType), count("[" <~ "]"))

    private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")

    /*
        A pair element type can be the pair keyword, a base type or an array type 
    */
    private lazy val pairElemType: Parsley[PairElemType] = (Pair <# "pair") <|> attempt(baseType <~ notFollowedBy("[")) <|> arrayType
    
    /* 
        Parsers for functions 
    */

    /* 
        The list of arguments for a function is comma seperated 
    */
    private lazy val arglist = sepBy(expr, ",")
    
    /* 
        Function parameters are made up of their type and their identifier, and 
        comma seperated in the function declaration 
    */
    private lazy val param = Parameter(types, Ident(VARIABLE))
    private lazy val params = sepBy(param, ",")

    /* 
        A function starts with a name and parameters, and contains statements, and ends with the end keyword 
    */
    private lazy val function = Function(attempt(types <~> Ident(VARIABLE) <~ "("), params <~ ")", "is" ~> nestedStatement <~ "end")
    lazy val functions = many(function)

    /* 
        Parses a function call and creates an AST node for it, which stores the name 
        of the function being called and its arguments 
    */
    private lazy val call = Call("call" ~> ident, "(" ~> arglist <~ ")").explain(s"To call a " +
      s"function: ${makeGreen("call <name of function>(<argument>,<argument>)")}\n")
    
    /* 
        Parsers for the left and right hand of an assignment statement 
    */
    private lazy val assignLHS: Parsley[AssignLHS] = (attempt(arrayElem) <|> ident <|> pairElem)
    private lazy val assignRHS = expr.explain(explainExpr) <|> arrayLiter <|> newPair <|> pairElem.explain(explainPairElem) <|> call
    
    /*
        Parsers for statements
        Nested Statements are seperated by semi-colons 
    */
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
        ).explain(explainStatement) 
    
    /* 
        Programs must start with a begin, any functions are defined next, followed by statements and the end keyword 
    */
    lazy val program = "begin" ~> (Begin(functions, nestedStatement)) <~ "end"

    /* 
        Expressions in brackets are treated like atoms 
    */
    private lazy val atom =  
        "(" ~> expr <~ ")" <|> attempt(arrayElem) <|> ident <|> charLiter <|> intLiter <|> boolLiter <|> stringLiter <|> pairLiter 


    /*
        Precendence table
        Lists the operators in descending order of precedence, whether they are prefix or infix
        and the corresponding AST node to construct 
    */
    lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
        Ops(Prefix)(Not  <# "!"),
        Ops(Prefix)(Neg  <# attempt("-" <~ notFollowedBy(digit))),
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
        
    ).label("operators")
    val result = fully(program)
}