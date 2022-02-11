package wacc

import parsley.Parsley, Parsley._
import parsley.debug._

object parser {
    import lexer.implicits.implicitLexeme
    import lexer._
    import ast._
    import parsley.character.digit
    import parsley.expr.{precedence, InfixL, InfixR, NonAssoc, Prefix, Ops}
    import parsley.combinator._
    import parsley.errors.combinator.ErrorMethods
    import color._


    /* Explanations for each type of syntax error to help make the error messages
       more informative */
    val explainStatement = 
        s"""|   Missing Statement(s):
            |   Statements are simply instructions.
            |         To do nothing: ${makeGreen("skip")}
            |         To assign a variable: ${makeGreen("<type> <name> = <expression>")}
            |         To Reassign variables: ${makeGreen("<variable> = <expression>")}
            |         To take in input and store in a variable from user: ${makeGreen("read <variable>")}
            |         To free pairs or arrays (not nested): ${makeGreen("free <variable>")}
            |         To give something from a function: ${makeGreen("return <expressions>")}
            |         To exit the program with an error number: ${makeGreen("exit <number>")}
            |         To print with out a new line: ${makeGreen("print <expression>")}
            |         To print with a new line: ${makeGreen("print <expression>")}
            |         To use an if statement: ${makeGreen("if <condition of type bool> <statement(s)> else <statement(s)> fi")}
            |         To repeat until a condition is met: ${makeGreen("While <condition> do <statement(s)> done")}
            |         To have nested begin statemenets: ${makeGreen("begin <statement(s)> end")}
            |         To write multiple statements ${makeGreen("<statement> ; <statment")}
            |         Variables are Expressions!
            |""".stripMargin

    val explainExpr =
    s"""|   Expressions:
        |      Expressions are a combinations of values and operators.
        |      Some operators take 2 arguments and others just 1 describe below:
        |      Not, Negate a bool  ${makeGreen("!<bool>")}
        |      Negation, negate a number ${makeGreen("-<int>")}
        |      Length, get the length of an array ${makeGreen("len <array>")}
        |      Ordinal, gives the ASCII value of the character ${makeGreen("ord <variable>")}
        |      Character, gives the ASCII charcter from the given number ${makeGreen("chr <int>")}
        |      Multiply, gives the  ${makeGreen("<int> * <int>")}
        |      Divide, gives the integer divison of 2 ints  ${makeGreen("<int> / <int>")}
        |      Mod, gives the remainder of dividing 2 ints ${makeGreen("<int> % <int>")}
        |      Add, gives the addition of 2 ints ${makeGreen("<int> + <int>")}
        |      Sub, gives the subtraction of 2 ints ${makeGreen("<int> - <int>")}
        |      Greater Than, gives true if LHS greater than RHS ${makeGreen("<int> > <int>")}
        |      Greater Than Or Equal To, gives true if LHS greater than or equal to RHS ${makeGreen("<int> >= <int>")}
        |      Less Than, gives true if LHS less than RHS ${makeGreen("<int> < <int>")}
        |      Less Than Or Equal To, gives true if LHS less than or equal toRHS ${makeGreen("<int> <= <int>")}
        |      Equals, gives true if LHS is equal RHS ${makeGreen("<variable> == <variable>")}
        |      Not Equals, gives true if LHS is not equal RHS ${makeGreen("<variable> != <variable>")}
        |      Logical And,gives true if both bools are true ${makeGreen("<bool> && <bool>")}
        |      Logical Or,gives true if one of the bools are true ${makeGreen("<bool> || <bool>")}
        |""".stripMargin
    val explainLHS = 
        s"""|   Left hand side of an assignment, left of the equals:
            |       Can be an array element: ${makeGreen("<array>[<position>] =")}
            |       Can be a variable: ${makeGreen("<variable> = ")}
            |       Can be a pair element: ${makeGreen("fst <pair> =")} 
            |                              ${makeGreen("snd <pair> =")}""".stripMargin
    val explainPairElem = 
        s"""|   Accessing pairs using fst and snd are used to access pairs:
            |      to get the first element ${makeGreen("fst <pair>")}
            |      to get the second element ${makeGreen("snd <pair>")}
            |""".stripMargin
         

    /* Parsers for literals and identifiers, used to construct the apporopriate AST nodes */
    private lazy val ident = Ident(VARIABLE).label("Variable").explain("Use to name expression")

    private lazy val charLiter = CharLiter(CHAR)
    private lazy val intLiter = IntLiter(INTEGER)
    private lazy val boolLiter = BoolLiter(("true" ~> pure(true)) <|> ("false" ~> pure(false)))
    private lazy val stringLiter = StrLiter(STRING)

    /* Constructs an AST ArrayLiter node by parsing a comma seperated list surrounded by square brackets */
    private lazy val arrayLiter = ("[" ~> ArrayLiter(sepBy(expr, ",")) <~ "]").hide.explain(s"Array " +
      s"can be constructed by having\n  elements, if any, seperated by commas surrounded in square " +
      s"brackets ${makeGreen("[<expression>,<expression>...]")}\n")

    /* Parser for a pair literal (i.e. a null pair) */
    private lazy val pairLiter = (PairLiter <# "null")

    /* Parsers for array and pair elements that construct the appropriate AST nodes
       Array elements are made up of an array variable name followed by an expression 
       (index of element) in square brackets */
    private lazy val arrayElem = ArrayElem(ident, endBy1("[" ~> expr, "]"))
    /* Pair elements are made up of the key word fst or snd followed by an expression (identifier of a pair) */
    private lazy val pairElem: Parsley[PairElem] = ("fst" ~> Fst(expr)) <|> ("snd" ~> Snd(expr)).hide

    /* Parser to construct a NewPair AST node*/
    private lazy val newPair = "newpair" ~> NewPair("(" ~> expr, "," ~> expr <~ ")").hide.explain(s"Pairs" +
      s" are contrusted using newpair: ${makeGreen("newpair (<expression>, <expression>)")}\n")


    /* The following parsers parse the type keywords and construct the appropriate AST node.
       Base types and pair types can't be followed by square brackets, as these are parsed
       in array type parser. */

    lazy val types: Parsley[Type] = (attempt((baseType <|> pairType) <~ notFollowedBy("[")) <|> arrayType).hide

    private lazy val baseType: Parsley[BaseType] = ((IntType <# "int") <|> (StrType <# "string") <|> (BoolType <# "bool") <|> (CharType <# "char")).hide
    /* Constructing the ArrayType node using the count parser to store how nested the array is */
    private def count(p: =>Parsley[_]): Parsley[Int] = p.foldLeft(0)((n, _) => n + 1)
    private lazy val arrayType = ArrayType((baseType <|> pairType), count("[" <~ "]"))

    private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType <~ ",", pairElemType <~ ")")

    /* A pair element type can be the pair keyword, a base type or an array type */
    private lazy val pairElemType: Parsley[PairElemType] = (Pair <# "pair") <|> attempt(baseType <~ notFollowedBy("[")) <|> arrayType
    
    /* Parsers for functions */

    /* The list of arguments for a function is comma seperated */
    private lazy val arglist = sepBy(expr, ",")
    
    /* Function parameters are made up of their type and their identifier, and 
       comma seperated in the function declaration */
    private lazy val param = Parameter(types, Ident(VARIABLE))
    private lazy val params = sepBy(param, ",")

    /* A function starts with a name and parameters, and contains statements, and ends with the end keyword */
    private lazy val function = Function(attempt(types <~> Ident(VARIABLE) <~ "("), params <~ ")", "is" ~> nestedStatement <~ "end")
    lazy val functions = many(function)

    /* Parses a function call and creates an AST node for it, which stores the name 
       of the function being called and its arguments */
    private lazy val call = Call("call" ~> ident, "(" ~> arglist <~ ")").hide.explain(s"To call a " +
      s"function: ${makeGreen("call <name of function>(<argument>,<argument>)")}\n")
    
    /* Parsers for the left and right hand of an assignment statement */
    private lazy val assignLHS: Parsley[AssignLHS] = (attempt(arrayElem) <|> ident <|> pairElem).hide
    private lazy val assignRHS = expr.explain(explainExpr) <|> arrayLiter <|> newPair <|> pairElem.explain(explainPairElem) <|> call
    
    /* Parsers for statements
       Nested Statements are seperated by semi-colons */
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
        ).hide.explain(explainStatement) 
    
    /* Programs must start with a begin, any functions are defined next, followed by statements and the end keyword */
    lazy val program = "begin" ~> (Begin(functions, nestedStatement)) <~ "end"

    /* Expressions in brackets are treated like atoms */
    private lazy val atom =  
        "(" ~> expr <~ ")" <|> attempt(arrayElem) <|> ident <|> charLiter <|> intLiter <|> boolLiter <|> stringLiter <|> pairLiter 


    /* Precendence table
       Lists the operators in descending order of precedence, whether they are prefix or infix
       and the corresponding AST node to construct */
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
        
    ).hide.label("operators")
    val result = fully(program)
}