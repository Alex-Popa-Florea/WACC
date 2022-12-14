package frontend

import color._
/* 
    Explanations for each type of syntax error to help make the error messages
    more informative 
*/

object explanations{
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
}