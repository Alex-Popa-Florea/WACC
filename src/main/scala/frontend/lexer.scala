package frontend

import parsley.Parsley

import Parsley._
import scala.collection.mutable.ListBuffer

object lexer {
    import parsley.token.{Lexer, Predicate, LanguageDef}
    import parsley.character.{alphaNum, upper, lower, newline, digit, char, string, satisfy}
    import parsley.combinator.{many, option, eof, between}
    import parsley.implicits.character.{stringLift, charLift}
    import parsley.implicits.zipped.Zipped3
    import parsley.errors.combinator.ErrorMethods

    val classFlag = true
    val basicKeywords = ListBuffer("begin", "end", "is", "skip", "read", "free", "return", 
                                    "exit", "print", "println", "if", "then", "else", "fi", 
                                    "while", "do", "done", "newpair", "call", "fst", "snd", 
                                    "pair", "true", "false", "null", "int", "bool", "char", 
                                    "string", "len", "ord", "chr")
    val keywords = if (!classFlag) {
            basicKeywords.toSet
        } else {
           (basicKeywords.append("class", "ssalc", "has", "public", "private", "newinstance", "extends", "this")).toSet
        }

    /*
        The language definition for the wacc language.

    */
    private val wacc = LanguageDef.plain.copy(
        commentLine = "#",
        keywords = keywords,
        operators = Set("!", "len", "ord", "chr", "*", "/", "%", "+", "-", ">",
                        ">=", "<", "<=", "==", "!=", "&&", "||"),
        identStart = Predicate(c => c.isLetter || c == '_'),
        identLetter = Predicate(c => c.isLetterOrDigit || c == '_'),
        space = Predicate(c => c == ' ' || c == '\t' || c == '\n')
    )

    val lexer = new Lexer(wacc)

    /*
        Helper vals for character lexing, a character can either be a letter or an 
        escape character
    */
    private lazy val charLetter = satisfy(c => c != '\\' && c != '\'' && c != '\"' && c > '\u0016') 
    private lazy val charEscape = '\\'  ~> ('0' #> '\u0000' <|> 'b' #> '\b' <|> 't' #> '\t' <|> 'n' #> '\n' <|> 'f' #> '\f' <|> 'r' #> '\r' <|> '\"' <|> '\'' <|> '\\') 

    private lazy val characterChar = (charLetter <|> charEscape).label("literal character")
    
    val VARIABLE = lexer.identifier
    /*
        Integers are stored as bigints to begin with, then they are checked for
        overflow in the constructor.
    */
    val INTEGER: Parsley[BigInt] = lexer.lexeme(attempt('-' ~> many(" ") ~> (digit.foldLeft1(BigInt(0))((x, d) => x * 10 + d.asDigit)).map(x => -1 * x)) <|> (option('+') ~> many(" ") ~> digit.foldLeft1(BigInt(0))((x, d) => x * 10 + d.asDigit)))
    
    /*
        Chars and strings need to be placed between single quotes and double quotes
        respectively.
    */
    val CHAR = lexer.lexeme(between('\''.label("character"), '\''.label("end of character"), characterChar))
    val STRING = lexer.lexeme(between('\"'.label("string"), '\"'.label("end of string"), many(characterChar))).map(_.mkString)

    /*
        Function that parses whitespace and end of file, ignoring them. 
    */
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.whiteSpace ~> p <~ eof 

    object implicits {
        implicit def implicitLexeme(s: String): Parsley[Unit] = {
            if (wacc.keywords(s))       lexer.keyword(s)
            else if (wacc.operators(s)) lexer.maxOp(s)
            else                           void(lexer.symbol_(s))
        }
    }
}
