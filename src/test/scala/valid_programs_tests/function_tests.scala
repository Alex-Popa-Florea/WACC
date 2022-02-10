package valid_programs_tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.io.Source
import parsley.{Success, Failure}
import wacc.lexer._
import wacc.symbolTable._
import wacc.functionTable._
import wacc.semanticAnalyser._
import wacc.parser._
import wacc.ast._
import org.scalatest.AppendedClues

class FunctionParserTest extends AnyFlatSpec with AppendedClues
{
  info("FUNCTION TESTS")

  "Incorrect functions and return statements" should "produce syntax errors" in
  {
    info("with stray return statement")
    var answer = result.parse("begin return 1 end")
    answer match {
      case Success(p) => {
        var error = syntaxAnalyser(p)
        error match {
            // Check function has syntax error
            case Some(e) => e should equal ("Cannot have return statement in main",(1,7))
            case _ => 
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with function without return statement")
    answer = result.parse("begin int f() is int a = 2 skip end")
    answer match {
      case Success(p) => {
        answer should equal (0)
      }
      case Failure(err) => {
        err should equal ("""(line 1, column 28):
                            |  unexpected "ski"
                            |  expected !=, ";", %, &&, *, +, -, /, <, <=, ==, >, >=, end, or ||
                            |  >begin int f() is int a = 2 skip end
                              ^""".stripMargin)
      }
    }
    def syntaxAnalyser(p: Node): (Option[(String, (Int, Int))]) = { 
        val symbolTable = new SymbolTable("Program", None)
        val functionTable = new FunctionTable()
        analyse(p, symbolTable, functionTable, None)
        returnTypeError
    }
  }
}