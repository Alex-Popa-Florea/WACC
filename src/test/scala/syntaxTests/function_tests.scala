package syntax_tests

import frontend.lexer._
import frontend.parser._
import frontend.semanticAnalyser._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Failure
import parsley.Success
import wacc.ast._
import wacc.functionTable._
import wacc.symbolTable._
import wacc.classTable._
import wacc.section._

import scala.io.Source

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
        val symbolTable = new SymbolTable(ProgramSection(), None)
        val functionTable = new FunctionTable(ProgramSection(), None)
        val classTable = new ClassTable()
        analyse(p, symbolTable, functionTable, classTable, None, false)
        returnTypeError
    }
  }
}