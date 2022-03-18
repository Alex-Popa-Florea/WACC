package syntaxTests

import frontend.lexer._
import frontend.parser._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success
import parsley.Failure
import wacc.ast._
import wacc.main.CLASSES


class ExprParserTest extends AnyFlatSpec with AppendedClues{
    CLASSES = true
    info("CLASS TESTS")
    "Class" should "produce a correct ast" in {
        info("Empty Class")
        fully(program).parse("begin class Shape() has ssalc skip end") should matchPattern{
        case Success(Begin(List(Class(VarIdent("Shape"),List(),None,List(),List())),List(),List(Skip()))) => } withClue("""Success(Begin(List(Class(VarIdent("Shape"),List(),None,List(),List())),List(),List()))""")
            
        info("Class with constructor")
        fully(program).parse("begin class Shape(int sides) has ssalc skip end") should matchPattern{
        case Success(Begin(
        List(Class(
        VarIdent("Shape"),List(Parameter(IntType(),VarIdent("sides"))),None,List(),List()))
        ,List(),List(Skip()))) => } withClue("""Success(Begin(List(Class(VarIdent("Shape"),List(Parameter(IntType(),VarIdent("sides"))),None,List(),List())),List(),List(Skip())))""")
        info("and end with ssalc")
        var answer = result.parse("begin class Shape() has skip end")
        answer match {
            case Success(p) => {
                answer should equal (0)
            }
            case Failure(err) => {
                err should equal ("""|(line 1, column 25):
                                     |  unexpected "skip"
                                     |  expected private, public, or ssalc
                                     |  >begin class Shape() has skip end
                                     |                           ^""".stripMargin)
            }
        }
        info("with correct object creation")
        answer = result.parse("begin class Shape() has ssalc class Shape circle = Shape() end")
        answer match {
            case Success(p) => {
                answer should equal (0)
            }
            case Failure(err) => {
                err should equal ("""|(line 1, column 57):
                                     |  unexpected "()"
                                     |  expected !=, ";", "[", %, &&, *, +, -, /, <, <=, ==, >, >=, end, or ||
                                     |  >begin class Shape() has ssalc class Shape circle = Shape() end
                                     |                                                           ^""".stripMargin)
            }
        }
        info("with class objects with scope")
        answer = result.parse("begin class Shape() has int x = 2 ssalc skip end")
        answer match {
            case Success(p) => {
                answer should equal (0)
            }
            case Failure(err) => {
                println("LINE 49  " + err)
                err should equal ("""|(line 1, column 25):
                                     |  unexpected "int"
                                     |  expected private, public, or ssalc
                                     |  >begin class Shape() has int x = 2 ssalc skip end
                                     |                           ^""".stripMargin)
            }
        }
    }
}
