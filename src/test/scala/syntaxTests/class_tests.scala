package syntaxTests

import frontend.lexer._
import frontend.parser._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success
import wacc.ast._
import wacc.main.CLASSES


class ExprParserTest extends AnyFlatSpec with AppendedClues{
    info("CLASS TESTS")
    CLASSES = true
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
        
    
    }
    
}
