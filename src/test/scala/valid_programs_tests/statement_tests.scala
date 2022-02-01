package valid_programs_tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success
import wacc.lexer._
import wacc.parser._
import wacc.ast._
import org.scalatest.AppendedClues

class StatParserTest extends AnyFlatSpec with AppendedClues{
    
    info("STATEMENT TESTS")

    "Skip statements" should "parse successfully and produce a correct AST" in {
    fully(statement).parse("skip") should matchPattern {case Success(Skip()) =>} withClue(" Success(Skip())")

    }

    "Assignment to array literal" should "parse successfully and produce a correct AST" in {
        info("singleton array")
        fully(statement).parse("int [] x = [1]") should matchPattern 
            {case Success(AssignType(ArrayType(IntType(), 1), Ident("x"), ArrayLiter(List(IntLiter(1))))) 
            =>} withClue(" Success(AssignType(ArrayType(IntType(), 1), Ident(\"x\"), ArrayLiter(List(IntLiter(1)))))")

        info("array with multiple elements")
        fully(statement).parse("bool [] arr = [true, false, true]") should matchPattern 
            {case Success(AssignType(ArrayType(BoolType(), 1),Ident("arr"), ArrayLiter(List(BoolLiter(true), BoolLiter(false), BoolLiter(true))))) 
            =>} withClue(" Success(AssignType(ArrayType(BoolType(), 1), Ident(\"arr\"), ArrayLiter(List(BoolLiter(true), BoolLiter(false), BoolLiter(true)))))")

    }

    "Newpair statements" should "parse successfully and produce a correct AST" in pending

    //arglist?

    "Call statements" should "parse successfully and produce a correct AST" in pending

    "Pair elements" should "parse successfully and produce a correct AST" in pending

    "Assign statements" should "parse successfully and produce a correct AST" in pending

    "Read statements" should "parse successfully and produce a correct AST" in pending

    "Free statements" should "parse successfully and produce a correct AST" in pending

    "Return statements" should "parse successfully and produce a correct AST" in pending

    "Exit statements" should "parse successfully and produce a correct AST" in pending

    "Print statements" should "parse successfully and produce a correct AST" in pending

    "Print line statements" should "parse successfully and produce a correct AST" in pending

    "If statements" should "parse successfully and produce a correct AST" in pending

    "While statements" should "parse successfully and produce a correct AST" in pending

    "Begin and end statements" should "parse successfully and produce a correct AST" in pending

    "Nested statements" should "parse successfully and produce a correct AST" in pending


}