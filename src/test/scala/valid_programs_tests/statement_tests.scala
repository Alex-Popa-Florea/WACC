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

    "Skip statements" should "parse successfully and produce a correct AST" in 
    {
        fully(statement).parse("skip") should matchPattern {
        case Success
        (
            Skip()
        ) =>} withClue(" Success(Skip())")
    }

    "Assignment to array literal" should "parse successfully and produce a correct AST" in 
    {
        info("singleton array")
        fully(statement).parse("int [] x = [1]") should matchPattern {
        case Success
        (
            AssignType
            (
                ArrayType(IntType(), 1),
                Ident("x"), 
                ArrayLiter(List(IntLiter(1)))
            )
        ) =>} withClue(" Success(AssignType(ArrayType(IntType(), 1), " +
                       "Ident(\"x\"), ArrayLiter(List(IntLiter(1)))))")

        info("array with multiple elements")
        fully(statement).parse("bool [] arr = [true, false, true]") should matchPattern {
        case Success
        (
            AssignType
            (
                ArrayType(BoolType(), 1),
                Ident("arr"), 
                ArrayLiter(List(BoolLiter(true), BoolLiter(false), BoolLiter(true)))
            )
        ) =>} withClue(" Success(AssignType(ArrayType(BoolType(), 1), Ident(\"arr\"), " +
                       "ArrayLiter(List(BoolLiter(true), BoolLiter(false), BoolLiter(true)))))")

    }

    "Assignment to newpair statements" should "parse successfully and produce a correct AST" in {
        fully(statement).parse("pair(int, int) p = newpair(1,2)") should matchPattern {
        case Success
        (
            AssignType
            (
                PairType(IntType(), IntType()),
                Ident("p"), 
                NewPair(IntLiter(1), IntLiter(2))
            )
        ) =>} withClue(" Success(AssignType(PairType(IntType(), IntType()), " +
                       "Ident(\"p\"), NewPair(IntLiter(1), IntLiter(2))))")
    }

    "Assignment to call statement" should "parse successfully and produce a correct AST" in {
        info("call function with no arguments")
        fully(statement).parse("bool res = call foo()") should matchPattern{
        case Success
        (
            AssignType
            (
                BoolType(),
                Ident("res"), 
                Call(Ident("foo"), List())
            )
        ) =>} withClue(" Success(AssignType(BoolType(), Ident(\"res\"), " +
                       "Call(Ident(\"foo\"), List())))")

        info("call function with arguments")
        fully(statement).parse("int res = call foo(1, true)") should matchPattern{
        case Success
        (
            AssignType
            (
                IntType(),
                Ident("res"), 
                Call(Ident("foo"), List(IntLiter(1), BoolLiter(true)))
            )
        ) =>} withClue(" Success(AssignType(IntType(), Ident(\"res\"), " +
                       "Call(Ident(\"foo\"), List(IntLiter(1), BoolLiter(true)))))")
    }
            

    "Assignment to expression statements" should "parse successfully and produce a correct AST" in {

        info("assign array element to expression")
        fully(statement).parse("arr[0] = 42") should matchPattern{
        case Success
        (
            Assign
            (
                ArrayElem(Ident("arr"), List(IntLiter(0))), 
                IntLiter(42)
            )
        ) =>} withClue(" Success(Assign(ArrayElem(Ident(\"arr\")," +
                       " List(IntLiter(0))), IntLiter(42)))")

        info("assign pair element to expression")
        fully(statement).parse("fst p = true") should matchPattern
            {case Success(Assign(Fst(Ident("p")), BoolLiter(true)))
        =>} withClue(" Success(Assign(Fst(Ident(\"p\")), BoolLiter(true)))")

        info("assign identifier to expression")
        fully(statement).parse("var = 1 + 2") should matchPattern
            {case Success(Assign(Ident("var"), Add(IntLiter(1), IntLiter(2))))
        =>} withClue(" Success(Assign(Ident(\"var\"), Add(IntLiter(1), IntLiter(2))))")
    }

    "Read statements" should "parse successfully and produce a correct AST" in {
        info("read into identifier")
        fully(statement).parse("read input") should matchPattern
            {case Success(Read(Ident("input")))
        =>} withClue(" Success(Read(Ident(\"input\")))")

        info("read into pair elem")
        fully(statement).parse("read snd p") should matchPattern
            {case Success(Read(Snd(Ident("p"))))
        =>} withClue(" Success(Read(Snd(Ident(\"p\"))))")

        info("read into array elem")
        fully(statement).parse("read arr[0][2]") should matchPattern
            {case Success(Read(ArrayElem(Ident("arr"), List(IntLiter(0), IntLiter(2)))))
        =>} withClue(" Success(Read(ArrayElem(Ident(\"arr\"), List(IntLiter(0), IntLiter(2)))))")
    }

    "Free statements" should "parse successfully and produce a correct AST" in {
        fully(statement).parse("free arr[0]") should matchPattern
            {case Success(Free(ArrayElem(Ident("arr"), List(IntLiter(0)))))
        =>} withClue(" Success(Free(ArrayElem(Ident(\"arr\"), List(IntLiter(0)))))")
    }

    "Return statements" should "parse successfully and produce a correct AST" in {
        fully(statement).parse("return result") should matchPattern
            {case Success(Return(Ident("result")))
        =>} withClue(" Success(Return(Ident(\"result\")))")
    }

    "Exit statements" should "parse successfully and produce a correct AST" in pending

    "Print statements" should "parse successfully and produce a correct AST" in pending

    "Print line statements" should "parse successfully and produce a correct AST" in pending

    "If statements" should "parse successfully and produce a correct AST" in pending

    "While statements" should "parse successfully and produce a correct AST" in pending

    "Begin and end statements" should "parse successfully and produce a correct AST" in pending

    "Nested statements" should "parse successfully and produce a correct AST" in pending


}