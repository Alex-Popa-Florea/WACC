package syntax_tests

import frontend.lexer._
import frontend.parser._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Success
import wacc.ast._

class ExprParserTest extends AnyFlatSpec with AppendedClues{
    
    info("EXPRESSION TESTS")

    "Integer Literals" should "parse successfully and produce correct AST" in {
    info("without a sign")
    fully(expr).parse("1") should matchPattern {
    case Success(IntLiter(1)
    ) =>} withClue(" Success(IntLiter(1))")

    info("with a positive a sign")
    fully(expr).parse("+1") should matchPattern {
    case Success(IntLiter(1)
    ) =>} withClue(" Success(IntLiter(1))")

    info("with a negative sign")
    fully(expr).parse("-1") should matchPattern {
    case Success(IntLiter(-1)
    ) =>} withClue(" Success(IntLiter(-1))")
  } 

    "Bool Literals" should "parse successfully and produce correct AST" in {
    info("true")
    fully(expr).parse("true") should matchPattern {
    case Success(BoolLiter(true)) =>} withClue(" Success(BoolLiter(true))")

    info("false")
    fully(expr).parse("false") should matchPattern {
    case Success(BoolLiter(false)) =>} withClue(" Success(BoolLiter(false))")

  } 

    "Char Literals" should "parse successfully and produce correct AST" in {
    val res = fully(expr).parse("'c'")
    res should matchPattern {
    case Success(CharLiter('c')) =>} withClue( " Success(CharLiter('c'))")
  } 

    "String Literals" should "parse successfully and produce correct AST" in {
    val res = fully(expr).parse("\"Hello\"")
    res should matchPattern {
    case Success(StrLiter("Hello")) =>} withClue( " Success(StrLiter(\"Hello\"))")
  } 

    "Pair Literals" should "parse successfully and produce correct AST" in {
    val res = fully(expr).parse("null")
    res should matchPattern {
    case Success(PairLiter()) =>} withClue( " Success(PairLiter())")
    }
  

    "Identifiers" should "parse successfully and produce correct AST" in {
    info("starting with lower case letter")
    fully(expr).parse("identifier1Name") should matchPattern {
    case Success(VarIdent("identifier1Name")
    ) =>} withClue( " Success(VarIdent(\"identifier1Name\"))")
    

    info("starting with upper case letter")
    fully(expr).parse("Identifier1Name") should matchPattern {
    case Success(VarIdent("Identifier1Name"))
     =>} withClue( " Success(VarIdent(\"Identifier1Name\"))")
  

    info("starting with an underscore letter")
    fully(expr).parse("_Identifier1Name") should matchPattern {
    case Success(VarIdent("_Identifier1Name")
    ) =>} withClue( " Success(VarIdent(\"_Identifier1Name\"))")
  }

    "Array elems" should "parse successfully and produce correct AST" in {
    info("single array elem")
    fully(expr).parse("array [1]") should matchPattern {
    case Success(ArrayElem(VarIdent("array"), List(IntLiter(1)))
    ) =>} withClue( " Success(ArrayElem(VarIdent(\"array\"), List(IntLiter(1))))")
    

    info("nested array elem")
    fully(expr).parse("array [1][2][3]") should matchPattern {
    case Success(ArrayElem(VarIdent("array"), List(IntLiter(1), IntLiter(2), IntLiter(3)))
    ) =>} withClue( " Success(ArrayElem(VarIdent(\"array\"), List(IntLiter(1), IntLiter(2), IntLiter(3))))")

  }

    "Unary operators" should "parse successfully and produce correct AST" in {
    info("logical not")
    fully(expr).parse("! true") should matchPattern {
    case Success(Not(BoolLiter(true))) =>} withClue( " Success(Not(BoolLiter(true)))")
    

    info("negate")
    fully(expr).parse("- (1)") should matchPattern {
    case Success(Neg(IntLiter(1))) =>} withClue( " Success(Neg(IntLiter(1)))")

    info("len")
    fully(expr).parse("len array") should matchPattern {
    case Success(Len(VarIdent("array"))) =>} withClue( " Success(Len(VarIdent(\"array\")))")

    info("ord")
    fully(expr).parse("ord 'c'") should matchPattern {
    case Success(Ord(CharLiter('c'))) =>} withClue( " Success(Ord(CharLiter('c')))")

    info("chr")
    fully(expr).parse("chr 20") should matchPattern {
    case Success(Chr(IntLiter(20))) =>} withClue( " Success(Chr(IntLiter(20)))")
  }

  "Binary operators" should "parse successfully and produce correct AST" in {
    info("multiply")
    fully(expr).parse("2 * 3") should matchPattern {
    case Success(Mul(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(Mul(IntLiter(2), IntLiter(3)))")

    info("divide")
    fully(expr).parse("6 / 3") should matchPattern {
    case Success(Div(IntLiter(6), IntLiter(3))
    ) =>} withClue( " Success(Div(IntLiter(6), IntLiter(3)))")

    info("modulus")
    fully(expr).parse("5 % 3") should matchPattern {
    case Success(Mod(IntLiter(5), IntLiter(3))
    ) =>} withClue( " Success(Mod(IntLiter(5), IntLiter(3)))")

    info("plus")
    fully(expr).parse("2 + 3") should matchPattern {
    case Success(Add(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(Add(IntLiter(2), IntLiter(3)))")

    info("minus")
    fully(expr).parse("3 - 1") should matchPattern {
    case Success(Sub(IntLiter(3), IntLiter(1))
    ) =>} withClue( " Success(Sub(IntLiter(3), IntLiter(1)))")

    info("greater than")
    fully(expr).parse("2 > 3") should matchPattern {
    case Success(GT(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(GT(IntLiter(2), IntLiter(3)))")

    info("greater than or equal")
    fully(expr).parse("2 >= 3") should matchPattern {
    case Success(GTE(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(GTE(IntLiter(2), IntLiter(3)))")

    info("less than")
    fully(expr).parse("2 < 3") should matchPattern {
    case Success(LT(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(LT(IntLiter(2), IntLiter(3)))")

    info("less than or equal to")
    fully(expr).parse("2 <= 3") should matchPattern {
    case Success(LTE(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(LTE(IntLiter(2), IntLiter(3)))")

    info("equal to")
    fully(expr).parse("2 == 3") should matchPattern {
    case Success(EQ(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(EQ(IntLiter(2), IntLiter(3)))")

    info("not equal to")
    fully(expr).parse("2 != 3") should matchPattern {
    case Success(NEQ(IntLiter(2), IntLiter(3))
    ) =>} withClue( " Success(NEQ(IntLiter(2), IntLiter(3)))")

    info("logical and")
    fully(expr).parse("true && false") should matchPattern {
    case Success(And(BoolLiter(true), BoolLiter(false))
    ) =>} withClue( " Success(And(BoolLiter(true), BoolLiter(false)))")

    info("logical or")
    fully(expr).parse("true || false") should matchPattern {
    case Success(Or(BoolLiter(true), BoolLiter(false))
    ) =>} withClue( " Success(Or(BoolLiter(true), BoolLiter(false)))")

    info("arithmetic precendence works")
    fully(expr).parse("2 + 3 * 4") should matchPattern {
    case Success(Add(IntLiter(2), Mul(IntLiter(3), IntLiter(4)))
    ) =>} withClue( " Success(Add(IntLiter(2), Mul(IntLiter(3), IntLiter(4))))")

    info("logical precendence works")
    fully(expr).parse("true || false && false") should matchPattern {
    case Success(Or(BoolLiter(true), And(BoolLiter(false), BoolLiter(false)))
    ) =>} withClue( " Success(Or(BoolLiter(true), And(BoolLiter(false), BoolLiter(false))))")
  }

  "Parentheses" should "parse successfully and produce a correct AST" in {
    info("around expression")
    fully(expr).parse("(1 + 2)") should matchPattern {
    case Success(Add(IntLiter(1), IntLiter(2))
    ) =>} withClue( " Success(Add(IntLiter(1), IntLiter(2)))")

    info("nested parentheses")
    fully(expr).parse("((1 * 2))") should matchPattern {
    case Success(Mul(IntLiter(1), IntLiter(2))
    ) =>} withClue( " Success(Mul(IntLiter(1), IntLiter(2)))")

    info("arithmetic precedence works with parentheses")
    fully(expr).parse("-(1 + 2) * 3") should matchPattern {
    case Success(Mul(Neg(Add(IntLiter(1), IntLiter(2))), IntLiter(3))
    ) =>} withClue( " Success(Mul(Neg(Add(IntLiter(1), IntLiter(2))), IntLiter(3)))")

    info("logical precedence works with parentheses")
    fully(expr).parse("(true || false) && true") should matchPattern {
    case Success(And(Or(BoolLiter(true), BoolLiter(false)), BoolLiter(true))
    ) =>} withClue( " Success(And(Or(BoolLiter(true), BoolLiter(false)), BoolLiter(true)))")
  }

}