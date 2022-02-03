package valid_programs_tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.io.Source
import parsley.{Success, Failure}
import wacc.lexer._
import wacc.parser._
import wacc.ast._
import org.scalatest.AppendedClues

class TypeParserTest extends AnyFlatSpec with AppendedClues
{

  info("TYPE TESTS")

  "Base Types" should "parse successfully and produce correct AST" in
  {
    info("with type 'int'")
    fully(types).parse("int") should matchPattern {
    case Success
    (
      IntType()
    ) =>} withClue(" Success(IntType())")
    info("with type 'bool'")
    fully(types).parse("bool") should matchPattern {
    case Success
    (
      BoolType()
    ) =>} withClue("Success(BoolType())")
    info("with type 'char'")
    fully(types).parse("char") should matchPattern {
    case Success
    (
      CharType()
    ) =>} withClue("Success(CharType())")
    info("with type 'string'")
    fully(types).parse("string") should matchPattern {
    case Success
    (
      StrType()
    ) =>} withClue("Success(StrType())")
    info("with type 'pair(int, char)'")
    fully(types).parse("pair(int, char)") should matchPattern {
    case Success
    (
      PairType(IntType(), CharType())
    ) =>} withClue("Success(Pair(IntType(), CharType()))")
  }
  "Array Types" should "parse successfully and produce correct AST" in
  {
    info("with type 'int[]'")
    fully(types).parse("int[]") should matchPattern {
    case Success
    (
      ArrayType(IntType(), 1)
    ) =>} withClue(" Success(ArrayType(IntType(), 1))")
    info("with type 'bool[][]'")
    fully(types).parse("bool[][]") should matchPattern {
    case Success
    (
      ArrayType(BoolType(), 2)
    ) =>} withClue(" Success(ArrayType(BoolType(), 2))")
    info("with type 'int[][][]'")
    fully(types).parse("int[][][]") should matchPattern {
    case Success
    (
      ArrayType
      (
        IntType(),
        3
      )
    ) =>} withClue(" Success(ArrayType(IntType(), 3))")
  }
}