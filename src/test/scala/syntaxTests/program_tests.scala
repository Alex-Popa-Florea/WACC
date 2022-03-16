package syntax_tests

import frontend.lexer._
import frontend.parser._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.Failure
import parsley.Success
import wacc.ast._

import scala.io.Source

class ProgramParserTest extends AnyFlatSpec with AppendedClues
{
  info("PROGRAM TESTS")

  "Basic Programs" should "parse successfully and produce correct AST" in
  {
    info("with skip statement")
    fully(program).parse("begin skip end") should matchPattern {
    case Success
    (
      Begin(List(), List(), List(Skip()))
    ) =>} withClue(" Success(Begin(List(), List(Skip())))")
    info("with exit statement")
    fully(program).parse("begin exit 7 end") should matchPattern {
    case Success
    (
      Begin(List(), List(), List(Exit(IntLiter(7))))
    ) =>} withClue( "Success(Begin(List(), List(Exit(IntLiter(7)))))")
    info("with basic if statement")
    fully(program).parse("begin if true then skip else skip fi end") should matchPattern {
    case Success
    (
      Begin
      (
        List(),
        List(),
        List
        (
          If(BoolLiter(true), List(Skip()), List(Skip()))
        )
      )
    ) =>} withClue( "Success(Begin(List(), List(If(BoolLiter(true), List(Skip()), List(Skip())))))")
    info("with print and println statements")
    fully(program).parse("begin print 1; println 'a' end") should matchPattern {
    case Success
    (
      Begin
      (
        List(),
        List(),
        List(Print(IntLiter(1)), Println(CharLiter('a')))
      )
    ) =>} withClue( "Success(Begin(List(), List(Print(IntLiter(1)), Println(CharLiter('a')))))")
    info("with assign and print statement")
    fully(program).parse("begin string s = \"hello world!\"; println s end") should matchPattern {
    case Success
    (
      Begin
      (
        List(),
        List(),
        List
        (
          AssignType(StrType(), VarIdent("s"), StrLiter("hello world!")),
          Println(VarIdent("s"))
        )
      )
    ) =>} withClue("Success(Begin(List(), List(AssignType(StrType(), VarIdent(\"s\"), StrLiter(\"hello world!\")), Println(VarIdent(\"s\")))))")
  }

  "Nested Begins" should "parse successfully and produce correct AST" in
  {
    info("with basic scope")
    fully(program).parse("begin skip; begin skip end end") should matchPattern {
    case Success
    (
      Begin
      (
        List(), 
        List(),
        List
        (
          Skip(),
          NestedBegin(List(Skip()))
        )
      )
    ) =>} withClue("Success(Begin(List(), List(Skip(), NestedBegin(List(Skip()))")
  }

  "Function Calls" should "parse successfully and produce correct AST" in
  {
    info("with basic call")
    fully(program).parse("begin int f() is return 0 end int x = call f() ; println x end") should matchPattern {
    case Success
    (
      Begin
      (
        List(),
        List
        (
          Function
          (
            IntType(),
            VarIdent("f"),
            List(),
            List
            (
              Return(IntLiter(0)) 
            )
          )
        ),
        List
        (
          AssignType(IntType(), VarIdent("x"), Call(VarIdent("f"), List())),
          Println(VarIdent("x"))
        )
      )
    ) =>} withClue("Success(Begin(List(Function(IntType(), VarIdent(\"f\"), List(), List(Return(IntLiter(0))))), List(AssignType(IntType(), VarIdent(\"x\"), Call(VarIdent(\"f\"), List())), Println(VarIdent(\"x\")))))")
    info("with two function definitions")
    fully(program).parse("begin int f() is return 2 end int g() is int x = call f(); return x * 2 end int y = call g(); println y end") should matchPattern {
    case Success
    (
      Begin
      (
        List(),
        List
        (
          Function
          (
            IntType(),
            VarIdent("f"),
            List(),
            List
            (
              Return(IntLiter(2))
            )
          ),
          Function
          (
            IntType(),
            VarIdent("g"),
            List(),
            List
            (
              AssignType(IntType(), VarIdent("x"), Call(VarIdent("f"), List())),
              Return(Mul(VarIdent("x"), IntLiter(2)))
            )
          )
        ),
        List
        (
          AssignType(IntType(), VarIdent("y"), Call(VarIdent("g"), List())),
          Println(VarIdent("y"))
        )
      )
    ) =>} withClue("Success(Begin(List(Function(IntType(), VarIdent(\"f\"), List(),List(Return(IntLiter(2)))), Function(IntType(), VarIdent(\"g\"), List(), List(AssignType(IntType(), VarIdent(\"x\"), Call(VarIdent(\"f\"), List())), Return(Mul(VarIdent(\"x\"), IntLiter(2)))))), List(AssignType(IntType(), VarIdent(\"y\"), Call(VarIdent(\"g\"), List())), Println(VarIdent(\"y\")))))")
  }
}