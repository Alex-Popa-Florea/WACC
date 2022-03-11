package semantic_tests

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
import wacc.types._
import wacc.section._
import wacc.arrayBounds._

import scala.collection.mutable.ListBuffer
import scala.io.Source

class SemanticTest extends AnyFlatSpec with AppendedClues{
  info ("SEMANTIC TESTS")

  "Assignment statements" should "parse successfully and produce correct symbol table" in
  {
    info("with constant assignments")
    var answer = result.parse("begin int i = 2; bool b = true; char c = 'a'; string h = \"hello\" end")
    answer match {
      case Success(p) => {
        // Check variable map is generated correctly.
        analyser(p)._1.getVariableMap() should equal (Map("i" -> (IntCheck(0), 4, Unknown()), "b" -> (BoolCheck(0), 5, Unknown()), "c" -> (CharCheck(0), 6, Unknown()), "h" -> (StrCheck(0), 10, Unknown())))
        // Check function table is empty.
        analyser(p)._2.getFuncMap().size should equal (0)
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Conditional statement programs" should "parse successfully and have children symbol tables" in
  {
    info("with if statement")
    var answer = result.parse("begin int a = 13; if a == 13 then println \"correct\" else println \"incorrect\" fi end")
    answer match {
      case Success(p) => {
        // Check program with if statement has two children i.e true table and false table.
        analyser(p)._1.getChildren().size should equal (2)
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with while statement")
    answer = result.parse("begin bool b = true ; while b do println \"flip b!\" ; b = !b done ; println \"end of loop\" end")
    answer match {
      case Success(p) => {
        // Check program with while statement has 1 child.
        analyser(p)._1.getChildren().size should equal (1)
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Functions" should "parse successfully and correct function tables" in
  {
    info("with no parameters")
    var answer = result.parse("begin int f() is return 0 end skip end")
    answer match {
      case Success(p) => {
        var funcMap = analyser(p)._2.getFuncMap()
        // Check function map contains only one key value pair.
        funcMap.size should equal (1)
        // Check function f's paramater list has size 0.
        funcMap.withFilter({case (name, (_, ts)) => name == "f"}).map({case (_, (_, ts)) => ts}) should equal (List(List()))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with parameters")
    answer = result.parse("begin int f(int a, int b) is int c = 5; return a + b end skip end")
    answer match {
      case Success(p) => {
        var funcMap = analyser(p)._2.getFuncMap()
        // Check function map contains only one key value pair.
        funcMap.size should equal (1)
        // Check function f's paramater list has two elements.
        funcMap.withFilter({case (name, (_, ts)) => name == "f"}).
        map({case (_, (_, ts)) => ts}) should equal (List(List(IntCheck(0), IntCheck(0))))
        // Check function stores correct return type.
        funcMap.map({case (_, (t, _)) => t}) should equal (List(IntCheck(0)))
        // Check symbol table for function has three key value pairs in the variable map.
        val children = analyser(p)._1.getChildren()
        children(0).getVariableMap().size should equal (3)
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with two function declarations")
    answer = result.parse("begin int f() is return 2 end int g() is int x = call f(); return x * 2 end int y = call g(); println y end")
    answer match {
      case Success(p) => {
        // Check function map has two key value pairs.
        var funcMap = analyser(p)._2.getFuncMap()
        funcMap.size should equal (2)
        // Check symbol table has two children.
        var children = analyser(p)._1.getChildren()
        children.size should equal (2)
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Functions" should "parse successfully and have correct return types" in
  {
    info("with two function declarations")
    var answer = result.parse("begin int f(int a) is return a + 2 end int g(int b) is return b * 2 end int y = call g(5); println y end")
    answer match {
      case Success(p) => {
        // Check function map has two key value pairs.
        var funcMap = analyser(p)._2.getFuncMap()
        funcMap.size should equal (2)
        // Check symbol table has two children.
        var children = analyser(p)._1.getChildren()
        children.size should equal (2)
        // Check Function has correct return type.
        funcMap.map({case (_, (t, _)) => t}) should equal (funcMap.flatMap({case (_, (_, ts)) => ts}))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with one function declaration")
    answer = result.parse("begin bool f(int a) is return a end skip end")
    answer match {
      case Success(p) => {
        var funcMap = analyser(p)._2.getFuncMap()
        // Check function declaration is incorrect and returns semantic error.
        var error = analyser(p)._3
        error should equal (List(("Expression does not match return type of function, expected bool but expression of type int found!",(1,24))))
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Functions" should "parse successfully and have correct number of arguments" in
  {
    info("with incorrect function call")
    var answer = result.parse("begin int f() is return 2 end int y = call f(5); println y end")
    answer match {
      case Success(p) => {
        var symbolTable = analyser(p)._1
        var error = analyser(p)._3
        // Check function has incorrect number of arguments.
        error should equal (List(("Wrong number of arguments in call to function f!",(1,39))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with correct function call")
    answer = result.parse("begin int f(int a) is return a end int y = call f(5) end")
    answer match {
      case Success(p) => {
        var funcMap = analyser(p)._2.getFuncMap()
        // Check function declaration is incorrect and returns semantic error.
        var error = analyser(p)._3
        error should equal (List())
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Relational operators" should "have matching types" in
  {
    info("with equality having mismatched types")
    var answer = result.parse("begin int i = 5; bool b = true; println i == b end")
    answer match {
      case Success(p) => {
        val variableMap = analyser(p)._1.getVariableMap()
        var lhsType = variableMap.filter({case (x, _) => x == "i"}).map({case (_, (t, _, _)) => t})
        var rhsType = variableMap.filter({case (x, _) => x == "b"}).map({case (_, (t, _, _)) => t})
        lhsType should equal (List(IntCheck(0)))
        rhsType should equal (List(BoolCheck(0)))
        val error = analyser(p)._3 
        // Check that == fails and returns semantic error
        error should equal (List(("Expressions in EQ(Ident(i),Ident(b)) have missmatched types: int and bool!",(1,43))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with equality having matching types")
    answer = result.parse("begin int i = 5; int j = 7; println i == j end")
    answer match {
      case Success(p) => {
        val variableMap = analyser(p)._1.getVariableMap()
        var lhsType = variableMap.filter({case (x, _) => x == "i"}).map({case (_, (t, _, _)) => t})
        var rhsType = variableMap.filter({case (x, _) => x == "j"}).map({case (_, (t, _, _)) => t})
        // Check that lhs and rhs have equal types
        lhsType should equal (rhsType)
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with greater than (>) having mismatched types")
    answer = result.parse("begin int i = 5; char c = 'a'; println i > c end")
    answer match {
      case Success(p) => {
        val variableMap = analyser(p)._1.getVariableMap()
        var lhsType = variableMap.filter({case (x, _) => x == "i"}).map({case (_, (t, _, _)) => t})
        var rhsType = variableMap.filter({case (x, _) => x == "c"}).map({case (_, (t, _, _)) => t})
        lhsType should equal (List(IntCheck(0)))
        rhsType should equal (List(CharCheck(0)))
        val error = analyser(p)._3
        // Check that > fails and returns semantic error
        error should equal (List(("Expressions in GT(Ident(i),Ident(c)) have missmatched types: int and char!",(1,42))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with greater than (>) having matching types")
    answer = result.parse("begin int i = 5; int j = 7; println i > j end")
    answer match {
      case Success(p) => {
        val variableMap = analyser(p)._1.getVariableMap()
        var lhsType = variableMap.filter({case (x, _) => x == "i"}).map({case (_, (t, _, _)) => t})
        var rhsType = variableMap.filter({case (x, _) => x == "j"}).map({case (_, (t, _, _)) => t})
        // Check that lhs and rhs have equal types
        lhsType should equal (rhsType)
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with greater than (>) having incorrect types")
    answer = result.parse("begin bool b = true; string s = \"hello\"; println b > s end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check that > should fail and return three semantic errors
        error.size should equal (3)
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Variables in programs" should "parse successfully and are defined before used" in
  {
    info("with undefined variables outside functions")
    var answer = result.parse("begin println y end")
    answer match {
      case Success(p) => {
        // Check function has incorrect number of arguments.
        var error = analyser(p)._3
        error should equal (List(("y undeclared: ",(1,15))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with undefined variables inside functions")
    answer = result.parse("begin int f() is return x end skip end")
    answer match {
      case Success(p) => {
        // Check function declaration is incorrect and returns semantic error.
        var error = analyser(p)._3
        error should equal (List(("x undeclared: ",(1,25))))
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Variable" should "declarations should have matching types" in
  {
    info("with int and char")
    var answer = result.parse("begin int a = 'c' end")
    answer match {
      case Success(p) => {
        // Check function has incorrect number of arguments.
        var error = analyser(p)._3
        error should equal (List(("Expression of type int expected in right hand side of assignment, but expression of type char found!",(1,15))))
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  def analyser(p: Node): (SymbolTable, FunctionTable, ListBuffer[(String, (Int, Int))]) = { 
    val symbolTable = new SymbolTable(ProgramSection(), None)
    val functionTable = new FunctionTable()
    analyse(p, symbolTable, functionTable, None)
    val e = errors
    errors = ListBuffer.empty
    (symbolTable, functionTable, e)
  }
}