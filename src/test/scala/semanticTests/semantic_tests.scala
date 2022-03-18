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
import wacc.classTable._
import wacc.types._
import wacc.section._
import wacc.main.STANDARD_LIBRARY
import wacc.main.ARRAY_BOUNDS
import wacc.main.CLASSES
import wacc.arrayBounds._
import wacc.section._

import scala.collection.mutable.ListBuffer
import scala.io.Source

class SemanticTest extends AnyFlatSpec with AppendedClues{
  CLASSES = true
  info ("SEMANTIC TESTS")

  "Assignment statements" should "parse successfully and produce correct symbol table" in
  {
    info("with constant assignments")
    var answer = result.parse("begin int i = 2; bool b = true; char c = 'a'; string h = \"hello\" end")
    answer match {
      case Success(p) => {
        // Check variable map is generated correctly.
        analyser(p)._1.getVariableMap() should equal (Map("i" -> ((IntCheck(0), 4, ListBuffer((ProgramSection(),Unknown(),true)))), "b" -> ((BoolCheck(0), 5, ListBuffer((ProgramSection(),Unknown(),true)))), "c" -> ((CharCheck(0), 6, ListBuffer((ProgramSection(),Unknown(),true)))), "h" -> ((StrCheck(0), 10, ListBuffer((ProgramSection(), Unknown(), true))))))
        var funcMap = analyser(p)._2.getFuncMap()
        if (STANDARD_LIBRARY) {
          // Check function map contains only no new key value pair.
          funcMap.size should equal (preDefFunc.size)
        } else {
          // Check function map is empty.
          funcMap.size should equal (0)
        }
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
        if (STANDARD_LIBRARY) {
          // Check function map contains only one new key value pair.
          funcMap.size should equal (preDefFunc.size + 1)
        } else {
          // Check function map contains only one key value pair.
          funcMap.size should equal (1)
        }
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
        if (STANDARD_LIBRARY) {
          // Check function map contains only one new key value pair.
          funcMap.size should equal (preDefFunc.size + 1)
        } else {
          // Check function map contains only one key value pair.
          funcMap.size should equal (1)
        }
        // Check function f's paramater list has two elements.
        funcMap.withFilter({case (name, (_, ts)) => name == "f"}).
        map({case (_, (_, ts)) => ts}) should equal (List(List(IntCheck(0), IntCheck(0))))
        if (!STANDARD_LIBRARY) {
          // Check function stores correct return type.
          funcMap.map({case (_, (t, _)) => t}) should equal (List(IntCheck(0)))
        }
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
        var funcMap = analyser(p)._2.getFuncMap()
        if (STANDARD_LIBRARY) {
          // Check function map contains two new key value pairs.
          funcMap.size should equal (preDefFunc.size + 2)
        } else {
          // Check function map contains two key value pairs.
          funcMap.size should equal (2)
        }
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
        var funcMap = analyser(p)._2.getFuncMap()
        if (STANDARD_LIBRARY) {
          // Check function map contains two new key value pairs.
          funcMap.size should equal (preDefFunc.size + 2)
        } else {
          // Check function map contains two key value pairs.
          funcMap.size should equal (2)
        }        
        // Check symbol table has two children.
        var children = analyser(p)._1.getChildren()
        children.size should equal (2)
        if (!STANDARD_LIBRARY) {
          // Check Function has correct return type.
          funcMap.map({case (_, (t, _)) => t}) should equal (funcMap.flatMap({case (_, (_, ts)) => ts}))
        }
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
        error should equal (List(("Expressions in EQ(VarIdent(i),VarIdent(b)) have missmatched types: int and bool!",(1,43))))
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
        error should equal (List(("Expressions in GT(VarIdent(i),VarIdent(c)) have missmatched types: int and char!",(1,42))))
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
  "Predefined functions" should "parse successfully" in
  {
    info("with standard library flag")
    STANDARD_LIBRARY = true
    var answer = result.parse("begin char[] c = ['A']; bool b = call is_upper_string(123) end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (!STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is not used.
          error should equal (List(("Function is_upper_string not declared!",(1,34))))
        } else {
          // Check error is thrown when standard library flag is set.
          error should equal (List(("Expected argument types: List(char[]), but found: List(int)in call to function is_upper_string!",(1,34))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("and have correct name")
    answer = result.parse("begin char[] c = ['A']; bool b = call contains(c, 'A') end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is set.
          error should equal (List(("Function contains not declared!",(1,34))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("and have correct parameter types")
    answer = result.parse("begin char[] c = ['A']; bool b = call contains_char(c, 12) end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is set.
          error should equal (List(("Expected argument types: List(char[], char), but found: List(char[], int)in call to function contains_char!",(1,34))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("and have correct number of parameters")
    answer = result.parse("begin int a = 5; int b = call abs(a, a, a) end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is set.
          error should equal (List(("Wrong number of arguments in call to function abs!",(1,26))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("and cannot be redefined when standard library flag is set")
    answer = result.parse("begin bool max_int(int a, int b) is return true end skip end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is set.
          error should equal (List((("Function max_int has already been defined, sorry!",(1,7)))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("and can be redefined when standard library flag is not set")
    STANDARD_LIBRARY = false
    answer = result.parse("begin int abs(int a) is return true end skip end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        if (STANDARD_LIBRARY) {
          // Check error is thrown when standard library flag is set.
          error should equal (List(("Function abs has already been defined, sorry!",(1,7))))
        }
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  "Arrays" should "parse successfully" in
  {
    info("with non negative indices when -ab flag is set")
    ARRAY_BOUNDS = true
    var answer = result.parse("begin int[] a = [43, 2, 18, 1] ; int[] b = [1, 2, 3] ; println a[-2] end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check error is thrown when array bound check flag is set.
        error should equal (List(("Out of bounds error in a",(1,66))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    ARRAY_BOUNDS = false
  }
  "Class objects" should "parse successfully when -ci flag is set" in
  {
    info("with correct parameter types")
    var answer = result.parse("begin class Shape(int side) has ssalc class Shape square = newinstance Shape(true); skip end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check error is thrown when classses with inheritance flag is set.
        error should equal (ListBuffer(("Expected argument types: List(int), but found: List(bool)in call to function Shape!",(1,60))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with correct inheritance")
    answer = result.parse("begin class Shape(int side) extends Geometry has ssalc skip end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check error is thrown when classses with inheritance flag is set.
        error should equal (ListBuffer(("Parent class Geometry does not exist",(1,7))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("with correct number of parameters")
    answer = result.parse("begin class Shape(int side) has ssalc class Shape square = newinstance Shape(1, 2); skip end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check error is thrown when classses with inheritance flag is set.
        error should equal (ListBuffer(("Wrong number of arguments in constructor of Shape!",(1,60))))
      }
      case Failure(err) => {
        println(err)
      }
    }
    info("after declaration")
    answer = result.parse("begin class Human man = newinstance Human(\"Mr.Wacc\") end")
    answer match {
      case Success(p) => {
        var error = analyser(p)._3
        // Check error is thrown when classses with inheritance flag is set.
        error should equal (ListBuffer(("Undefined class Human",(1,13)), ("Class Human not declared!",(1,25))))
      }
      case Failure(err) => {
        println(err)
      }
    }
  }
  def analyser(p: Node): (SymbolTable, FunctionTable, ListBuffer[(String, (Int, Int))]) = { 
    val symbolTable = new SymbolTable(ProgramSection(), None)
    val functionTable = new FunctionTable(ProgramSection(), None)
    val classTable = new ClassTable()
    analyse(p, symbolTable, functionTable, classTable, None, false)
    val e = errors
    errors = ListBuffer.empty
    (symbolTable, functionTable, e)
  }
}