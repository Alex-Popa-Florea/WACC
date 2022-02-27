package backendTests

import wacc.symbolTable._
import wacc.functionTable._

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

import backend.codeGenerator.generate
import backend.codeGenerator.writeToFile
import backend.lines._
import backend.data._
import backend.instructions._
import backend.operators._
import frontend.color._
import frontend.edata._
import frontend.error.StringErrorBuilder
import frontend.parser._
import frontend.semanticAnalyser._
import parsley.Failure
import parsley.Success
import parsley.io.ParseFromIO

import java.io.File
import java.io.FileNotFoundException
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.io.Source

class AssemblyGenerationTest extends AnyFlatSpec with AppendedClues {
    info ("ASSEMBLY GENERATION TESTS")
  
    "Exit statements" should "produce correct assembly lines" in
    {
        val lines = fileToInstructions("./wacc_examples/basic/exit/exitBasic.wacc")
        lines should equal (List(Text(), Main(), PUSH(List(LR())), LDR(None, R(4), Immed(7)), MOV(None, false, R(0), R(4)), BL(None, "exit"), LDR(None, R(0), Immed(0)), POP(List(PC())), Ltorg()))
    }


    def fileToInstructions(fileName: String): List[Line] = {
        /*
            Create a new symbol table and function table
        */
        val symbolTable = new SymbolTable(ProgramSection(), None)
        val functionTable = new FunctionTable()

        /*
            Parse the input file    
        */
        val file = new File(fileName)
        val answer = result.parseFromFile(file)
        /*
            If syntax analysis passes, perform semantic analysis on the AST produced by the parser
        */
        answer.get match {
            case Success(x) => 
                val (semanticallyValid, hasReturnStatements) = analyse(x, symbolTable, functionTable, None)
                generate(x, symbolTable, functionTable)
            case _ => List.empty
        }
    }
}