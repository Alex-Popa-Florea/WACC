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
import wacc.classTable._
import wacc.types._
import wacc.section._
import wacc.main.ARRAY_BOUNDS

import backend.codeGenerator.generate
import backend.codeGenerator.writeToFile
import backend.codeGenerator.msg
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
import backend.codeGenerator.msg

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
        lines should equal (List(Text(), Main(), PUSH(List(LR())), LDR(None, R(4), Immed(7)), MOV(None, false, R(0), R(4)), BL(None, "exit"), 
                            LDR(None, R(0), Immed(0)), POP(List(PC())), Ltorg()))
    }

    "Print statements" should "produce correct assembly lines" in 
    {
        msg = 0
        val lines = fileToInstructions("./wacc_examples/IO/print/print.wacc")
        lines should equal (List(Data(), Msg(0, 13, "Hello World!\n"), Msg(1, 5, "%.*s\\0"), Text(), Main(), PUSH(List(LR())), LDR(None, R(4), Label("msg_0")), MOV(None, false, R(0), R(4)), BL(None, "p_print_string"), 
                            LDR(None, R(0), Immed(0)), POP(List(PC())), Ltorg(), P("print_string"), PUSH(List(LR())), 
                LDR(None, R(1), ZeroOffset(R(0))),
                ADD(None, false, R(2), R(0), Immed(4)),
                LDR(None, R(0), Label(s"msg_1")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))))
    }

    "Array accesses" should "not contain array bounds error checking with -ab flag" in
    {
        ARRAY_BOUNDS = true
        msg = 0
        val lines = fileToInstructions("./wacc_examples/array/arrayLookup.wacc")
        lines should equal (List(Data(), Msg(0, 3, "%d\\0"), Msg(1, 1, "\\0"), Text(), Main(), PUSH(List(LR())), SUB(None, false, SP(), SP(), Immed(4)), LDR(None, R(0), Immed(20)), BL(None, "malloc"), 
                MOV(None, false, R(4), R(0)), LDR(None, R(5), Immed(43)), STR(None, R(5), ImmediateOffset(R(4), Immed(4))), LDR(None, R(5), Immed(2)), STR(None, R(5), ImmediateOffset(R(4), Immed(8))),
                LDR(None, R(5), Immed(18)), STR(None, R(5), ImmediateOffset(R(4), Immed(12))), LDR(None, R(5), Immed(1)), STR(None, R(5), ImmediateOffset(R(4), Immed(16))), LDR(None, R(5), Immed(4)), 
                STR(None, R(5), ZeroOffset(R(4))), STR(None, R(4), ZeroOffset(SP())), ADD(None, false, R(4), SP(), Immed(0)), LDR(None, R(5), Immed(0)), LDR(None, R(4), ZeroOffset(R(4))), MOV(None, false, R(0), R(5)),
                MOV(None, false, R(1), R(4)), ADD(None, false, R(4), R(4), Immed(4)), ADD(None, false, R(4), R(4), LogicalShiftLeft(R(5), Immed(2))), LDR(None, R(4), ZeroOffset(R(4))), MOV(None, false, R(0), R(4)),
                BL(None, "p_print_int"), BL(None, "p_print_ln"), ADD(None, false, SP(), SP(), Immed(4)), LDR(None, R(0), Immed(0)), POP(List(PC())), Ltorg(), P("print_int"), PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)), LDR(None, R(0), Label(s"msg_0")), ADD(None, false, R(0), R(0), Immed(4)), BL(None, "printf"), MOV(None, false, R(0), Immed(0)), BL(None, "fflush"), POP(List(PC())),
                P("print_ln"), PUSH(List(LR())), LDR(None, R(0), Label(s"msg_1")), ADD(None, false, R(0), R(0), Immed(4)), BL(None, "puts"), MOV(None, false, R(0), Immed(0)), BL(None, "fflush"), POP(List(PC()))))
        ARRAY_BOUNDS = false
    }

    def fileToInstructions(fileName: String): List[Line] = {
        /*
            Create a new symbol table and function table
        */
        val symbolTable = new SymbolTable(ProgramSection(), None)
        val functionTable = new FunctionTable(ProgramSection(), None)
        val classTable = new ClassTable()

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
                val (semanticallyValid, hasReturnStatements) = analyse(x, symbolTable, functionTable, classTable, None, false)
                generate(x, symbolTable, functionTable, classTable)
            case _ => List.empty
        }
    }
}