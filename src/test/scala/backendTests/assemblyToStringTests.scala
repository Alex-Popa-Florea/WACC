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

class AssemblyToStringTest extends AnyFlatSpec with AppendedClues {
    info ("ASSEMBLY TO STRING TESTS")
  
    "ADD instruction" should "produce correct string" in
    {
        info("without cond or S flag")
        ADD(None, false, R(0), R(1), Immed(0)).toString() should equal ("    ADD r0, r1, #0\n")

        info("with cond")
        ADD(Some(EQCOND()), false, R(0), R(1), Immed(0)).toString() should equal ("    ADDEQ r0, r1, #0\n")

        info("with S flag")
        ADD(None, true, R(0), R(1), Immed(0)).toString() should equal ("    ADDS r0, r1, #0\n")

        info("with cond and S flag")
        ADD(Some(EQCOND()), true, R(0), R(1), Immed(0)).toString() should equal ("    ADDEQS r0, r1, #0\n")
    }

    "SUB instruction" should "produce correct string" in
    {
        info("without cond or S flag")
        SUB(None, false, R(0), R(1), Immed(0)).toString() should equal ("    SUB r0, r1, #0\n")

        info("with cond")
        SUB(Some(EQCOND()), false, R(0), R(1), Immed(0)).toString() should equal ("    SUBEQ r0, r1, #0\n")

        info("with S flag")
        SUB(None, true, R(0), R(1), Immed(0)).toString() should equal ("    SUBS r0, r1, #0\n")

        info("with cond and S flag")
        SUB(Some(EQCOND()), true, R(0), R(1), Immed(0)).toString() should equal ("    SUBEQS r0, r1, #0\n")
    }
    
    "B instruction" should "produce correct string" in
    {
        info("without cond")
        B(None, "malloc").toString() should equal ("    B malloc\n")

        info("with cond")
        B(Some(EQCOND()), "malloc").toString() should equal ("    BEQ malloc\n")
    }

    "LDR instruction" should "produce correct string" in
    {
        info("without cond")
        //println(LDR(None, R(0), ZeroOffset(R(0))))
        LDR(None, R(0), ZeroOffset(R(0))).toString() should equal ("    LDR r0, [r0]\n")

        info("with cond")
        LDR(Some(EQCOND()), R(0), ZeroOffset(R(0))).toString() should equal ("    LDREQ r0, [r0]\n")
    }



    "PUSH instruction" should "produce correct string" in
    {
        PUSH(List(R(0))).toString() should equal ("    PUSH {r0}\n")
    }

    "Message" should "produce correct string" in
    {
        Msg(0, 8, "overflow").toString() should equal (s"""|msg_0:
                                                           |    .word 8
                                                           |    .ascii    "overflow"
                                                           |""".stripMargin)
    }

    "Character" should "produce correct string" in
    {
        info("with normal character")
        Character('h').toString() should equal ("'h'")

        info("with escaped character")
        Character('\n').toString() should equal ("10")
    }
    
    "Shifts" should "produce correct string" in
    {
        info("with logical shift")
        LogicalShiftLeft(R(0), Immed(0)).toString() should equal ("r0, LSL #0")

        info("with arithmetic shift")
        ArithmeticShiftRight(R(0), Immed(0)).toString() should equal ("r0, ASR #0")
    }

    "Offsets" should "produce correct string" in
    {
        info("with zero offset")
        ZeroOffset(R(0)).toString() should equal ("[r0]")

        info("with immeadiate offset")
        ImmediateOffset(R(0), Immed(0)).toString() should equal ("[r0, #0]")

        info("with register write back")
        RegisterWriteBack(R(0), Immed(0)).toString() should equal ("[r0, #0]!")
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