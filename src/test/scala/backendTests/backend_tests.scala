package backend_tests
import frontend.color._
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.main._

import java.io.ByteArrayOutputStream
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import io.Source._

class backend_tests extends AnyFlatSpec{
    //DOTN FORGET TO REFACTOR
    //traversing through and getting all .wacc files
    def travel(root: File):ListBuffer[File] = {
        var files: ListBuffer[File] = ListBuffer.empty
        for (file:File <- root.listFiles()){
            if(file.isDirectory() && file.getName != "advanced" ){
                files ++= travel(file)
            }
            else if(file.isFile()){
                files.append(file)
            }
        }
        files
    }

    //getting output and formatting it correctly
    def getOutput(file: File): (Int,String) = {
        import scala.util.matching.Regex

        val contents = fromFile(file).mkString
        val guard = "==========================================================="
        val outputpattern = s"(?s)(?<=$guard\n).*?(?=$guard)".r
        val exitpattern = "(?s)(?<=The exit code is ).*?(?=.\n-- Finished)".r
        val output = outputpattern findFirstIn contents match {
            case Some(value) => value.toString()
            case None => "no value"
        }
        val exitCode = exitpattern findFirstMatchIn contents match {
            case Some(value) => value.toString().toInt
            case None => 9999
        }
        (exitCode,output)
    }

    def checker(dir: File): Unit = {
        for(file <- dir.listFiles()){
            if (file.isFile()){
                info("Testing "+file.toString())
                val exePath = file.toString.replace(".s","")
                val fileName = file.getName().replace(".s","")
                s"arm-linux-gnueabi-gcc -o $exePath -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $file".!
                val output = new ByteArrayOutputStream
                val args =
                if(argsMap.contains(fileName)){
                    argsMap(fileName)
                }else{
                    ""
                }
                val exitCode = (s"echo $args" #| s"qemu-arm -L /usr/arm-linux-gnueabi/ $exePath" #> output).!
                val (expectedExit,expectedOuput) = getOutput(new File("./refCompileOutput/"+file.getName.replace(".s",".output")))
                output.toString("UTF-8") should equal (expectedOuput)
                exitCode should equal (expectedExit)
            }
        }
    }
    val argsMap: Map[String,String] = Map(
    ("echoBigInt","2000"),("echoBigNegInt","-2000"),("echoChar","c"),("echoInt","10"),
    ("echoNegInt","-10"),("echoPuncChar",","),("fibonacciFullIt","10"),("fibonacciFullRec","10"),
    ("IOLoop","1 Y 2 N"),("IOSequence","1"),("printInputTriangle","2"),("read","1"),("readPair","b 2"),("rmStyleAddIO","1"))

    val root = new File("./wacc_examples")
    val programs = travel(root).toList
    "rm -rf output".!
    "mkdir -p output".!
    programs.map(i => main(Array(i.toString(),"output"+i.toString()
                                                        .replace("./wacc_examples","")
                                                        .replace("/"+i.getName(),"")
                                                        )))

    "Array" should "give correct out put and error code" in (pending)
    // {
    //     info("Checking array")
    //     checker(new File("./output/array"))
    // }
    "Basic" should "give correct out put and error code" in {
        info("Checking basic")
        checker(new File("./output/basic"))
    }
    "Exit" should "give correct out put and error code" in {
        info("Checking exit")
        checker(new File("./output/basic/exit"))
    }
    "Skip" should "give correct out put and error code" in {
        info("Checking basic")
        checker(new File("./output/basic/skip"))
    }

    "Expressions" should "give correct out put and error code" in //(pending)
    {
        info("Checking expressions")
        checker(new File("./output/expressions"))
    }
    "Nested_functions" should "give correct out put and error code" in //(pending)
    {
        info("Checking nested_functions")
        checker(new File("./output/function/nested_functions"))
    }
    "Simple_functions" should "give correct out put and error code" in (pending)
    // {
    //     info("Checking simple_functions")
    //     checker(new File("./output/function/simple_functions"))
    // }
    "If" should "give correct out put and error code" in {
        info("Checking if")
        checker(new File("./output/if"))
    }
    "IO" should "give correct out put and error code" in {
        info("Checking IO")
        checker(new File("./output/IO"))
    }
    "Print" should "give correct out put and error code" in {
        info("Checking print")
        checker(new File("./output/IO/print"))
    }
    "Read" should "give correct out put and error code" in {
        info("Checking read")
        checker(new File("./output/IO/read"))
    }
    "Pairs" should "give correct out put and error code" in (pending)
    // {
    //     info("Checking pairs")
    //     checker(new File("./output/pairs"))
    // }
    "ArrayOutOfBounds" should "give correct out put and error code" in {
        info("Checking arrayOutOfBounds")
        checker(new File("./output/runtimeErr/arrayOutOfBounds"))
    }
    "DivideByZero" should "give correct out put and error code" in {
        info("Checking divideByZero")
        checker(new File("./output/runtimeErr/divideByZero"))
    }
    "IntegerOverflow" should "give correct out put and error code" in //(pending)
    {
        info("Checking integerOverflow")
        checker(new File("./output/runtimeErr/integerOverflow"))
    }
    "NullDereference" should "give correct out put and error code" in {
        info("Checking nullDereference")
        checker(new File("./output/runtimeErr/nullDereference"))
    }
    "Scope" should "give correct out put and error code" in (pending)
    // {
    //     info("Checking scope")
    //     checker(new File("./output/scope"))
    // }
    "Sequence" should "give correct out put and error code" in {
        info("Checking sequence")
        checker(new File("./output/sequence"))
    }
    "Variables" should "give correct out put and error code" in {
        info("Checking variables")
        checker(new File("./output/variables"))
    }
    "While" should "give correct out put and error code" in {
        info("Checking while")
        checker(new File("./output/while"))
    }
}