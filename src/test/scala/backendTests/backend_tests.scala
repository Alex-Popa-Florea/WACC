package backendTests
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
            if(file.isDirectory() && file.toString() != "./wacc_example/advanced" ){
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
        val contents = fromFile(file).getLines().toArray
        var outputArray = contents.slice(1,contents.size).takeWhile(i => i != "===========================================================").mkString("\n")
        if (outputArray != ""){
            outputArray++="\n"
        }
        val exitCode = contents(contents.size-2).split(" ").last.replace(".","").toInt
        (exitCode,outputArray)
    }

    
    val root = new File("./wacc_example")
    val programs = travel(root).map(i => i.toString()).toList

    "mkdir -p output".!
    programs.map(i => main(Array(i,"output")))

    val outputFolder = new File("./output")

    //Compare the output and the expected output of the .s files

    for (file <- outputFolder.listFiles()){
        file.toString() should "return correct exit code and output" in {pending}
        // {
        //     val exeName = file.toString.replace(".s","")
        //     s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $file".!
        //     val output = new ByteArrayOutputStream
        //     val exitCode = (s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName" #> output).!
        //     val (expectedExit,expectedOuput) = getOutput(new File(s"./refCompileOutput/${file.getName.replace(".s",".output")}"))
        //     output.toString("UTF-8") should equal (expectedOuput)
        //     exitCode should equal (expectedExit)
        // }
    }


    //purge all the .s files
    "rm -rf output".!
}