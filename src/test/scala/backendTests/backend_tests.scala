package backendTests
import scala.sys.process._
import java.io.File
import scala.collection.mutable.ListBuffer
import wacc.main._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.{Try, Success, Failure}
import java.io.ByteArrayOutputStream

class backend_tests{

    //DOTN FORGET TO REFACTOR
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
    
    val root = new File("./wacc_example")
    val programs = travel(root).map(i => i.toString()).toList

    "mkdir -p output".!
    programs.map(i => main(Array(i,"output")))

    val outputfolder = new File("./output")

    for (file <- outputfolder.listFiles()){
        println(file.toString())
        val exe_name = file.toString.replace(".s","")
        s"arm-linux-gnueabi-gcc -o $exe_name -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $file".!
        val output = new ByteArrayOutputStream
        val exit_code = (s"qemu-arm -L /usr/arm-linux-gnueabi/ $exe_name" #> output).!
        println((exit_code,output))
    }


    //purge
    //"rm -rf output".!
}