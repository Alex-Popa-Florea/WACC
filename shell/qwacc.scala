package shell

import wacc.main._

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.sys.process._
import scala.util.control.Breaks._

import io.Source._

object shell{

    def insert(file: File, line: String) = {
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        bw.write(line)
        bw.write(" end")
        bw.close()
    }

    def getOutputAndCode(file: File, args: String):(String,Int) = {
        val output = new ByteArrayOutputStream
        val name  = file.getPath().replace(".wacc",".s")
        val exeName = s"./tmp/${file.getName().replace(".wacc","")}"
        s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $name".!
        val exitCode = (s"echo $args" #| s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName" #> output).!
        (output.toString("UTF-8"),exitCode)
    }

    def main(args: Array[String]) = {
        println("Ensure all statements end with a ;")

        var command = ""
        val dir = new File("./tmp/")
        dir.mkdir()
        val file = File.createTempFile("tmp",".wacc",dir)//maybe change to /tmp?
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        val validCommands = ListBuffer[String]()
        breakable{
            while (command != "exit"){
            
                command = readLine(">>> ")
                val p = file.getPath()
                if(command == "exit") break()
                if(validCommands.size == 0){
                    if (command.last != ';'){
                        println("MUST HAVE A SEMI COLON AT THE END")
                    }
                    else{
                        //insert(file,command)
                        val line = command.substring(0,command.length()-1)
                        insert(file,line)
                        wacc.main.main(Array(p,"tmp"))
                        val (output, exitCode) = getOutputAndCode(file,"")
                        if (exitCode == 0){
                            validCommands.addOne(output)
                            println(output)
                        }
                    }

                }
                else{
                    bw.write(command)
                    bw.close()
                    wacc.main.main(Array(p,"tmp"))
                }
                println(p)
            }
        }
        
        
        //val content = fromFile(file).mkString
        
        
        //delete tmp file and folder
    }
}