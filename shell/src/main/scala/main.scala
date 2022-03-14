package qwacc

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
import scala.collection.mutable.Map

import io.Source._

object shell{

    def insert(file: File, command: String, validCommands: ListBuffer[String]) = {
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        val lines = validCommands.mkString("; ")
        bw.write(lines)
        if(validCommands.size == 0){
            bw.write(command)
        }
        else{
            bw.write(s"; $command")
        }
        bw.write(" end")
        bw.close()
    }

    def getOutputAndCode(file: File, args: String):(String,Int) = {
        val output = new ByteArrayOutputStream
        val assemblyfile  = file.getPath().replace(".wacc",".s")
        val exeName = s"./tmp/${file.getName().replace(".wacc","")}"
        s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $assemblyfile".!
        val exitCode = (s"echo $args" #| s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName" #> output).!
        s"rm $exeName $assemblyfile"
        (output.toString("UTF-8"),exitCode)
    }

    val varMap: Map[String,String] = Map()

    def addValidCommand(command: String, buffer: ListBuffer[String], input: String) = {
        val seperate = command.filterNot(_.isWhitespace)
        //Variables 
        if (seperate.contains("=")){
            var variable = seperate.substring(3,seperate.indexOf("="))
            val noVariable = variable == ""
            (seperate.substring(0,3):  @unchecked) match {
            case i@"int" if !noVariable => varMap.addOne(variable,i); buffer.addOne(command)
            case c@"chr" if !noVariable => varMap.addOne(variable,c); buffer.addOne(command)
            case _ => None
            }
        }
        (seperate.substring(0,4):  @unchecked) match {
            case b@"bool" => varMap.addOne(seperate.substring(4,seperate.indexOf("=")),b); buffer.addOne(command)
            case r@"read" => {
                val readVariable = seperate.substring(4)
                if(varMap(readVariable) == "int" && input.charAt(0).isLetter){
                    buffer.addOne((s"$readVariable = 1;"))
                }else{
                    buffer.addOne((s"$readVariable = $input"))
                }
            }
            case p@"print" => None
            case _ => None
        }

        //re-assignment of variables
        if (seperate.contains("=")){
            val variable_name = seperate.substring(0,seperate.indexOf("="))
            if (varMap.keySet.exists(_==variable_name)){
            buffer.addOne(command)
            }
        }

    }

    def main(args: Array[String]) = {

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
                val path = file.getPath()

                if(command == "exit") break()
                var args = ""
                command = command.trim().strip()
                if(command.substring(0,4) == "read"){
                    val input = readLine()
                }
                
                insert(file,command,validCommands)
                wacc.main.main(Array(path,"tmp"))
                if(new File(file.getPath().replace(".wacc",".s")).exists){
                    val (output, exitCode) = getOutputAndCode(file,args)
                    //println(output)
                    addValidCommand(command,validCommands,args)
                }
                // println(validCommands)
                // println(p)
            }
        }
        //delete tmp file and folder

        // if(!file.delete()){
        //     println("ERROR tmp file not deleted")
        // }
        // if(!dir.delete()){
        //     val exit = "rm -drf tmp/".!
        //     println("tmp Directory force deleted")
        // }
    }   
}