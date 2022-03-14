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
        if(validCommands.size == 1){
            bw.write(s"; $command")
        }
        else{
            bw.write(command)
        }
        bw.write(" end")
        bw.close()
    }

    def getOutputAndCode(file: File, args: Char):(String,Int) = {
        val output = new ByteArrayOutputStream
        val name  = file.getPath().replace(".wacc",".s")
        val exeName = s"./tmp/${file.getName().replace(".wacc","")}"
        s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $name".!
        val exitCode = (s"echo $args" #| s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName" #> output).!
        (output.toString("UTF-8"),exitCode)
    }

    val varMap: Map[String,String] = Map()

    def addValidCommand(command: String, buffer: ListBuffer[String], input: Char) = {
        val seperate = command.filterNot(_.isWhitespace)
        println(seperate.substring(0,3))
        //Variables 
        if (seperate.contains("=")){
            var variable = seperate.substring(3,seperate.indexOf("="))
            (seperate.substring(0,3):  @unchecked) match {
            case i@"int" => varMap.addOne(variable,i); buffer.addOne(command)
            case c@"chr" => varMap.addOne(variable,c); buffer.addOne(command)
            case _ => None
            }
        }
        println(seperate.substring(4).trim().strip())
        (seperate.substring(0,4):  @unchecked) match {
            case b@"bool" => varMap.addOne(seperate.substring(4,seperate.indexOf("=")),b); buffer.addOne(command)
            case r@"read" => {
                val read_variable = seperate.substring(4)
                if(varMap(read_variable) == "int" && input.isLetter){
                    buffer.addOne((s"$read_variable = 1;"))
                }else{
                    buffer.addOne((s"$read_variable = $input"))
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

                //TODO RESTRUCTURE FOR NEW SEMI COLON RULES, SHOULD LOOK CLEANER

                if(command == "exit") break()
                var args: Char = 'a'
                command = command.trim().strip()
                if(command.substring(0,4) == "read"){
                    args = readLine().charAt(0)
                }
                
                insert(file,command,validCommands)
                wacc.main.main(Array(p,"tmp"))
                if(new File(file.getPath().replace(".wacc",".s")).exists){
                    val (output, exitCode) = getOutputAndCode(file,args)
                    println(output)
                    addValidCommand(command,validCommands,args)
                }
                println(validCommands)
                println(p)
            }
        }
        
        
        //val content = fromFile(file).mkString
        
        
        //delete tmp file and folder
    }
}