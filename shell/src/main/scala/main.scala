package qwacc

import backend.codeGenerator.generate
import backend.codeGenerator.writeToFile
import frontend.edata._
import frontend.error
import frontend.error.StringErrorBuilder
import frontend.parser._
import frontend.semanticAnalyser._
import parsley.Failure
import parsley.Success
import parsley.io.ParseFromIO
import wacc.functionTable._
import wacc.main._
import wacc.section._
import wacc.symbolTable._

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.io.StdIn.readLine
import scala.sys.process._
import scala.util.control.Breaks._

import io.Source._

object shell{

    def insert(file: File, command: String, validCommands: ListBuffer[String],validFunctions: ListBuffer[String], function: Boolean) = {
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        val lines = validCommands.mkString("; ")
        val functions = validFunctions.mkString(" ")
        if (function){
            if(validCommands.size == 0){
                bw.write(command)
                bw.write(" skip")
            } else{
                bw.write(functions)
                bw.write(s" $command ")
                bw.write(lines)
            }
        } else{
            if(validCommands.size == 0){
                bw.write(command)
            } else{
                bw.write(functions)
                bw.write(s" $lines; ")
                bw.write(s"$command")
            }
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
    val types = Array("int", "char", "string", "pair", "bool")

    def addValidCommand(command: String, buffer: ListBuffer[String], input: String) = {
        val seperate = command.filterNot(_.isWhitespace)
        if(seperate.contains("=")){
        //Variables 
            var variable = seperate.substring(0,seperate.indexOf("="))
            if(varMap.keySet.contains(variable)){
                buffer.addOne(command)
            }
            
            if(seperate.startsWith("int")){
                variable = seperate.substring(3,seperate.indexOf("="))
                varMap.addOne(variable,"int")
                buffer.addOne(command)
            }

            variable = seperate.substring(4,seperate.indexOf("="))
            if(seperate.startsWith("bool")){
                varMap.addOne(variable,"bool")
                buffer.addOne(command)
            }
            if(seperate.startsWith("char")){
                varMap.addOne(variable,"char")
                buffer.addOne(command)
            }
            if(seperate.startsWith("pair")){
                var pairVariable = seperate.substring(seperate.indexOf(")")+1,seperate.indexOf("="))
                val pairTypes = seperate.substring(seperate.indexOf("(")-1,seperate.indexOf(")")).split(",")
                println(pairVariable)
                varMap.addOne(s"fst$pairVariable",pairTypes(0))
                varMap.addOne(s"snd$pairVariable",pairTypes(1))
                buffer.addOne(command)
            }
            if(seperate.startsWith("string")){
                variable = seperate.substring(5,seperate.indexOf("="))
                varMap.addOne(variable,"string"); buffer.addOne(command)
            }
        }else{
            if(seperate.substring(0,4) == "read"){
                val readVariable = seperate.substring(4)
                var pair = false
                if(readVariable.size > 3){
                    pair = readVariable.substring(0,3) == "fst" || readVariable.substring(0,3) == "snd"
                }
                //redo to check char thing for piar int if given char is 1
                var args = input
                (varMap(readVariable): @unchecked) match {
                    case "int" => {
                        if(args.forall(_.isDigit)){
                            
                            breakable{
                                while(true){
                                    args.toIntOption match {
                                        case None => println("Integer number too large, try again"); args = readLine()
                                        case Some(value) => args = value.toString(); break()
                                    }
                                }
                            }
                            if(pair){
                                val(fst,variable) = readVariable.splitAt(3)
                                buffer.addOne(s"$fst $variable = $args")
                            } else{
                                buffer.addOne(s"$readVariable = $args")
                            }
                        }
                        else{
                            args = "1"
                            if(pair){
                                val(fst,variable) = readVariable.splitAt(3)
                                buffer.addOne(s"$fst $variable = $args")
                            } else{
                                buffer.addOne(s"$readVariable = $args")
                            }
                        }
                    }
                    case "char" => {
                        args = args.charAt(0).toString()
                        if(pair){
                            val(fst,variable) = readVariable.splitAt(3)
                            buffer.addOne(s"$fst $variable = $args")
                        } else{
                            buffer.addOne(s"$readVariable = $args")
                        }                                
                    }
                }
            }
        }
    }

    def main(args: Array[String]) = {
        //intro msg here plz

        var command = ""
        val dir = new File("./tmp/")
        dir.mkdir()
        val file = File.createTempFile("tmp",".wacc",dir)//maybe change to /tmp?
        val path = file.getPath()
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        val validCommands = ListBuffer[String]()
        val validFunctions = ListBuffer[String]()

        breakable{
            while (command != "exit"){
                
                command = readLine(">>> ").trim().strip()
                if(command == ":ml"){
                    println("Muliline mode enabled use of if, while, function definitions or begin statements\n")
                    val lines = new ListBuffer[String]()
                    while(command != ""){
                        command = readLine("    | ").trim().strip()
                        lines.addOne(command)
                    }
                    println("EXITING setting")
                    command = lines.mkString(" ").strip()
                    println(command)
                }
                val function = types.exists(command.startsWith(_)) && command.endsWith("end") 
                println(s"IS FUCNCTION $function")

                if(command == ":exit") break()

                insert(file,command,validCommands,validFunctions,function)
                
                val res = result.parseFromFile(file)
                res.get match {
                    case Failure(exception) => println(exception)
                    case Success(value) => {
                        val symbolTable = new SymbolTable(ProgramSection(), None)
                        val functionTable = new FunctionTable() 
                        val (semanticallyValid, hasReturnStatements) = analyse(value, symbolTable, functionTable, None)
                        if (!hasReturnStatements) errorGenerator(Syntax, Some(path), file, ListBuffer(returnTypeError.get))
                        else{
                            if(semanticallyValid){
                                var args = ""
                                if (command.size > 4){
                                    if(command.substring(0,4) == "read"){
                                        args = readLine()
                                    }
                                }
                                if(function) validFunctions.addOne(command)  
                                else addValidCommand(command,validCommands,args)
                                val lines = generate(value, symbolTable, functionTable)
                                writeToFile(lines,"tmp/"+file.getName().replace(".wacc",".s"), true)
                                
                                val (output, exitCode) = getOutputAndCode(file,args)
                                if (output != "") println(output)
                            
                            } else{
                                errorGenerator(Semantic, Some(path), file, errors)
                            }
                        }
                    }
                }
                println(validCommands)
                println(path)
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