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
import java.lang.ProcessBuilder

import io.Source._
import java.io.OutputStream
import scala.sys.process.processInternal
import scala.collection.immutable

import multiStream._
import java.io.PrintStream
import qwacc.multiStream.MultiStream
import scala.io.StdIn
import java.nio.charset.StandardCharsets

object shell{

    val validCommands = ListBuffer[String]()
    val validFunctions = ListBuffer[String]()

    def insert(file: File, command: String, validCommands: ListBuffer[String],validFunctions: ListBuffer[String], function: Boolean, read: String) = {
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        val lines = validCommands.mkString("; ")
        val functions = validFunctions.mkString(" ")
        if (function){
            if(validCommands.size == 0){
                bw.write(functions)
                bw.write(s" $command")
                bw.write(" skip")
            } else{
                bw.write(functions)
                bw.write(s" $command ")
                bw.write(lines)
            }
        } else{
            if(validCommands.size == 0){
                bw.write(functions)
                bw.write(s" $command")
                if(read != ""){
                    bw.write(s"; println $read")
                }
            } else{
                bw.write(functions)
                bw.write(s" $lines; ")
                bw.write(s"$command")
                if(read != ""){
                    bw.write(s"; println $read")
                }
            }
        }
        bw.write(" end")
        bw.close()
    }

    def getOutputAndCode(file: File):String = {
        var output = ""
        var out = new ByteArrayOutputStream
        val assemblyfile  = file.getPath().replace(".wacc",".s")
        val exeName = s"./tmp/${file.getName().replace(".wacc","")}"
        s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $assemblyfile".!
        val stream = new MultiStream(System.out,out)
        System.setOut(new PrintStream(stream))
        val io = BasicIO.standard(true)
        val proc = Process(s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName").run(io)
        while(proc.isAlive()){}
        output = out.toString("UTF-8").split("\n").last
        System.setOut(System.out)
        s"rm tmp/$exeName tmp/$assemblyfile"        
        output
    }

    def addValidCommand(command:String, buffer:ListBuffer[String], input:String ) = {
        if(command.contains("=") && !command.contains("print")){
            if(command.contains("call")){
                val variable = command.substring(0,command.indexOf("=")).trim().strip()
                val validComand = s"$variable = $input"
                validCommands.addOne(validComand)
            }else{
                buffer.addOne(command)

            }
        }else{
            if(command.startsWith("if")){
                None
            }else{
                command.substring(0,4) match {
                    case "read" =>{
                        val (_,variable) = command.splitAt(4)
                        val validComand = s"${variable.strip()} = $input"
                        validCommands.addOne(validComand)
                    }
                    case _ => None
                }
            }
        }
    }

    val types = Array("int", "char", "string", "pair", "bool")
    def processCommand(file: File, command: String, function: Boolean) = {
        val path = file.getPath()
        var read = false
        var isFunctionCall = command.contains("call") && !command.contains("print") 
        // println(command)
        if (command.size > 4){
            val (com,variable) = command.splitAt(4)
            if( com.trim().strip() == "read" ){
                read = true
                // println("THINKS ITS READ")
                insert(file,command,validCommands,validFunctions,function,variable.trim().strip())
            }
            else if (isFunctionCall){
                val variable = command.substring(command.indexOf(" "),command.indexOf("=")).trim().strip()
                // println(variable)
                insert(file,command,validCommands,validFunctions,function,variable)
            }
            else{
            insert(file,command,validCommands,validFunctions,function,"") 
            }
        }else{
            insert(file,command,validCommands,validFunctions,function,"")
        }
        
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
                        val lines = generate(value, symbolTable, functionTable)
                        writeToFile(lines,"tmp/"+file.getName().replace(".wacc",".s"), true)
                        //println(isFunctionCall)
                        val output = getOutputAndCode(file)
                        if (output.startsWith("ArrayIndexOutOfBoundsError") ){
                            println(output)
                        }
                        else if(read || isFunctionCall){
                            addValidCommand(command,validCommands,output)
                        }
                        else{
                            if(function) validFunctions.addOne(command)
                            else addValidCommand(command,validCommands,"")
                        }
                    
                    } else{
                        errorGenerator(Semantic, Some(path), file, errors)
                    }
                }
            }
        }
        read = false
        // println(validCommands)
        // println(validFunctions)
        //println(path)   
    }
    def noChain(command: String): Boolean = {
        val entryTerminatorPairs = Array(("if","fi"),("while","done"),("begin","end"))
        entryTerminatorPairs.foreach{
            case (entry,term) if(command.startsWith(entry) && command.endsWith(term)) =>{
                println("entry " ++entry)
                val firstEntry = term.length() //0
                val lastTerm = command.indexOf(term)
                val inside = command.substring(term.length(),command.length-term.length())
                println(inside)
                val secondEntry = inside.indexOf(entry)
                val secondTerm = inside.indexOf(term)

                println("sec " ++ secondEntry.toString ++ "sec term" ++ secondTerm.toString())
                val noChain =  secondEntry < secondTerm || secondTerm == -1 || secondEntry == -1 //might bite you in the arse check it
                return noChain
            }
            case _ => None
    }
        if(command.contains("=")){
            // println("tried split")
            return command.split(";").size == 1
        }
        else{
            // println("goto contain")
            return !command.contains(";")
        }
        // println("got to end")
        true
    }   

    def main(args: Array[String]) = {
        //intro msg here plz

        var command = ""
        val dir = new File("./tmp/")
        dir.mkdir()
        val file = File.createTempFile("tmp",".wacc",dir)//maybe change to /tmp?
        var first = true
        var functionWithStatement = false
        breakable{
            while (command != "exit"){
                command = readLine(">>> ").trim().strip()
                if(command != ""){
                    if(command == ":ml"){
                        println("Muliline mode enabled use for if, while, function definitions or begin statements\n")
                        val lines = new ListBuffer[String]()
                        while(command != ""){
                            command = readLine("    | ").trim().strip()
                            lines.addOne(command)
                        }
                        command = lines.mkString(" ").strip()
                    }
                    if(command == ":exit") break()
                    if (first){
                        functionWithStatement = types.exists(command.startsWith(_)) && command.contains("is") && !command.endsWith("end") && !command.contains("print")
                        if(functionWithStatement){
                            println("Fuctions must be defined on their own with other statements")
                        }
                    }
                    
                    if(!functionWithStatement){
                        var function = types.exists(command.startsWith(_)) && command.endsWith("end")
                        if(noChain(command) || function){
                            processCommand(file,command.trim().strip(),function)
                            if(validCommands.size == 1 || validFunctions == 1){
                                first = false
                            }
                            function = false
                        }else{
                            println("NO CHAIN COMMANDS")
                        }                        
                    }
                } 
                Thread.sleep(10)
            }   
        }
        //delete tmp file and folder

        if(!file.delete()){
            println("ERROR tmp file not deleted")
        }
    }   
}