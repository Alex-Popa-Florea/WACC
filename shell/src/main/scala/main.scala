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
import wacc.arrayBounds
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
import java.io.OutputStream
import java.io.PrintStream
import java.lang.ProcessBuilder
import java.nio.charset.StandardCharsets
import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.io.StdIn
import scala.io.StdIn.readLine
import scala.sys.process._
import scala.sys.process.processInternal
import scala.util.control.Breaks._

import io.Source._
import multiStream._

object shell{
    /*
        Lists that contain valid functions and valid commands and the ouput stream that the data goes too.
    */
    val validCommands = ListBuffer[String]()
    val validFunctions = ListBuffer[String]()
    var out = new ByteArrayOutputStream()


    /*
        Inserting a command into a .wacc file given the function flag and a readVariable if the command is read.
    */
    def insert(file: File, command: String, function: Boolean, readVariable: String) = {
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
            } else{
                bw.write(functions)
                bw.write(s" $lines; ")
                bw.write(s"$command")
            }
            if(readVariable != ""){
                bw.write(s"; println $readVariable")
            }            
        }
        bw.write(" end")
        bw.close()
    }


    /*
        Emulate the .wacc file allowing the user to take input when necessary.
    */
    def emulate(file: File) = {
        val assemblyfile  = file.getPath().replace(".wacc",".s")
        val exeName = s"./tmp/${file.getName().replace(".wacc","")}"
        s"arm-linux-gnueabi-gcc -o $exeName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $assemblyfile".!
        val builder = Process(s"qemu-arm -L /usr/arm-linux-gnueabi/ $exeName")
        out = new ByteArrayOutputStream()
        val stream = new MultiStream(System.out,out)
        System.setOut(new PrintStream(stream))
        val io = BasicIO.standard(true)
        val proc = builder.run(io)
        while(proc.isAlive()){}
        s"rm -f $exeName $assemblyfile".! 
    }

    
    /*
        Gets the output of the file.
    */
    def getOutput(file: File):String = {
        emulate(file)
        out.toString("UTF-8").split("\n").last  
    }

    
    /*
        Helper function for removeJunk used to recurse
    */
    def recursiveRemove(c:String): String = {
        if (c.startsWith("if")) removeJunk(c)
        else if (c.startsWith("begin")) removeJunk(c)
        else if (c.startsWith("while")) removeJunk(c)
        else if(c.contains("read")) "skip"
        else if(c.contains("print")) "skip"
        else c
    }

    /*
        Give a command removes reads and prints while preserving other statements.  
    */    
    def removeJunk(command: String): String = {
        val IF   = command.startsWith("if")
        val BEGIN = command.startsWith("begin")
        val WHILE = command.startsWith("while")

        if (IF){
            println("enter if ")
            var Array(cond, rest) = command.split("then ")
            var Array(trueValue,falseValue) = rest.split(" else ")
            trueValue = trueValue.split(";").map{recursiveRemove}.mkString(";")
            falseValue = falseValue.replace("fi","").split(";").map{recursiveRemove}.mkString(";")
            rest = trueValue ++ " else " ++ falseValue ++ " fi"
            return cond ++ "then " ++rest
        }
        else if(WHILE){
            var Array(cond, rest) = command.split("do ")
            var body = rest.replace("done","").split(";").map{recursiveRemove}.mkString(";")
            rest = body ++ " done"
            return cond ++ "do " ++ rest
        }
        else if(BEGIN){
            val beginIndex = 4
            val endIndex = command.length()-3
            var body = command.substring(beginIndex,endIndex).split(";").map{recursiveRemove}.mkString(";")
            return "begin" ++ body ++ "end"
        }
        return ""
    }

    /*
        Add a valid command to the buffer.
        valid command - a command that can be used to preserve the state of the program.
    */    
    def addValidCommand(command:String, input:String, map: collection.immutable.Map[String, (wacc.types.TypeCheck, Int, ListBuffer[(Section, arrayBounds.ArraySize, Boolean)])]) = {
        if(command.contains("=") && !command.contains("print")){
            if(command.contains("call")){
                val variable = command.substring(0,command.indexOf("=")).trim().strip()
                val validComand = s"$variable = $input"
                validCommands.addOne(validComand)
            }else{
                validCommands.addOne(command)
            }
        }else{
            command.substring(0,4) match {
                case "read" =>{
                    val (_,variable) = command.splitAt(4)
                    var key = ""
                    if (variable.contains("[")){
                        key = variable.slice(0,variable.indexOf("["))
                    }else{
                        key = variable
                    }
                    var validCommand = ""
                    (map(key.strip()): @unchecked) match {
                        case (wacc.types.CharCheck(_),_,_) => validCommand = s"${variable} = '$input'"
                        case (wacc.types.IntCheck(_),_,_) => validCommand = s"${variable} = $input"
                    }
                    
                    validCommands.addOne(validCommand)
                }
                case _ => None
            }
        }
    }
    /*
        Used to update variables that were used within scopes like if statements
    */
    def updateVariables(file: File,command: String,map: collection.immutable.Map[String, (wacc.types.TypeCheck, Int, ListBuffer[(Section, arrayBounds.ArraySize, Boolean)])]) = {
        val variables = map.keys.toArray
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        val lines = validCommands.mkString("; ")
        val functions = validFunctions.mkString(" ")
        if(validCommands.size == 0){
            bw.write(functions)
            bw.write(s" $command")
        } else{
            bw.write(functions)
            bw.write(s" $lines; ")
            bw.write(s"$command")
        }
        
        val delmiter = "####################"
        bw.write(s"""; println "$delmiter" """)

        variables.map(v => {
            (map(v)) match {
                case (wacc.types.IntCheck(i),_,_) if i == 0  => {bw.write(s"""; print "$v = " """);bw.write(s"; println $v")}
                case (wacc.types.CharCheck(i),_,_) if i == 0 => {bw.write(s"""; print "$v = " """);bw.write(s"; println $v")}
                case (wacc.types.BoolCheck(i),_,_) if i == 0 => {bw.write(s"""; print "$v = " """);bw.write(s"; println $v")}
                case (wacc.types.StrCheck(i),_,_) if i == 0  => {bw.write(s"""; print "$v = " """);bw.write(s"; println $v")}
                case _ => validCommands.addOne(removeJunk(command))
            }
        })

        bw.write(" end")
        bw.close()

        val res = result.parseFromFile(file)
        res.get match {
            case _: Failure[_] => println("DIDNT PARSE")
            case Success(value) => {
                val symbolTable = new SymbolTable(ProgramSection(), None)
                val functionTable = new FunctionTable(ProgramSection(), None) 
                val classTable = new wacc.classTable.ClassTable() 
                val (semanticallyValid, hasReturnStatements) = analyse(value, symbolTable, functionTable, classTable, None, false)
                val lines = generate(value, symbolTable, functionTable, classTable)       
                writeToFile(lines,"tmp/"+file.getName().replace(".wacc",".s"), true)
            }
        }

        emulate(file)

        val outputStrings = out.toString("UTF-8").split("\n")
        val delimterIndex = outputStrings.reverse.indexOf(delmiter)
        val variableValues = outputStrings.slice(outputStrings.length-delimterIndex,outputStrings.length)
        variableValues.foreach(i => if (i != "") validCommands.addOne(i))
    }

    /*
        Processes the Command and identifies functions
    */
    def processCommand(file: File, command: String, function: Boolean) = {
        val path = file.getPath()
        var read = false
        var isFunctionCall = command.contains("call") && !command.contains("print") 
        val body = command.startsWith("if")||command.startsWith("begin")||command.startsWith("while")
        if (command.size > 4){
            val (com,variable) = command.splitAt(4)
            if( com.trim().strip() == "read" ){
                read = true
                insert(file,command,function,variable.trim().strip())
            }
            else if (isFunctionCall){
                val variable = command.substring(0,command.indexOf("=")).trim().strip().split(" ").last
                insert(file,command,function,variable)
            }
            else{
            insert(file,command,function,"") 
            }
        }else{
            insert(file,command,function,"")
        }
        
        val res = result.parseFromFile(file)
        res.get match {
            case Failure(exception) => println(exception)
            case Success(value) => {
                val symbolTable = new SymbolTable(ProgramSection(), None)
                val functionTable = new FunctionTable(ProgramSection(), None) 
                val classTable = new wacc.classTable.ClassTable() 
                val (semanticallyValid, hasReturnStatements) = analyse(value, symbolTable, functionTable, classTable, None, false)
                if (!hasReturnStatements) errorGenerator(Syntax, Some(path), file, ListBuffer(returnTypeError.get))
                else{
                    if(semanticallyValid){
                        val lines = generate(value, symbolTable, functionTable, classTable)
                        writeToFile(lines,"tmp/"+file.getName().replace(".wacc",".s"), true)
                        if(body){
                            updateVariables(file,command,symbolTable.getVariableMap())
                        }else{
                            val output = getOutput(file)
                            if (output.startsWith("ArrayIndexOutOfBoundsError") ){
                                println(output)
                            }

                            else if(read || isFunctionCall){
                                addValidCommand(command,output,symbolTable.getVariableMap())
                            }
                            else{
                                if(function) validFunctions.addOne(command)
                                else addValidCommand(command,"",symbolTable.getVariableMap())
                            }                            
                        }
                    } else{
                        errorGenerator(Semantic, Some(path), file, errors)
                    }
                }
            }
        }
        read = false
    }

    /*
        Checks that there isnt any chain commands
    */
    def noChain(command: String): Boolean = {
        val entryTerminatorPairs = Array(("if","fi"),("while","done"),("begin","end"))
        entryTerminatorPairs.foreach{
            case (entry,term) if(command.startsWith(entry) && command.endsWith(term)) =>{
                val firstEntry = term.length() //0
                val lastTerm = command.indexOf(term)
                val inside = command.substring(term.length(),command.length-term.length())
                val secondEntry = inside.indexOf(entry)
                val secondTerm = inside.indexOf(term)
                val noChain =  secondEntry < secondTerm || secondTerm == -1 || secondEntry == -1 //might bite you in the arse check it
                return noChain
            }
            case _ => None
        }
        if(command.contains("=")){
            return command.split(";").size == 1
        }
        else{
            return !command.contains(";")
        }
        true
    }   
    /*
        Main where it all starts
    */
    def main(args: Array[String]) = {

        println("""|Welcome to QWACC (quick WACC)! An interactive shell for the WACC language.
                   |when typing in statements semi colons aren't needed however within functions, if, while, nested begin bodies they are needed.
                   |Chain commands ie statements ; statments are not allowed.
                   |use :exit to quit the language and :ml to use multiline mode.
                   |""".stripMargin)

        var command = ""
        val dir = new File("./tmp/")
        dir.mkdir()
        val file = File.createTempFile("tmp",".wacc",dir)//maybe change to /tmp?
        var first = true
        var functionWithStatement = false
        val types = Array("int", "char", "string", "pair", "bool")
        breakable{
            while (command != "exit"){
                command = readLine("🦆🦆🦆 ").trim().strip()
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
                        }else{
                            println("NO CHAIN COMMANDS")
                        } 
                        function = false                       
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