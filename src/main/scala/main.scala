package wacc

import backend.codeGenerator.generate
import backend.codeGenerator.writeToFile
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

import ast._
import types._
import symbolTable._
import functionTable._
import wacc.classTable._
import section._

object main {

    /*
        Flags.
    */
    var STANDARD_LIBRARY = false
    var ARRAY_BOUNDS = false
    var HELP = false
    var CLASSES = false

    def main(args: Array[String]) = {
        
        /* 
            Assert that an input file has been provided for compilation
        */
        assert(args.head != "")
        val output = if (args.size >= 2 && (args(args.size - 1)(0) != '-')) {
            args(args.size - 1)
        } else {
            ""
        }

        val flags: Array[String] = args.slice(1, args.length)
        var i = 0
        while (i < flags.length) {
            flags(i) match {
                // Standard Library Flag
                case "-sl" => 
                    STANDARD_LIBRARY = true
                // Classes with inheritance Flag
                case "-ci" =>
                    CLASSES = true
                // Array Bounds Check Flag
                case "-ab" =>
                    ARRAY_BOUNDS = true
                // Help Flag
                case "-help" =>
                    HELP = true
                case _ => 
            }
            i += 1
        }

        /*
            Create a new symbol table and function table
        */
        val symbolTable = new SymbolTable(ProgramSection(), None)
        val functionTable = new FunctionTable(ProgramSection(), None)
        val classTable = new ClassTable()

        /* 
            Create error builder to be used to format syntax errors
        */
        implicit val eb = new StringErrorBuilder

        /*
            Print helpful information for the user in the terminal
            if -help flag is entered. 
        */
        if (HELP) {
            val sb = new StringBuilder("\n****************************************************************\n\n")
            sb.append("Thank you for choosing to use our WACC_03 compiler today!\n")
            sb.append("1. The syntax to run files is \"run <waccFilePath> <-flags>\"\n")
            sb.append("2. You are free to use the flags for help (-help), using standard\n")
            sb.append("   library functions (-sl), using classes with inheritance (-ci)\n")
            sb.append("   and checking array bounds (-ab).\n")
            sb.append("3. The standard library functions provided are\n   max_int(x: Int, y: Int), max_char(x: Char, y: Char),\n   min_int(x: Int, y: Int), min_char(x: Char, y: Char),\n")
            sb.append("   abs(x: Int), pow(x: Int), is_upper_string(char[] s), is_upper_char(char c),\n   is_lower_string(char[] s), is_lower_char(char c)\n")
            sb.append("   contains_int(int[] a, int n), contains_char(char[] s, char c)\n")
            sb.append("4. Classes can be defined using the following syntax:\n\n   class className({type varName}) has\n      {class statements eg. <scope type varName = value>}\n   ssalc\n\n")
            sb.append("5. A test suite has been provided which contains unit tests and\n")
            sb.append("   can be run using the syntax \"test <testFile>\".\n")
            sb.append("5. To recompile the code after making changes, use the command \"compile\".\n")
            sb.append("6. After running a file using our compiler, the resulting assembly code\n")
            sb.append("   is stored in the root file directory in this format \"waccFileName.s\"\n")
            sb.append("7. To exit the sbt environment use the command \"exit\".\n")
            sb.append("8. Lastly, we have an interactive shell QWACC for you to run your\n")
            sb.append("   desired wacc code.\n")
            sb.append("\nHappy Waccing! :D\n")
            sb.append("\n****************************************************************\n")
            println(sb.toString)
        }

        /*
            Parse the input file    
        */
        val file = new File(args.head)
        val answer = result.parseFromFile(file)

        answer.recover{
            case _ : FileNotFoundException => println(makeRed(s"ERROR[FILE NOT FOUND]"))
            sys.exit()
        }

        /*
            If syntax analysis passes, perform semantic analysis on the AST produced by the parser
        */
        answer.get match {
            case Success(x) => {
                val (semanticallyValid, hasReturnStatements) = analyse(x, symbolTable, functionTable, classTable, None, false)
                if (!hasReturnStatements) {
                    /*
                        Errors relating to missing return statements in functions are found 
                        in the semantic analyser but are actually syntax errors, so the exit code is 100
                    */
                    errorGenerator(Syntax, Some(args.head), file, ListBuffer(returnTypeError.get))
                    sys.exit(100)
                } else {
                    /*
                        If semantic analysis passes, print the AST, symbol table and function table
                    */
                    if (semanticallyValid) {
                        val lines = generate(x, symbolTable, functionTable, classTable)
                        if(output == ""){
                            writeToFile(lines, file.getName().replace(".wacc",".s"), false)
                        }
                        else{
                            writeToFile(lines, output+"/"+file.getName().replace(".wacc",".s"), true)
                        }
                        
                    } else {
                        /*
                        Otherwise, print the errors produced using the error generator
                        and exit with a status of 200. */
                        errorGenerator(Semantic, Some(args.head), file, errors)
                        sys.exit(200)
                    }
                }
            }
            /*
            If parsing fails, print the syntax error produced, and exit with a status of 100. */
            case Failure(err) => {
                println(err)
                sys.exit(100)
            }
        }
    } 
}