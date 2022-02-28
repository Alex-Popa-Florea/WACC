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
import symbolTable._
import functionTable._
import section._

object main {

    def main(args: Array[String]) = {
        /* 
            Assert that an input file has been provided for compilation
        */
        assert(args.head != "")
        var output = args(1)

        /*
            Create a new symbol table and function table
        */
        val symbolTable = new SymbolTable(ProgramSection(), None)
        val functionTable = new FunctionTable()

        /* 
            Create error builderto be used to format syntax errors
        */
        implicit val eb = new StringErrorBuilder

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
                println(s"${args.head} = $x")
                val (semanticallyValid, hasReturnStatements) = analyse(x, symbolTable, functionTable, None)
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
                        println("")
                        functionTable.printFunctionTables()
                        println("")
                        symbolTable.printSymbolTables(symbolTable, 0)
                        val lines = generate(x, symbolTable, functionTable)
                        if(output == ""){
                            writeToFile(lines, file.getName().replace(".wacc",".s"))
                        }
                        else{
                            writeToFile(lines, output++"/"++file.getName().replace(".wacc",".s"))
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