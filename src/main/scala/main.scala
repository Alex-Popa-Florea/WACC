package wacc

import java.io.FileNotFoundException

object main {
    import parser._
    import parsley.{Success, Failure}
    import parsley.io.ParseFromIO
    import scala.io.Source
    import ast._
    import types._
    import semanticAnalyser._
    import symbolTable._
    import wacc.symbolTable._
    import wacc.functionTable._
    import parsley.io.ParseFromIO
    import java.io.{File,FileNotFoundException}
    import color._
    import error.StringErrorBuilder


    def main(args: Array[String]) = {
       assert(args.head != "")
            var symbolTable = new SymbolTable("Program", None)
            var functionTable = new FunctionTable()
            implicit val eb = new StringErrorBuilder
            var answer = result.parseFromFile(new File(args.head))

            answer.recover{
                case _ : FileNotFoundException => println(makeRed(s"ERROR[FILE NOT FOUND]"))
                sys.exit()
            }

            answer.get match {
                case Success(x) => {
                    println(s"${args.head} = $x")
                    val semanticAnalysis = analyse(x, symbolTable, functionTable, None)
                    if (!semanticAnalysis._2) {
                        sys.exit(100)
                    } else {
                        if (semanticAnalysis._1) {
                            println("")
                            functionTable.printFunctionTables()
                            println("")
                            symbolTable.printSymbolTables(symbolTable, 0)
                            println("")
                            println(semanticAnalysis)
                        } else {
                            sys.exit(200)
                        }
                    }
                }
                case Failure(err) => {
                    println(err)
                    //sys.exit(100)
                }
            }
    }
}