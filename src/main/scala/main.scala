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
    import symbolTable._
    import functionTable._
    import edata._
    import parsley.io.ParseFromIO
    import java.io.{File,FileNotFoundException}
    import color._
    import error.StringErrorBuilder


    def main(args: Array[String]) = {
        assert(args.head != "")
        val symbolTable = new SymbolTable("Program", None)
        val functionTable = new FunctionTable()
        implicit val eb = new StringErrorBuilder
        val file = new File(args.head)
        val answer = result.parseFromFile(file)

        answer.recover{
            case _ : FileNotFoundException => println(makeRed(s"ERROR[FILE NOT FOUND]"))
            sys.exit()
        }

        answer.get match {
            case Success(x) => {
                println(s"${args.head} = $x")
                val semanticAnalysis = analyse(x, symbolTable, functionTable, None)
                if (!semanticAnalysis._2) {
                    errorGenerator(Syntax, Some(args.head), file, List(returnTypeError.get))
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
                        println(errors)
                        errorGenerator(Semantic, Some(args.head), file, errors)
                        println("exit:200")
                        sys.exit(200)
                    }
                }
            }
            case Failure(err) => {
                println(err)
                println("exit:100")
                sys.exit(100)
            }
        }
        println("exit:0")
    }
}