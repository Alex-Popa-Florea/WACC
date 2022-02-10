package wacc

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


    def main(args: Array[String]) = {
       assert(args.head != "")
            var symbolTable = new SymbolTable("Program", None)
            var functionTable = new FunctionTable()
            val answer = result.parse(args.head) //Source.fromFile(args.head).getLines.toList.mkString("\n")
            answer match {
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