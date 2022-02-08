package wacc

object main {
    import parser._
    import parsley.{Success, Failure}
    import scala.io.Source
    import ast._
    import types._
    import semanticAnalyser._
    import symbolTable._


    def main(args: Array[String]) = {
       assert(args.head != "")
            var symbolTable = new SymbolTable(None)
            result.parse(Source.fromFile(args.head).getLines.toList.mkString("\n")) match {
                case Success(x) => {
                    println(s"${args.head} = $x")
                    val semanticAnalysis = analyse(x, symbolTable, None)
                    if (!semanticAnalysis._2) {
                        sys.exit(100)
                    } else {
                        if (semanticAnalysis._1) {
                            println(symbolTable.variableMap)
                        } else {
                            sys.exit(200)
                        }
                    }
                }
                case Failure(err) => {
                    println(err)
                    sys.exit(100)
                }
            }
    }
}