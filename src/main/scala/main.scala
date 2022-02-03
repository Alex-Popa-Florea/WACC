package wacc

object main {
    import parser._
    import parsley.{Success, Failure}
    import scala.io.Source
    import ast._
  

    def main(args: Array[String]) = {
       assert(args.head != "")
        result.parse(Source.fromFile(args.head).getLines.toList.mkString("\n")) match {
            case Success(x) => {
                println(s"${args.head} = $x")
            }
            case Failure(err) => {
                println(err)
                sys.exit(200)
            }
        }
    }




}   