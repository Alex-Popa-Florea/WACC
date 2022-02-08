package wacc

object main {
    import parser._
    import parsley.{Success, Failure}
    import parsley.io.ParseFromIO
    import scala.io.Source
    import ast._
    import java.io.File;

    def main(args: Array[String]) = {
       assert(args.head != "")
        result.parseFromFile(new File(args.head)).get match {
            case Success(x) => {
                println(s"${args.head} = $x")
            }
            case Failure(err) => {
                println(err)
                sys.exit(100)
            }
        }
    }




}   