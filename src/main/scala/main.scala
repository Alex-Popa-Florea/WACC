package wacc
import parsley.Parsley, Parsley._
object main {
    import parser._
    import parsley.{Success, Failure}
    def main(args: Array[String]) = {
        result.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(err) => println(err)
        }
    }

}