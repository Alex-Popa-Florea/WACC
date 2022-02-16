package wacc

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

object edata {
    import color._
    import java.io.File

    sealed trait ErrorType
    case object Syntax extends ErrorType
    case object Semantic extends ErrorType

    /*
        Used to format error message with the type of error, the name of the file, positions and r
    */
    def eformat(err: ErrorType,source: Option[String],pos: String, reasons: String): String  = 
        s"""${pos}
        |${source.getOrElse("")}
        |${makeRed(s"[${err} ERROR]")}
        |${reasons}""".stripMargin
        
    /*
        Used for generating a list of errors
    */
    def errorGenerator(t: ErrorType, source: Option[String],file: File, errs: ListBuffer[(String, (Int, Int))]): Unit = {
        val program = scala.io.Source.fromFile(file).getLines().toList
        def errorPointer(caretAt: Int) = s"${" " * caretAt}^"
        val errorLineStart = ">"
        for(e <- errs) {
            var line = e._2._1
            var col  = e._2._2
            var reason = s"""
                            |${e._1}
                            |$errorLineStart${program(line-1)}
                            |${" " * errorLineStart.length}${errorPointer(col-1)}""".stripMargin

            println(eformat(t,source, s"At line: $line, Column: $col", reason))
        }
    }

    /*
        Used by the error builder to make generic errors
    */
    def evanillaError(unexpected: Option[String], expected: Option[String], reasons: List[String], lines:String): String =
        s"""
        |${unexpected.get} 
        |${expected.get} 
        |${reasons.mkString("\n")}
        |${lines}""".stripMargin

    /*
        Used by the error builder to make specfic errors
    */
    def especialisedError(msgs: List[String], lines:String): String =
        s"""|
        |${msgs.mkString} 
        |${lines}""".stripMargin

}


