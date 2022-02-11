package wacc
object edata {
    import color._
    import java.io.File

    sealed trait ErrorType
    case object Syntax extends ErrorType
    case object Semantic extends ErrorType
 
    //IO
    //LineInfo

    def eformat(err: ErrorType,source: Option[String],pos: String, reasons: String): String  = 
        s"""${pos}
        |${source.getOrElse("")}
        |${makeRed(s"[${err} ERROR]")}
        |${reasons}""".stripMargin

    def errorGenerator(t: ErrorType, source: Option[String],file: File, errs: List[(String, (Int, Int))]): Unit = {
        val program = scala.io.Source.fromFile(file).getLines().toList
        def errorPointer(caretAt: Int) = s"${" " * caretAt}^"
        val errorLineStart = ">"
        for(e <- errs) {
            var line = e._2._1
            var col  = e._2._2
            var reason = s"""
                            |${e._1}
                            |$errorLineStart${program(line-1)}
                            |${" " * errorLineStart.length}${errorPointer(col)}""".stripMargin

            println(eformat(t,source, s"At line: $line, Column: $col", reason))
        }
    }

    def evanillaError(unexpected: Option[String], expected: Option[String], reasons: List[String], lines:String): String ={
        s"""
        |${unexpected.get} 
        |${expected.get} 
        |${reasons.mkString("\n")}
        |${lines}""".stripMargin
    }

    def especialisedError(msgs: List[String], lines:String): String ={
        s"""${msgs.mkString} 
        ${lines}""".stripMargin    
    }

}


