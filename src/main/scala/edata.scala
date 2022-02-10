package wacc
object edata {
    import color._

    sealed trait ErrorType
    case object Syntax extends ErrorType
    case object Semantic extends ErrorType
 
    //IO
    //LineInfo

    def eformat(err:ErrorType,source: Option[String],pos:String, lines:String): String  = 
s"""${pos}
${source.getOrElse("")}
${makeRed(s"[${err} ERROR]")}
${lines}
"""

    def errorGenerator(t: ErrorType, errs: List[(String, (Int, Int))]): Unit = {
        for(e <- errs) {
            println(eformat(t,None, s"At line: ${e._2._1}, Column: ${e._2._2}", e._1))
        }
    }

    def evanillaError(unexpected: Option[String], expected: Option[String], reasons: List[String]): String ={
s"""${unexpected.get} 
${expected.get} 
${reasons.mkString("\n")}"""
    }

    def especialisedError(msgs: List[String]): String ={
        s"""${msgs.mkString}"""     
    }

}


