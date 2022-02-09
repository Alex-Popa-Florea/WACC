package wacc
object edata {
    sealed trait ErrorType
    case object Syntax extends ErrorType
    case object Semantic extends ErrorType
 
    //IO
    //LineInfo


    private def makeRed(msg:String):String = {
        Console.RED+msg+Console.RESET
    }

    def eformat(err:ErrorType,pos:String, lines:String): String  = 
s"""${pos}
${makeRed(s"[${err} ERROR]")}
${lines}
"""

    def evanillaError(unexpected: Option[String], expected: Option[String], reasons: List[String]): String ={
s"""${unexpected.get} 
${expected.get} 
${reasons.mkString("\n")}"""
    }

    def especialisedError(msgs: List[String]): String ={
        s"""${msgs.mkString}"""     
    }

}


