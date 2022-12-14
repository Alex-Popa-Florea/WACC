package frontend

object error {
    import parsley.errors.ErrorBuilder
    import parsley.Result
    import scala.util._
    import edata._
    
    //Extends the Default parsely error builder 
    class StringErrorBuilder extends ErrorBuilder[String] {
        override def format(pos: Position, source: Source, lines: ErrorInfoLines): String = {
            eformat(Syntax,source,pos,lines)
        }

        type Position = String
        override def pos(line: Int, col: Int): Position = s"At line: ${line}, Column: ${col}"

        type Source = Option[String]
        override def source(sourceName: Option[String]): Source =  sourceName.map(name => s"File: $name")

        type ErrorInfoLines = String
        override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = 
            evanillaError(unexpected,expected,reasons,line)
        override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = 
            especialisedError(msgs,line)

        type ExpectedItems = Option[String]
        override def combineExpectedItems(alts: Set[Item]): ExpectedItems = Option(alts.toList.filter(_.nonEmpty).mkString(" , "))

        type Messages = List[Message]
        override def combineMessages(alts: Seq[Message]): Messages = alts.toList

        type UnexpectedLine = Option[String]
        override def unexpected(item: Option[Item]): UnexpectedLine = item.map("Unexpected " + _)
        type ExpectedLine = Option[String]
        override def expected(alts: ExpectedItems): ExpectedLine = alts.map("Expected: " + _)

        type Message = String
        override def reason(reason: String): Message = reason
        override def message(msg: String): Message = msg

        type LineInfo = String
        override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int): LineInfo = {
            def errorPointer(caretAt: Int) = s"${" " * caretAt}^"
            val errorLineStart = ">"
            val before = "" 
            val after  = ""
            linesBefore.foreach(line => before + s"$errorLineStart$line ")
            linesBefore.foreach(line => after  + s"$errorLineStart$line ")
            s"""
            |$before
            |$errorLineStart$line 
            |${" " * errorLineStart.length}${errorPointer(errorPointsAt)}
            |$after""".stripMargin
        }
        
        type Item = String
        type Raw = String
        type Named = String
        type EndOfInput = String
        override def raw(item: String): Raw = item
        override def named(item: String): Named = item
        override val endOfInput: EndOfInput = "end of input"

        override val numLinesBefore: Int = 0
        override val numLinesAfter: Int = 0
    }
}
