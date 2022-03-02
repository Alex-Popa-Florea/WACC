package backend

import lines._

object data {
    sealed trait Data extends Line
    case class Msg(id: Int, size: Int, text: String) extends Data {
        override def toString() : String = {
            var newText = text.replace("\u0000", raw"\0")
                              .replace("\b", raw"\b")
                              .replace("\t", raw"\t")
                              .replace("\n", raw"\n")
                              .replace("\f", raw"\f")
                              .replace("\r", raw"\r")
                              .replace("\"", """\"""")
                              .replace("\'", raw"\'")
            s"""|msg_${id}:
                |    .word ${size}
                |    .ascii    "${newText}"
                |""".stripMargin
        }
    }
}