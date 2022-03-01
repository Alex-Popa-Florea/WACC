package backend

import lines._

object data {
    sealed trait Data extends Line
    case class Msg(id: Int, size: Int, text: String) extends Data {
        override def toString() : String = {
            println(text.replace("\"", """\""""))
            s"""|msg_${id}:
                |    .word ${size}
                |    .ascii    "${text.replace("\"", """\"""")}"
                |""".stripMargin
        }
    }
}