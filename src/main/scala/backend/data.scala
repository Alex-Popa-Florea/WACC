package backend

import lines._

object data {

    /*
        Trait to represent elements within the data section of the assembly code
    */
    sealed trait Message extends Line

    /*
        Class that represents a message within the data section of the assembly code
    */
    case class Msg(id: Int, size: Int, text: String) extends Message {
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