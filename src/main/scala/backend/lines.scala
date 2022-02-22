package backend

object lines {
    trait Line

    sealed trait Section extends Line

    case class Data() extends Section {
        override def toString() : String = {
            """|.data
               |
               |""".stripMargin
        }
    }

    case class Text() extends Section {
        override def toString() : String = {
            """|
               |.text
               |
               |.global main
               |""".stripMargin
        }
    }
    
}
