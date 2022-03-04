package backend

object lines {
    
    /*
        Sealed trait to represent lines of assembly code.
    */
    trait Line

    /*
        Sealed trait to represent sections of the code of assembly code.
    */
    sealed trait AssemblySection extends Line

    /*
        Class to represent the data section of the code.
    */
    case class Data() extends AssemblySection {
        override def toString() : String = {
            """|.data
               |
               |""".stripMargin
        }
    }

    /*
        Class to represent the text section of the code.
    */
    case class Text() extends AssemblySection {
        override def toString() : String = {
            """|
               |.text
               |
               |.global main
               |""".stripMargin
        }
    }   
}
