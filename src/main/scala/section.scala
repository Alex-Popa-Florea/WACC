package wacc

object section {
    /*
        Sealed trait to represent sections of the code for the symbol table
    */
    sealed trait Section

    /*
        Class to represent the program section
    */
    case class ProgramSection() extends Section {
        override def toString() : String = {
            "Program"
        }
    }
    /*
        Class to represent the section of a function
    */
    case class FunctionSection(name: String) extends Section {
        override def toString() : String = {
            s"Function ${name}"
        }
    }
    /*
        Class to represent the section of a true branch of an if statement
    */
    case class TrueIfSection() extends Section {
        override def toString() : String = {
            "True branch of if statement"
        }
    }
    /*
        Class to represent the section of a false branch of an if statement
    */
    case class FalseIfSection() extends Section {
        override def toString() : String = {
            "False branch of if statement"
        }
    }
    /*
        Class to represent the section of a while statement
    */
    case class WhileSection() extends Section {
        override def toString() : String = {
            "Statements inside while loop"
        }
    }
    /*
        Class to represent the section of a nested begin statement
    */
    case class NestedProgramSection() extends Section {
        override def toString() : String = {
            "Statements inside nested begin"
        }
    }

}
