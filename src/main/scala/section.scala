package wacc

object section {
    sealed trait Section

    case class ProgramSection() extends Section {
        override def toString() : String = {
            "Program"
        }
    }

    case class FunctionSection(name: String) extends Section {
        override def toString() : String = {
            s"Function ${name}"
        }
    }

    case class TrueIfSection() extends Section {
        override def toString() : String = {
            "True branch of if statement"
        }
    }

    case class FalseIfSection() extends Section {
        override def toString() : String = {
            "False branch of if statement"
        }
    }

    case class WhileSection() extends Section {
        override def toString() : String = {
            "Statements inside while loop"
        }
    }

    case class NestedProgramSection() extends Section {
        override def toString() : String = {
            "Statements inside nested begin"
        }
    }

}
