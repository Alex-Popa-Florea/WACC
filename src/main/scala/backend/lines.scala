package backend

object lines {
    trait Line

    case class Section(string: String) extends Line {
        override def toString() : String = {
            string
        }
    }
    
}
