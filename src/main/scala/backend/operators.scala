package backend

object operators {
    sealed trait Operand2

    case class Immed(kind: String, value: Int) extends Operand2 with A_mode2 {
        override def toString() : String = {
            value.toString()
        }
    }
    case class Character(value: Char) extends Operand2

    sealed trait A_mode2

    sealed trait Offset extends A_mode2
    case class OImmediateOffset(rn: Register, immed: Immed) extends Offset {
        override def toString() : String = {
            s"[${rn.toString()}, #${immed.toString()}]"
        }
    }
    case class ZeroOffset(rn: Register) extends A_mode2 {
        override def toString() : String = {
            s"[${rn.toString()}]"
        }
    }
    case class Label(value: String) extends A_mode2 {
        override def toString() : String = {
            value.toString()
        }
    }

    sealed trait PreIndexedOffset extends A_mode2
    case class PrImmediateOffset(rn: Register, immed: Immed) extends PreIndexedOffset

    sealed trait PostIndexedOffset extends A_mode2
    case class PoImmediateOffset(rn: Register, immed: Immed) extends PostIndexedOffset

    sealed trait Cond
    case class EQCOND() extends Cond {
        override def toString() : String = {
            s"EQ"
        }
    }
    case class NECOND() extends Cond {
        override def toString() : String = {
            s"NE"
        }
    }
    case class HSCSCOND() extends Cond {
        override def toString() : String = {
            s"HSCS"
        }
    }
    case class LOCCCOND() extends Cond {
        override def toString() : String = {
            s"LOCC"
        }
    }
    case class MICOND() extends Cond {
        override def toString() : String = {
            s"MI"
        }
    }
    case class PLCOND() extends Cond {
        override def toString() : String = {
            s"PL"
        }
    }
    case class VSCOND() extends Cond {
        override def toString() : String = {
            s"VS"
        }
    }
    case class VCCOND() extends Cond {
        override def toString() : String = {
            s"VC"
        }
    }
    case class HICOND() extends Cond {
        override def toString() : String = {
            s"HI"
        }
    }
    case class LSCOND() extends Cond {
        override def toString() : String = {
            s"LS"
        }
    }
    case class GECOND() extends Cond {
        override def toString() : String = {
            s"GE"
        }
    }
    case class LTCOND() extends Cond {
        override def toString() : String = {
            s"LT"
        }
    }
    case class GTCOND() extends Cond {
        override def toString() : String = {
            s"GE"
        }
    }
    case class LECOND() extends Cond {
        override def toString() : String = {
            s"LE"
        }
    }
    case class ALCOND() extends Cond {
        override def toString() : String = {
            s"AL"
        }
    }

    sealed trait Register extends Operand2
    case class R(value: Int) extends Register {
        override def toString() : String = {
            s"r${value}"
        }
    }
    case class LR() extends Register {
        override def toString() : String = {
            "lr"
        }
    }
    case class SP() extends Register {
        override def toString() : String = {
            "sp"
        }
    }
    case class PC() extends Register {
        override def toString() : String = {
            "pc"
        }
    }
}
