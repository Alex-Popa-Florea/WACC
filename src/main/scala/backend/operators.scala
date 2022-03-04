package backend

object operators {
    /*
        Sealed trait to represant operands of certain instructions
    */
    sealed trait Operand2

    /*
        Class to represent immediate integer values
    */
    case class Immed(value: Int) extends Operand2 with A_mode2 {
        override def toString() : String = {
            value.toString()
        }
    }
    /*
        Class to represent immediate characters
    */
    case class Character(value: Char) extends Operand2 {
        override def toString() : String = {
            if (value == '\u0000') {
                "0"
            } else if(value == '\b') {
                "8"
            } else if(value == '\t') {
                "9"
            } else if(value == '\n') {
                "10"
            } else if(value == '\f') {
                "12"
            } else if(value == '\r') {
                "13"
            } else {
                "'" + value.toString() + "'"
            }
        }
    }

    /*
        Class to represent left logical shifts
    */
    case class LogicalShiftLeft(rm: Register, immed: Immed) extends Operand2 {
        override def toString() : String = {
            rm.toString() + ", LSL #" + immed.toString()
        }
    }
    /*
        Class to represent right arithmetic shifts
    */
    case class ArithmeticShiftRight(rm: Register, immed: Immed) extends Operand2 {
        override def toString() : String = {
            rm.toString() + ", ASR #" + immed.toString()
        }
    }

    /*
        Sealed trait to represant the addressing modes of certain instructions
    */
    sealed trait A_mode2

    /*
        Sealed trait to represant offsets of certain instructions
    */
    sealed trait Offset extends A_mode2
    /*
        Class to represent memory addresses from a register
    */
    case class ZeroOffset(rn: Register) extends A_mode2 {
        override def toString() : String = {
            s"[${rn.toString()}]"
        }
    }
    /*
        Class to represent memory addresses from a register with an immediate offset
    */
    case class ImmediateOffset(rn: Register, immed: Immed) extends Offset {
        override def toString() : String = {
            s"[${rn.toString()}, #${immed.toString()}]"
        }
    }
    /*
        Class to update the base register based on the calculated address of transfer
    */
    case class RegisterWriteBack(rn: Register, immed: Immed) extends Offset {
        override def toString() : String = {
            s"[${rn.toString()}, #${immed.toString()}]!"
        }
    } 
    /*
        Class to represent labels to jump to
    */
    case class Label(value: String) extends A_mode2 {
        override def toString() : String = {
            value.toString()
        }
    }

    /*
        Sealed trait to represent condition codes
    */
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
    case class HSCOND() extends Cond {
        override def toString() : String = {
            s"HS"
        }
    }
    case class CSCOND() extends Cond {
        override def toString() : String = {
            s"CS"
        }
    }
    case class LOCOND() extends Cond {
        override def toString() : String = {
            s"LO"
        }
    }
    case class CCCOND() extends Cond {
        override def toString() : String = {
            s"CC"
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
            s"GT"
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

    /*
        Sealed trait to represent registers
    */
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
