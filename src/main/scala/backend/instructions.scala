package backend

import backend.operators._
import lines._

object instructions {
    sealed trait Instruction extends Line
    
    sealed trait Scope extends Instruction
    case class Main() extends Scope {
        override def toString() : String = {
            "main:\n"
        }
    }
    
    case class P(string: String) extends Scope {
        override def toString() : String = {
            "p_" + string + ":\n"
        }
    }
    
    case class F(string: String) extends Scope {
        override def toString() : String = {
            "f_" + string + ":\n"
        }
    }
    
    case class L(id: Int) extends Scope {
        override def toString() : String = {
            s"""L${id}:\n"""
        }
    }

    case class PrintString(string: String) extends Scope {
        override def toString() : String = {
            string
        }
    }

    case class Ltorg() extends Instruction {
        override def toString() : String = {
            "    .ltorg\n"
        }
    }
    
    sealed trait Arithmetic extends Instruction
    case class ADD(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Arithmetic {
        override def toString() : String = {
            var addString = s"    ADD${cond.getOrElse("").toString()}"
            if (s) {
                addString += "S"
            }
            addString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(kind, value) => addString += "#"
                case Character(value) => addString += "#"
                case _ =>
            }
            addString += s"${operand2.toString()}\n"
            addString
        }
    }
    case class SUB(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Arithmetic {
        override def toString() : String = {
            var subString = s"    SUB${cond.getOrElse("").toString()}"
            if (s) {
                subString += "S"
            }
            subString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(kind, value) => subString += "#"
                case Character(value) => subString += "#"
                case _ =>
            }
            subString += s"${operand2.toString()}\n"
            subString
        }
    }
    case class SMULL(cond: Option[Cond], s: Boolean, rdlo: Register, rdhi: Register, rm: Register, rs: Register) extends Arithmetic {
        override def toString() : String = {
            var smullString = s"    SMULL${cond.getOrElse("").toString()}"
            if (s) {
                smullString += "S"
            }
            smullString += s" ${rdlo.toString()}, ${rdhi.toString()}, ${rm.toString()}, ${rs.toString()}\n"
            smullString
        }
    }
    
    sealed trait Compare extends Instruction
    case class CMP(cond: Option[Cond], rn: Register, operand2: Operand2) extends Compare {
        override def toString() : String = {
            s"    CMP${cond.getOrElse("").toString()} ${rn.toString()}, ${operand2.toString()}\n"
        }
    }
    case class CMN(cond: Option[Cond], rn: Register, operand2: Operand2) extends Compare {
        override def toString() : String = {
            s"    CMN${cond.getOrElse("").toString()} ${rn.toString()}, ${operand2.toString()}\n"
        }
    }

    sealed trait Logical extends Instruction
    case class MOV(cond: Option[Cond], s: Boolean, rd: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var movString = s"    MOV${cond.getOrElse("").toString()}"
            if (s) {
                movString += "S"
            }
            movString += s" ${rd.toString()}, "
            operand2 match {
                case Immed(kind, value) => movString += "#"
                case Character(value) => movString += "#"
                case _ =>
            }
            movString += s"${operand2.toString()}\n"
            movString
        }
    }
    case class AND(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var andString = s"    AND${cond.getOrElse("").toString()}"
            if (s) {
                andString += "S"
            }
            andString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(kind, value) => andString += "#"
                case Character(value) => andString += "#"
                case _ =>
            }
            andString += s"${operand2.toString()}\n"
            andString
        }
    }
    case class EOR(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var eorString = s"    EOR${cond.getOrElse("").toString()}"
            if (s) {
                eorString += "S"
            }
            eorString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(kind, value) => eorString += "#"
                case Character(value) => eorString += "#"
                case _ =>
            }
            eorString += s"${operand2.toString()}\n"
            eorString
        }
    }
    case class ORR(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var orrString = s"    ORR${cond.getOrElse("").toString()}"
            if (s) {
                orrString += "S"
            }
            orrString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(kind, value) => orrString += "#"
                case Character(value) => orrString += "#"
                case _ =>
            }
            orrString += s"${operand2.toString()}\n"
            orrString
        }
    }

    sealed trait Branch extends Instruction
    case class B(cond: Option[Cond], label: String) extends Branch {
        override def toString() : String = {
            s"    B${cond.getOrElse("").toString()} ${label}\n"
        }
    }
    case class BL(cond: Option[Cond], label: String) extends Branch {
        override def toString() : String = {
            s"    BL${cond.getOrElse("").toString()} ${label}\n"
        }
    }

    sealed trait Load extends Instruction
    case class LDR(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Load {
        override def toString() : String = {
            var ldrString = s"    LDR${cond.getOrElse("").toString()} ${rd.toString()}, "
            a_mode2 match {
                case OImmediateOffset(rn, immed) => ldrString += s"${a_mode2.toString()}\n"
                case ZeroOffset(rn) => ldrString += s"${a_mode2.toString()}\n"
                case Immed(kind, value) => ldrString += s"=${a_mode2.toString()}\n"
                case Label(value) => ldrString += s"=${a_mode2.toString()}\n"
                case _ => ldrString += s"#${a_mode2.toString()}\n"
            }
            ldrString
        }
    }
    case class LDRB(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Load {
        override def toString() : String = {
            var ldrString = s"    LDR${cond.getOrElse("").toString()}B ${rd.toString()}, "
            a_mode2 match {
                case OImmediateOffset(rn, immed) => ldrString += s"${a_mode2.toString()}\n"
                case ZeroOffset(rn) => ldrString += s"${a_mode2.toString()}\n"
                case Immed(kind, value) => ldrString += s"=${a_mode2.toString()}\n"
                case Label(value) => ldrString += s"=${a_mode2.toString()}\n"
                case _ => ldrString += s"#${a_mode2.toString()}\n"
            }
            ldrString
        }
    }

    sealed trait Store extends Instruction
    case class STR(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Store {
        override def toString() : String = {
            var strString = s"    STR${cond.getOrElse("").toString()} ${rd.toString()}, "
            a_mode2 match {
                case Immed(kind, value) => strString += s"=${a_mode2.toString()}\n"
                case Label(value) => strString += s"=${a_mode2.toString()}\n"
                case _ => strString += s"${a_mode2.toString()}\n"
            }
            strString
        }
    }
    case class STRB(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Store {
        override def toString() : String = {
            var strString = s"    STR${cond.getOrElse("").toString()}B ${rd.toString()}, "
            a_mode2 match {
                case Immed(kind, value) => strString += s"=${a_mode2.toString()}\n"
                case Label(value) => strString += s"=${a_mode2.toString()}\n"
                case _ => strString += s"${a_mode2.toString()}\n"
            }
            strString
        }
    }

    sealed trait PushPop extends Instruction
    case class PUSH(regList: List[Register]) extends PushPop {
        override def toString() : String = {
            var pushString = s"    PUSH {${regList.head.toString()}"
            for (i <- 1 to regList.size - 1) {
                pushString += ", "
                pushString += regList(i).toString()
            }
            pushString += s"}\n"
            pushString
        }
    }
    case class POP(regList: List[Register]) extends PushPop {
        override def toString() : String = {
            var popString = s"    POP {${regList.head.toString()}"
            for (i <- 1 to regList.size - 1) {
                popString += ", "
                popString += regList(i).toString()
            }
            popString += s"}\n"
            popString
        }
    }
    
}
