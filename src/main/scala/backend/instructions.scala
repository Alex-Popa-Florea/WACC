package backend

import backend.operators._
import lines._

object instructions {
    
    val preDefFunc: List[String] = List("max_int", "max_char", "min_int", "min_char", "abs", "pow")

    /*
        Trait to represent elements within the text section of the assembly code.
    */
    sealed trait Instruction extends Line
    
    /*
        Trait to represent labels showing scopes and functions of the assembly code.
    */
    sealed trait Scope extends Instruction
    
    /*
        The main function.
    */
    case class Main() extends Scope {
        override def toString() : String = {
            "main:\n"
        }
    }

    /*
        Helper functions generated within the code.
    */
    case class P(string: String) extends Scope {
        override def toString() : String = {
            "p_" + string + ":\n"
        }
    }
    
    /*
        Functions defined within wacc the program.
    */
    case class F(string: String) extends Scope {
        override def toString() : String = {
            if (preDefFunc.contains(string)) {
                "def_" + string + ":\n"
            }
            else {
                "f_" + string + ":\n"
            }
        }
    }
    
    /*
        Inner labels to jump to in if and while statements.
    */
    case class L(id: Int) extends Scope {
        override def toString() : String = {
            s"""L${id}:\n"""
        }
    }

    /*
        String literals within the program.
    */
    case class PrintString(string: String) extends Scope {
        override def toString() : String = {
            string
        }
    }

    /*
        LTORG. label.
    */
    case class Ltorg() extends Instruction {
        override def toString() : String = {
            "    .ltorg\n"
        }
    }
    
    /*
        Sealed trait to represent arithmetic instructions.
    */
    sealed trait Arithmetic extends Instruction
    
    /*
        Add assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class ADD(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Arithmetic {
        override def toString() : String = {
            var addString = s"    ADD${cond.getOrElse("").toString()}"
            if (s) {
                addString += "S"
            }
            addString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => addString += "#"
                case Character(value) => addString += "#"
                case _ =>
            }
            addString += s"${operand2.toString()}\n"
            addString
        }
    }
    
    /*
        Substract assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class SUB(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Arithmetic {
        override def toString() : String = {
            var subString = s"    SUB${cond.getOrElse("").toString()}"
            if (s) {
                subString += "S"
            }
            subString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => subString += "#"
                case Character(value) => subString += "#"
                case _ =>
            }
            subString += s"${operand2.toString()}\n"
            subString
        }
    }
    
    /*
        Reverse substract assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class RSB(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Arithmetic {
        override def toString() : String = {
            var subString = s"    RSB${cond.getOrElse("").toString()}"
            if (s) {
                subString += "S"
            }
            subString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => subString += "#"
                case Character(value) => subString += "#"
                case _ =>
            }
            subString += s"${operand2.toString()}\n"
            subString
        }
    }
    
    /*
        Multiply signed long assembly instruction, with optional condition codes and optional setting of flags.
    */
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
    
    /*
        Sealed trait to represent comparison instructions.
    */
    sealed trait Compare extends Instruction
    
    /*
        Compare assembly instruction, with optional condition codes.
    */
    case class CMP(cond: Option[Cond], rn: Register, operand2: Operand2) extends Compare {
        override def toString() : String = {
            var cmpString = s"    CMP${cond.getOrElse("").toString()} ${rn.toString()}, "
            operand2 match {
                case Immed(value) => cmpString += "#"
                case Character(value) => cmpString += "#"
                case _ =>
            }
            cmpString += s"${operand2.toString()}\n"
            cmpString
        }
    }
    
    /*
        Compare negative assembly instruction, with optional condition codes
    */
    case class CMN(cond: Option[Cond], rn: Register, operand2: Operand2) extends Compare {
        override def toString() : String = {
            var cmnString = s"    CMN${cond.getOrElse("").toString()} ${rn.toString()}, "
            operand2 match {
                case Immed(value) => cmnString += "#"
                case Character(value) => cmnString += "#"
                case _ =>
            }
            cmnString += s"${operand2.toString()}\n"
            cmnString
        }
    }

    /*
        Sealed trait to represent logical instructions.
    */
    sealed trait Logical extends Instruction
    
    /*
        Move assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class MOV(cond: Option[Cond], s: Boolean, rd: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var movString = s"    MOV${cond.getOrElse("").toString()}"
            if (s) {
                movString += "S"
            }
            movString += s" ${rd.toString()}, "
            operand2 match {
                case Immed(value) => movString += "#"
                case Character(value) => movString += "#"
                case _ =>
            }
            movString += s"${operand2.toString()}\n"
            movString
        }
    }
    
    /*
        AND assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class AND(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var andString = s"    AND${cond.getOrElse("").toString()}"
            if (s) {
                andString += "S"
            }
            andString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => andString += "#"
                case Character(value) => andString += "#"
                case _ =>
            }
            andString += s"${operand2.toString()}\n"
            andString
        }
    }
    
    /*
        XOR assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class EOR(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var eorString = s"    EOR${cond.getOrElse("").toString()}"
            if (s) {
                eorString += "S"
            }
            eorString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => eorString += "#"
                case Character(value) => eorString += "#"
                case _ =>
            }
            eorString += s"${operand2.toString()}\n"
            eorString
        }
    }
    
    /*
        OR assembly instruction, with optional condition codes and optional setting of flags.
    */
    case class ORR(cond: Option[Cond], s: Boolean, rd: Register, rn: Register, operand2: Operand2) extends Logical {
        override def toString() : String = {
            var orrString = s"    ORR${cond.getOrElse("").toString()}"
            if (s) {
                orrString += "S"
            }
            orrString += s" ${rd.toString()}, ${rn.toString()}, "
            operand2 match {
                case Immed(value) => orrString += "#"
                case Character(value) => orrString += "#"
                case _ =>
            }
            orrString += s"${operand2.toString()}\n"
            orrString
        }
    }

    /*
        Sealed trait to represent branch instructions.
    */
    sealed trait Branch extends Instruction
    
    /*
        Branch assembly instruction, with optional condition codes.
    */
    case class B(cond: Option[Cond], label: String) extends Branch {
        override def toString() : String = {
            s"    B${cond.getOrElse("").toString()} ${label}\n"
        }
    }
    
    /*
        Branch with link assembly instruction, with optional condition codes.
    */
    case class BL(cond: Option[Cond], label: String) extends Branch {
        override def toString() : String = {
            s"    BL${cond.getOrElse("").toString()} ${label}\n"
        }
    }

    /*
        Sealed trait to represent load instructions.
    */
    sealed trait Load extends Instruction
    
    /*
        Loading a word assembly instruction, with optional condition codes.
    */
    case class LDR(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Load {
        override def toString() : String = {
            var ldrString = s"    LDR${cond.getOrElse("").toString()} ${rd.toString()}, "
            a_mode2 match {
                case ImmediateOffset(rn, immed) => ldrString += s"${a_mode2.toString()}\n"
                case ZeroOffset(rn) => ldrString += s"${a_mode2.toString()}\n"
                case Immed(value) => ldrString += s"=${a_mode2.toString()}\n"
                case Label(value) => ldrString += s"=${a_mode2.toString()}\n"
                case _ => ldrString += s"#${a_mode2.toString()}\n"
            }
            ldrString
        }
    }
    
    /*
        Loading a byte assembly instruction, with optional condition codes.
    */
    case class LDRB(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Load {
        override def toString() : String = {
            var ldrString = s"    LDR${cond.getOrElse("").toString()}B ${rd.toString()}, "
            a_mode2 match {
                case ImmediateOffset(rn, immed) => ldrString += s"${a_mode2.toString()}\n"
                case ZeroOffset(rn) => ldrString += s"${a_mode2.toString()}\n"
                case Immed(value) => ldrString += s"=${a_mode2.toString()}\n"
                case Label(value) => ldrString += s"=${a_mode2.toString()}\n"
                case _ => ldrString += s"#${a_mode2.toString()}\n"
            }
            ldrString
        }
    }
    
    /*
        Loading a signed byte assembly instruction, with optional condition codes.
    */
    case class LDRSB(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Load {
        override def toString() : String = {
            var ldrString = s"    LDR${cond.getOrElse("").toString()}SB ${rd.toString()}, "
            a_mode2 match {
                case ImmediateOffset(rn, immed) => ldrString += s"${a_mode2.toString()}\n"
                case ZeroOffset(rn) => ldrString += s"${a_mode2.toString()}\n"
                case Immed(value) => ldrString += s"=${a_mode2.toString()}\n"
                case Label(value) => ldrString += s"=${a_mode2.toString()}\n"
                case _ => ldrString += s"#${a_mode2.toString()}\n"
            }
            ldrString
        }
    }

    /*
        Sealed trait to represent store instructions.
    */
    sealed trait Store extends Instruction
    
    /*
        Storing a word assembly instruction, with optional condition codes.
    */
    case class STR(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Store {
        override def toString() : String = {
            var strString = s"    STR${cond.getOrElse("").toString()} ${rd.toString()}, "
            a_mode2 match {
                case Immed(value) => strString += s"=${a_mode2.toString()}\n"
                case Label(value) => strString += s"=${a_mode2.toString()}\n"
                case _ => strString += s"${a_mode2.toString()}\n"
            }
            strString
        }
    }

    /*
        Storing a byte assembly instruction, with optional condition codes
    */
    case class STRB(cond: Option[Cond], rd: Register, a_mode2: A_mode2) extends Store {
        override def toString() : String = {
            var strString = s"    STR${cond.getOrElse("").toString()}B ${rd.toString()}, "
            a_mode2 match {
                case Immed(value) => strString += s"=${a_mode2.toString()}\n"
                case Label(value) => strString += s"=${a_mode2.toString()}\n"
                case _ => strString += s"${a_mode2.toString()}\n"
            }
            strString
        }
    }

    /*
        Sealed trait to represent push and pop instructions
    */
    sealed trait PushPop extends Instruction
    
    /*
        Push assembly instruction, to push registers onto the stack
    */
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
    
    /*
        Pop assembly instruction, to pop registers from the stack
    */
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
