package backend

import wacc.ast._

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import wacc.symbolTable._
import wacc.functionTable._
import backend.instructions._
import backend.operators._

object codeGenerator {
    
    var msg = 0

    val data = 
        """|.data
           |
           |""".stripMargin
    val text = 
        """|.text
           |
           |.global main
           |""".stripMargin
    val runtimeError: ListBuffer[Instruction] = ListBuffer(
        BL(None, "p_print_string"), 
        MOV(None, false, R(0), 
        Immed("", -1)), 
        BL(None, "exit")
    )
    val exit: ListBuffer[Instruction] = ListBuffer(
        MOV(None, false, R(0), R(4)), 
        BL(None, "exit")
    )

    def generate(program: Node, symbolTable: SymbolTable, functionTable: FunctionTable, fileName: String): Unit = {
        val fileWriter = new FileWriter(new File(fileName))
        val dataMap: Map[String, String] = Map.empty
        val textMap: Map[String, ListBuffer[Instruction]] = Map.empty
        val bw = new BufferedWriter(fileWriter)
        generateNode(program, symbolTable, functionTable, "main", dataMap, textMap)
        if (!dataMap.isEmpty) {
            bw.write(dataMap.values.foldLeft(data)((a, b) => a + b))
            bw.write("\n")
        }
        bw.write(textMap.foldLeft(text)((label, element) => 
            if (element._1.charAt(0) == 'f') {
                label + element._1 + "\n" + element._2.foldLeft("")((a, b) => a + b.toString())
            } else {
                label
            }))
        bw.write(textMap("main").foldLeft("main:\n")((a, b) => a + b.toString()))
        bw.write(textMap.foldLeft("    .ltorg\n")((label, element) => 
            if (element._1.charAt(0) == 'p') {
                label + element._1 + "\n" + element._2.foldLeft("")((a, b) => a + b.toString())
            } else {
                label
            }))
        bw.close()
    }

    def generateNode(node: Node, symbolTable: SymbolTable, functionTable: FunctionTable, label: String, dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {

        node match {
            case Begin(func, stat) =>
                func.map(function => generateNode(function, symbolTable, functionTable, "f_" + function.id.variable, dataMap, textMap))
                textMap(label) = ListBuffer(PUSH(List(LR())))
                var i = symbolTable.getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }
                
                stat.map(statement => generateNode(statement, symbolTable, functionTable, label, dataMap, textMap))
                i = symbolTable.getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }
                textMap(label).addOne(LDR(None, R(0), Immed("", 0)))
                textMap(label).addOne(POP(List(PC())))

            case Function(t, id, vars, stat) => 

            case AssignType(t, id, rhs) => 
                generateRHS(rhs, symbolTable, functionTable, label, 4, dataMap, textMap)
                if (symbolTable.getSize() - symbolTable.findId(id).get == 0) {
                    textMap(label).addOne(STR(None, R(4), ZeroOffset(SP())))
                } else {
                    textMap(label).addOne(STR(None, R(4), OImmediateOffset(SP(), Immed("", symbolTable.getSize() - symbolTable.findId(id).get))))
                }
            
            case Assign(lhs, rhs) => 

            case Exit(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addAll(exit)

            case Skip() => 

            case _ => 
        }
    }

    def generateRHS(assignRHS: AssignRHS, symbolTable: SymbolTable, functionTable: FunctionTable, label: String, register: Int, dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        assignRHS match {
            case expr: Expr => generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap) 
            case ArrayLiter(array) => 
            case NewPair(expr1, expr2) => 
            case Fst(expr) => 
            case Snd(expr) => 
            case Call(id, args) => 
        }
    }

    def generateExpr(expr: Expr, symbolTable: SymbolTable, functionTable: FunctionTable, label: String, register: Int, dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        expr match {
            case ident: Ident => 
                if (symbolTable.getSize() - symbolTable.findId(ident).get == 0) {
                    textMap(label).addOne(LDR(None, R(register), ZeroOffset(SP())))
                } else {
                    textMap(label).addOne(LDR(None, R(register), OImmediateOffset(SP(), Immed("", symbolTable.getSize() - symbolTable.findId(ident).get))))
                }
            case ArrayElem(id, exprs) => 
            case IntLiter(x) => 
                textMap(label).addOne(LDR(None, R(register), Immed("", x)))
            case BoolLiter(bool) =>
                if (bool) {
                    textMap(label).addOne(MOV(None, false, R(register), Immed("", 1)))
                } else {
                    textMap(label).addOne(MOV(None, false, R(register), Immed("", 0)))
                }
            case CharLiter(char) => 
                textMap(label).addOne(MOV(None, false, R(register), Character(char)))
            case StrLiter(string) => 
            case PairLiter() =>
            case Not(expr1) =>
            case Neg(expr1) => 
            case Len(expr1) => 
            case Ord(expr1) => 
            case Chr(expr1) => 
            case binOpInt: BinOpInt => 
                generateExpr(binOpInt.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                generateExpr(binOpInt.expr2, symbolTable, functionTable, label , register + 1, dataMap, textMap)
                binOpInt match {
                    case Mul(expr1, expr2) => 
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(SMULL(None, false, R(register), R(register + 1), R(register), R(register + 1)))
                        textMap(label).addOne(CMP(None, R(register + 1), R(register)))
                        textMap(label).addOne(BL(Some(NECOND()), "p_throw_overflow_error"))
                    case Div(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(register)))
                        textMap(label).addOne(MOV(None, false, R(1), R(register + 1)))
                        textMap(label).addOne(BL(None, "p_check_divide_by_zero"))
                        textMap(label).addOne(BL(None, "__aeabi_idiv"))
                        textMap(label).addOne(MOV(None, false, R(4), R(0)))
                    case Mod(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(register)))
                        textMap(label).addOne(MOV(None, false, R(1), R(register + 1)))
                        textMap(label).addOne(BL(None, "p_check_divide_by_zero"))
                        textMap(label).addOne(BL(None, "__aeabi_idivmod"))
                        textMap(label).addOne(MOV(None, false, R(4), R(1)))
                    case Add(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(ADD(None, true, R(register), R(register), R(register + 1)))
                        textMap(label).addOne(BL(Some(VSCOND()), "p_throw_overflow_error"))
                    case Sub(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(SUB(None, true, R(register), R(register), R(register + 1)))
                        textMap(label).addOne(BL(Some(VSCOND()), "p_throw_overflow_error"))
                }
                textMap("p_throw_runtime_error") = runtimeError
                generateString(dataMap, textMap)
            case GT(expr1, expr2) => 
            case GTE(expr1, expr2) => 
            case LT(expr1, expr2) => 
            case LTE(expr1, expr2) => 
            case EQ(expr1, expr2) => 
            case NEQ(expr1, expr2) => 
            case binOpBool: BinOpBool => 
                binOpBool match {
                    case And(expr1, expr2) => 
                        textMap(label).addOne(AND(None, false, R(register), R(register), R(register + 1)))
                    case Or(expr1, expr2) =>
                        textMap(label).addOne(ORR(None, false, R(register), R(register), R(register + 1)))
                }
        }
    }



    def generateStackStart(symbolTable: SymbolTable, label: String, dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", symbolTable.getSize())))
    }

    def generateStackEnd(symbolTable: SymbolTable, label: String, dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", symbolTable.getSize())))
    }

    def generateString(dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        if (!dataMap.contains("p_print_string")) {
            dataMap("p_print_string") = 
                s"""|msg_${msg}:
                    |    .word 5
                    |    .ascii	"%.*s\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_print_string") = ListBuffer(
                PUSH(List(LR())), 
                LDR(None, R(1), ZeroOffset(R(0))),
                ADD(None, false, R(2), R(0), Immed("", 4)),
                LDR(None, R(0), Label(s"msg_${dataMap("p_print_string").charAt(4)}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateOverflow(dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        if (!dataMap.contains("p_throw_overflow_error")) {
            dataMap("p_throw_overflow_error") = 
                s"""|msg_${msg}:
                    |    .word 83
                    |    .ascii	"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_throw_overflow_error") = ListBuffer(
                LDR(None, R(0), Label(s"msg_${dataMap("p_throw_overflow_error").charAt(4)}")),
                BL(None, "p_throw_runtime_error")
            )
            // textMap("p_throw_overflow_error").addOne(BL(None, "p_throw_runtime_error"))
        }
    }

    def generateCheckDivZero(dataMap: Map[String, String], textMap: Map[String, ListBuffer[Instruction]]): Unit = {
        if (!dataMap.contains("p_check_divide_by_zero")) {
            dataMap("p_check_divide_by_zero") = 
                s"""|msg_${msg}:
                    |    .word 45
                    |    .ascii	"DivideByZeroError: divide or modulo by zero\\n\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_check_divide_by_zero") = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(1), Immed("", 0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap("p_check_divide_by_zero").charAt(4)}")),
                BL(Some(EQCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            // textMap("p_check_divide_by_zero").addOne(CMP(None, R(1), Immed("", 0)))
            // textMap("p_check_divide_by_zero").addOne(LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap("p_check_divide_by_zero").charAt(4)}")))
            // textMap("p_check_divide_by_zero").addOne(BL(Some(EQCOND()), "p_throw_runtime_error"))
            // textMap("p_check_divide_by_zero").addOne(POP(List(PC())))
        }
    }
}
