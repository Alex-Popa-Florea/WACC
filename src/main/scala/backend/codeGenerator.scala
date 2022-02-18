package backend

import wacc.ast._

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

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
    val mainStart = 
        """|main:
           |    PUSH {lr}
           |""".stripMargin
    val mainEnd = 
        """|    LDR r0, =0
           |    POP {pc}
           |    .ltorg
           |""".stripMargin
    val runtimeError = 
        """|p_throw_runtime_error:
           |    BL p_print_string
           |    MOV r0, #-1
           |    BL exit
           |""".stripMargin
    val exit = 
        """|    MOV r0, r4
           |    BL exit
           |""".stripMargin

    def generate(program: Node, fileName: String): Unit = {
        val fileWriter = new FileWriter(new File(fileName))
        val dataMap: Map[String, String] = Map.empty
        val textMap: Map[String, String] = Map.empty
        val bw = new BufferedWriter(fileWriter)
        generateNode(program, "", dataMap, textMap)
        if (!dataMap.isEmpty) {
            bw.write(data)
            bw.write(dataMap.values.foldLeft("")((a, b) => a + b))
        }
        // for (i <- 0 to dataMap.size) {
        //     bw.write(dataMap())
        // }
        bw.write("\n")
        bw.write(text)
        bw.write(textMap.foldLeft("")((label, element) => 
            if (element._1.charAt(0) == 'f') {
                label + element._2
            } else {
                label
            }))
        bw.write(textMap("main"))
        bw.write(textMap.foldLeft("")((label, element) => 
            if (element._1.charAt(0) == 'p') {
                label + element._2
            } else {
                label
            }))
        bw.close()
    }

    def generateNode(node: Node, label: String, dataMap: Map[String, String], textMap: Map[String, String]): Unit = {
        node match {
            case Begin(func, stat) =>
                func.map(function => generateNode(function, "f_" + function.id.variable, dataMap, textMap))
                textMap("main") = mainStart
                stat.map(statement => generateNode(statement, "main", dataMap, textMap))
                textMap("main") += mainEnd

            case Function(t, id, vars, stat) => 

            case Exit(expr) => 
                generateExpr(expr, label, 4, dataMap, textMap)
                textMap(label) += exit

            case Skip() => 

            case _ => 
        }
    }

    def generateExpr(expr: Expr, label: String, register: Int, dataMap: Map[String, String], textMap: Map[String, String]): Unit = {
        expr match {
            case Ident(variable) => 
            case ArrayElem(id, exprs) => 
            case IntLiter(x) => 
                textMap(label) += 
                    s"""|    LDR r${register}, =${x}
                        |""".stripMargin
            case BoolLiter(bool) =>
                if (bool) {
                    textMap(label) += 
                        s"""|    MOV r${register}, =1
                            |""".stripMargin
                } else {
                    textMap(label) += 
                        s"""|    MOV r${register}, =0
                            |""".stripMargin
                }
            case CharLiter(char) => 
            case StrLiter(string) => 
            case PairLiter() =>
            case Not(expr1) =>
            case Neg(expr1) => 
            case Len(expr1) => 
            case Ord(expr1) => 
            case Chr(expr1) => 
            case binOpInt: BinOpInt => 
                generateExpr(binOpInt.expr1, label, register, dataMap, textMap) 
                generateExpr(binOpInt.expr2, label , register + 1, dataMap, textMap)
                binOpInt match {
                    case Mul(expr1, expr2) => 
                        generateOverflow(dataMap, textMap)
                        textMap(label) +=
                            s"""|    SMULL r${register}, r${register + 1}, r${register}, r${register + 1}
                                |    CMP r${register + 1}, r${register}, ASR #31
                                |    BLNE p_throw_overflow_error
                                |""".stripMargin
                    case Div(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label) +=
                            s"""|    MOV r0, r${register}
                                |    MOV r1, r${register + 1}
                                |    BL p_check_divide_by_zero
                                |    BL __aeabi_idiv
                                |    MOV r4, r0
                                |""".stripMargin
                    case Mod(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label) +=
                            s"""|    MOV r0, r${register}
                                |    MOV r1, r${register + 1}
                                |    BL p_check_divide_by_zero
                                |    BL __aeabi_idivmod
                                |    MOV r4, r1
                                |""".stripMargin
                    case Add(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label) +=
                            s"""|    ADDS r${register}, r${register}, r${register + 1}
                                |    BLVS p_throw_overflow_error
                                |""".stripMargin
                    case Sub(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label) +=
                            s"""|    SUBS r${register}, r${register}, r${register + 1}
                                |    BLVS p_throw_overflow_error
                                |""".stripMargin
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
                        textMap(label) +=
                            s"""|    AND r${register}, r${register}, r${register + 1}
                                |""".stripMargin
                    case Or(expr1, expr2) =>
                        textMap(label) +=
                            s"""|    ORR r${register}, r${register}, r${register + 1}
                                |""".stripMargin
                }
        }
    }

    def generateString(dataMap: Map[String, String], textMap: Map[String, String]): Unit = {
        if (!dataMap.contains("p_print_string")) {
            dataMap("p_print_string") = 
                s"""|msg_${msg}:
                    |    .word 5
                    |    .ascii	"%.*s\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_print_string") = 
                s"""|p_print_string:
                    |    PUSH {lr}
                    |    LDR r1, [r0]
                    |    ADD r2, r0, #4
                    |    LDR r0, =msg_${dataMap("p_print_string").charAt(4)}
                    |    ADD r0, r0, #4
                    |    BL printf
                    |    MOV r0, #0
                    |    BL fflush
                    |    POP {pc}
                    |""".stripMargin
        }
    }

    def generateOverflow(dataMap: Map[String, String], textMap: Map[String, String]): Unit = {
        if (!dataMap.contains("p_throw_overflow_error")) {
            dataMap("p_throw_overflow_error") = 
                s"""|msg_${msg}:
                    |    .word 83
                    |    .ascii	"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_throw_overflow_error") = 
                s"""|p_throw_overflow_error:
                    |    LDR r0, =msg_${dataMap("p_throw_overflow_error").charAt(4)}
                    |    BL p_throw_runtime_error
                    |""".stripMargin
        }
    }

    def generateCheckDivZero(dataMap: Map[String, String], textMap: Map[String, String]): Unit = {
        if (!dataMap.contains("p_check_divide_by_zero")) {
            dataMap("p_check_divide_by_zero") = 
                s"""|msg_${msg}:
                    |    .word 45
                    |    .ascii	"DivideByZeroError: divide or modulo by zero\\n\\0"
                    |""".stripMargin
            msg += 1
            textMap("p_check_divide_by_zero") = 
                s"""|p_check_divide_by_zero:
                    |    PUSH {lr}
                    |    CMP r1, #0
                    |    LDREQ r0, =msg_${dataMap("p_check_divide_by_zero").charAt(4)}
                    |    BLEQ p_throw_runtime_error
                    |    POP {pc}
                    |""".stripMargin
        }
    }
}
