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
import backend.data._
import backend.lines._
import wacc.types.IntCheck
import wacc.types.BoolCheck
import wacc.types.CharCheck
import wacc.types.StrCheck
import wacc.types.PairCheck
import wacc.types.EmptyPairCheck
import parsley.registers
import wacc.section._

object codeGenerator {
    
    var msg = 0
    var scopeLabels = 0
    
    val runtimeError: ListBuffer[Instruction] = ListBuffer(
        BL(None, "p_print_string"), 
        MOV(None, false, R(0), 
        Immed("", -1)), 
        BL(None, "exit")
    )

    def writeToFile(lines: List[Line], fileName: String): Unit = {
        val fileWriter = new FileWriter(new File(fileName))
        val bw = new BufferedWriter(fileWriter)
        lines.map(line => bw.write(line.toString()))
        bw.close()
    }

    def generate(program: Node, symbolTable: SymbolTable, functionTable: FunctionTable): List[Line] = {
        val dataMap: Map[Scope, Msg] = Map.empty
        val textMap: Map[Scope, ListBuffer[Instruction]] = Map.empty
        generateNode(program, symbolTable, functionTable, Main(), dataMap, textMap)
        val lines: ListBuffer[Line] = ListBuffer.empty
        if (!dataMap.isEmpty) {
            lines.addOne(Data())
            lines.addAll(dataMap.values.toSeq.sortBy(x => x.id))
        }
        lines.addOne(Text())
        textMap.keySet.map(key =>
            key match {
                case F(string) => 
                    lines.addOne(key)
                    lines.addAll(textMap(key))
                case _ => 
            }    
        )
        lines.addOne(Main())
        lines.addAll(textMap(Main()))
        textMap.keySet.map(key =>
            key match {
                case P(string) =>
                    lines.addOne(key)
                    lines.addAll(textMap(key))
                case _ => 
            }    
        )
        lines.toList
    }

    def generateNode(node: Node, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {

        node match {
            case Begin(func, stat) =>
                // Generating the code for user defined functions
                func.map(function => generateNode(function, symbolTable, functionTable, F(function.id.variable), dataMap, textMap))

                textMap(label) = ListBuffer(PUSH(List(LR())))

                /*
                    Decrementing the stack based on how many assignments are present in the program, making sure if more that 1024 bytes are needed
                    it is done in multiple subtractions
                */
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
                
                // Generating the code for the statements within main
                stat.map(statement => generateNode(statement, symbolTable, functionTable, label, dataMap, textMap))

                /*
                    Incrementing the stack back to its original position, making sure if more that 1024 bytes are needed
                    it is done in multiple subtractions
                */
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
                textMap(label).addOne(Ltorg())

            case Function(t, id, vars, stat) => 

            case AssignType(t, id, rhs) => 
                generateRHS(rhs, symbolTable, functionTable, label, 4, dataMap, textMap)
                val a_mode2 = if (symbolTable.getSizeWithIdent(id).get - symbolTable.findId(id).get == 0) {
                    ZeroOffset(SP())       
                } else {
                    OImmediateOffset(SP(), Immed("", symbolTable.getSizeWithIdent(id).get - symbolTable.findId(id).get))
                }
                val lhsSize = getBytes(id, symbolTable)
                if (lhsSize == 4) {
                    textMap(label).addOne(STR(None, R(4), a_mode2))
                } else {
                    textMap(label).addOne(STRB(None, R(4), a_mode2))
                }

            case Assign(lhs, rhs) => 
                generateRHS(rhs, symbolTable, functionTable, label, 4, dataMap, textMap)
                lhs match {
                    case ident: Ident => 
                        val a_mode2 = if (symbolTable.getSizeWithIdent(ident).get - symbolTable.findId(ident).get == 0) {
                            ZeroOffset(SP())       
                        } else {
                            OImmediateOffset(SP(), Immed("", symbolTable.getSizeWithIdent(ident).get - symbolTable.findId(ident).get))
                        }
                        val lhsSize = getBytes(ident, symbolTable)
                        if (lhsSize == 4) {
                            textMap(label).addOne(STR(None, R(4), a_mode2))
                        } else {
                            textMap(label).addOne(STRB(None, R(4), a_mode2))
                        }
                    case arrayElem: ArrayElem =>
                        textMap(label).addOne(ADD(None, false, R(5), SP(), Immed("", symbolTable.getSizeWithIdent(arrayElem.id).get - symbolTable.findId(arrayElem.id).get)))
                        var i = 0
                        arrayElem.exprs.map(expr => {
                            generateExpr(expr, symbolTable, functionTable, label, 6, dataMap, textMap)
                            textMap(label).addAll(List(
                                LDR(None, R(5), ZeroOffset(R(5))), 
                                MOV(None, false, R(0), R(6)),
                                MOV(None, false, R(1), R(5)),
                                BL(None, "p_check_array_bounds"),
                                ADD(None, false, R(5), R(5), Immed("", 4)),
                                symbolTable.find(arrayElem.id) match {
                                    case Some(value) => value match {
                                        case BoolCheck(nested) => 
                                            if (nested - i == 1) {
                                                ADD(None, false, R(5), R(5), R(6))
                                            } else {
                                                ADD(None, false, R(5), R(5), LogicalShiftLeft(R(6), Immed("", 2)))
                                            }
                                        case CharCheck(nested) =>
                                            if (nested - i == 1) {
                                                ADD(None, false, R(5), R(5), R(6))
                                            } else {
                                                ADD(None, false, R(5), R(5), LogicalShiftLeft(R(6), Immed("", 2)))
                                            }
                                        case _ =>
                                            ADD(None, false, R(5), R(5), LogicalShiftLeft(R(6), Immed("", 2)))
                                    }
                                    case None => 
                                        ADD(None, false, R(5), R(5), LogicalShiftLeft(R(6), Immed("", 2))) //HEYYYY BROTHER MAYBE CHECK THAT THIS IS RIGHTTT OHHH 
                                }
                            ))
                            generateCheckArrayBounds(dataMap, textMap)
                            i += 1
                        })
                        val elemSize = getBytes(arrayElem, symbolTable)
                        if (elemSize == 4) {
                            textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5))))
                        } else {
                            textMap(label).addOne(STRB(None, R(4), ZeroOffset(R(5))))
                        }
                    case Fst(expr) =>
                        generateExpr(expr, symbolTable, functionTable, label, 5, dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(5)))
                        textMap(label).addOne(BL(None, "p_check_null_pointer"))
                        textMap(label).addOne(LDR(None, R(5), ZeroOffset(R(5))))
                        symbolTable.find(expr match {
                            case ident: Ident => ident
                        }) match {
                            case Some(typeCheck) => typeCheck match {
                                case PairCheck(type1, _, _) => type1 match {
                                    case IntCheck(_) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5)))) 
                                    case BoolCheck(_) => textMap(label).addOne(STRB(None, R(4), ZeroOffset(R(5))))
                                    case CharCheck(_) => textMap(label).addOne(STRB(None, R(4), ZeroOffset(R(5))))
                                    case StrCheck(_) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5))))
                                    case PairCheck(_, _, _) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5))))
                                    case EmptyPairCheck() =>
                                }
                                case _ =>
                            }
                            case None =>
                        }
                        generateCheckNullPointer(dataMap, textMap)
                    case Snd(expr) =>
                        generateExpr(expr, symbolTable, functionTable, label, 5, dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(5)))
                        textMap(label).addOne(BL(None, "p_check_null_pointer"))
                        textMap(label).addOne(LDR(None, R(5), ZeroOffset(R(5))))
                        symbolTable.find(expr match {
                            case ident: Ident => ident
                        }) match {
                            case Some(typeCheck) => typeCheck match {
                                case PairCheck(_, type2, _) => type2 match {
                                    case IntCheck(_) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5)))) 
                                    case BoolCheck(_) => textMap(label).addOne(STRB(None, R(4), ZeroOffset(R(5))))
                                    case CharCheck(_) => textMap(label).addOne(STRB(None, R(4), ZeroOffset(R(5))))
                                    case StrCheck(_) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5))))
                                    case PairCheck(_, _, _) => textMap(label).addOne(STR(None, R(4), ZeroOffset(R(5))))
                                    case EmptyPairCheck() =>
                                }
                                case _ =>
                            }
                            case None =>
                        }
                        generateCheckNullPointer(dataMap, textMap)
                }

            case print: PrintTrait =>
                generateExpr(print.expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(4)))
                textMap(label).addOne(print.expr match {
                    case ident: Ident => 
                        symbolTable.find(ident) match {
                            case Some(typeCheck) => typeCheck match {
                                case IntCheck(nested) => 
                                    if (nested == 0) {
                                        generateInt(dataMap, textMap)
                                        BL(None, "p_print_int")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case BoolCheck(nested) =>
                                    if (nested == 0) {
                                        generateBool(dataMap, textMap)
                                        BL(None, "p_print_bool")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case CharCheck(nested) =>
                                    if (nested == 0) {
                                        BL(None, "putchar")
                                    } else if (nested == 1){
                                        generateString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case StrCheck(nested) =>
                                    if (nested == 0) {
                                        generateString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case PairCheck(type1, type2, nested) =>
                                    generateReference(dataMap, textMap)
                                    BL(None, "p_print_reference")
                                case EmptyPairCheck() => BL(None, "p_print_reference")
                            }
                            case None => BL(None, "p_print_reference") // think 
                        }
                    case ArrayElem(id, exprs) => 
                        val size = exprs.size
                        symbolTable.find(id) match {
                            case Some(value) => value match {
                                case IntCheck(nested) => 
                                    if (nested == size) {
                                        generateInt(dataMap, textMap)
                                        BL(None, "p_print_int")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case BoolCheck(nested) =>
                                    if (nested == size) {
                                        generateBool(dataMap, textMap)
                                        BL(None, "p_print_bool")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case CharCheck(nested) =>
                                    if (nested == size) {
                                        BL(None, "putchar")
                                    } else if (nested == size - 1){
                                        generateString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case StrCheck(nested) =>
                                    if (nested == size) {
                                        generateString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generateReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }
                                case PairCheck(type1, type2, nested) =>
                                    generateReference(dataMap, textMap)
                                    BL(None, "p_print_reference")
                                case EmptyPairCheck() => BL(None, "p_print_reference")
                            }
                            case None => BL(None, "p_print_reference") // one day adhithi, she told me, its all wrong
                        }
                    case IntLiter(x) => 
                        generateInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    case BoolLiter(bool) => 
                        generateBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    case CharLiter(char) =>
                        BL(None, "putchar")
                    case StrLiter(string) => 
                        generateString(dataMap, textMap)
                        BL(None, "p_print_string")
                    case PairLiter() => 
                        generateReference(dataMap, textMap)
                        BL(None, "p_print_reference")
                    case Not(expr1) =>
                        generateBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    case Neg(expr1) =>
                        generateInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    case Len(expr1) =>
                        generateInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    case Ord(expr1) => 
                        generateInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    case Chr(expr1) => 
                        BL(None, "putchar")
                    case _: BinOpInt => 
                        generateInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    case _: BinOpBool => 
                        generateBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    case _: BinOpEqs => 
                        generateBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    case _: BinOpComp => 
                        generateBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                })
                print match {
                    case Println(expr) => 
                        generateLine(dataMap, textMap)
                        textMap(label).addOne(BL(None, "p_print_ln"))
                    case _ =>
                }

            case Exit(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addAll(ListBuffer(
                    MOV(None, false, R(0), R(4)), 
                    BL(None, "exit")
                ))

            case ifStatement: If => 
                generateExpr(ifStatement.cond, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(CMP(None, R(4), Immed("", 0)))
                val falseLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(B(Some(EQCOND()), s"L${falseLabel}"))
                var i = ifStatement.trueSemanticTable.getOrElse(symbolTable).getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }
                ifStatement.trueStat.map(statement => generateNode(statement, ifStatement.trueSemanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                i = ifStatement.trueSemanticTable.getOrElse(symbolTable).getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }
                val contLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(B(None, s"L${contLabel}"))
                textMap(label).addOne(L(falseLabel))
                i = ifStatement.falseSemanticTable.getOrElse(symbolTable).getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }
                ifStatement.falseStat.map(statement => generateNode(statement, ifStatement.falseSemanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                i = ifStatement.falseSemanticTable.getOrElse(symbolTable).getSize()
                while (i > 0) {
                    if (i > 1024) {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", 1024)))
                        i -= 1024
                    } else {
                        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", i)))
                        i = 0
                    }
                }                
                textMap(label).addOne(L(contLabel))
            case Skip() => 

            case _ => 
        }
    }

    def generateRHS(assignRHS: AssignRHS, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        assignRHS match {
            case expr: Expr => generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap) 
            case ArrayLiter(array) => 
                val elemtSize = getBytes(array.head, symbolTable)
                textMap(label).addOne(LDR(None, R(0), Immed("", array.size * elemtSize + 4)))
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(MOV(None, false, R(register), R(0)))
                var i = 4;
                array.map(expr => {
                    generateExpr(expr, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                    textMap(label).addOne(
                        if (elemtSize == 4) {
                            STR(None, R(register + 1), OImmediateOffset(R(register), Immed("", i)))
                        } else {
                            STRB(None, R(register + 1), OImmediateOffset(R(register), Immed("", i)))
                        })
                    i += elemtSize
                })
                textMap(label).addOne(LDR(None, R(register + 1), Immed("", array.size)))
                textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(register))))
            case NewPair(expr1, expr2) => 
                textMap(label).addOne(LDR(None, R(0), Immed("", 8)))
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(MOV(None, false, R(register), R(0)))
                generateExpr(expr1, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                textMap(label).addOne(LDR(None, R(0), Immed("", getBytes(expr1, symbolTable))))
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(0))))
                textMap(label).addOne(STR(None, R(0), ZeroOffset(R(register))))
                generateExpr(expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                textMap(label).addOne(LDR(None, R(0), Immed("", getBytes(expr2, symbolTable))))
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(0))))
                                textMap(label).addOne(STR(None, R(0), OImmediateOffset(R(register), Immed("", 4))))
                
            case Fst(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                symbolTable.find(expr match {
                    case ident: Ident => ident
                }) match {
                    case Some(typeCheck) => typeCheck match {
                        case PairCheck(type1, _, _) => type1 match {
                            case IntCheck(_) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register)))) 
                            case BoolCheck(_) => textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            case CharCheck(_) => textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            case StrCheck(_) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            case PairCheck(_, _, _) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            case EmptyPairCheck() =>
                        }
                        case _ =>
                    }
                    case None =>
                }
                generateCheckNullPointer(dataMap, textMap)
            case Snd(expr) =>
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), OImmediateOffset(R(register), Immed("", 4))))
                symbolTable.find(expr match {
                    case ident: Ident => ident
                }) match {
                    case Some(typeCheck) => typeCheck match {
                        case PairCheck(_, type2, _) => type2 match {
                            case IntCheck(_) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register)))) 
                            case BoolCheck(_) => textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            case CharCheck(_) => textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            case StrCheck(_) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            case PairCheck(_, _, _) => textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            case EmptyPairCheck() => 
                        }
                        case _ =>
                    }
                    case None =>
                }
                generateCheckNullPointer(dataMap, textMap)
            case Call(id, args) =>
                 
        }
    }

    def getBytes(expr: Expr, symbolTable: SymbolTable): Int = {
        expr match {
            case ident: Ident => 
                symbolTable.find(ident) match {
                    case Some(typeCheck) => typeCheck match {
                        case BoolCheck(nested) =>
                            if (nested == 0) {
                                1
                            } else {
                                4
                            }
                        case CharCheck(nested) =>
                            if (nested == 0) {
                                1
                            } else {
                                4
                            }
                        case _ => 4 // only comedy, no work: so wake me up when you think of a solution to this
                    }
                    case None => 0 // somebody once told me this line is kinda shitty, i aint the sharpst wacc in the computingg, she was looking kinda parsley with her combinators and
                }
            case ArrayElem(id, exprs) => 
                val size = exprs.size
                symbolTable.find(id) match {
                    case Some(value) => value match {
                        case BoolCheck(nested) =>
                            if (nested == size) {
                                1
                            } else {
                                4
                            }
                        case CharCheck(nested) =>
                            if (nested == size) {
                                1
                            } else {
                                4
                            }
                        case _ => 4
                    }
                    case None => 0 // one day adhithi, she told me, its all wrong
                }
            case IntLiter(x) => 
                4
            case BoolLiter(bool) => 
                1
            case CharLiter(char) =>
                1
            case StrLiter(string) => 
                4
            case PairLiter() => 
                4
            case Not(expr1) =>
                1
            case Neg(expr1) =>
                4
            case Len(expr1) =>
                4
            case Ord(expr1) => 
                4
            case Chr(expr1) => 
                1
            case _: BinOpInt => 
                4
            case _: BinOpBool => 
                1
            case _: BinOpEqs => 
                1
            case _: BinOpComp => 
                1
        }
    }

    def generateExpr(expr: Expr, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        expr match {
            case ident: Ident => 
                val a_mode2 = if (symbolTable.getSizeWithIdent(ident).get - symbolTable.findId(ident).get == 0) {
                    ZeroOffset(SP())
                } else {
                    OImmediateOffset(SP(), Immed("", symbolTable.getSizeWithIdent(ident).get - symbolTable.findId(ident).get))
                }
                val exprSize = getBytes(expr, symbolTable)
                if (exprSize == 4) {
                    textMap(label).addOne(LDR(None, R(register), a_mode2))
                } else {
                    textMap(label).addOne(LDRSB(None, R(register), a_mode2))
                }
            case arrayElem: ArrayElem => 
                textMap(label).addOne(ADD(None, false, R(register), SP(), Immed("", symbolTable.getSizeWithIdent(arrayElem.id).get - symbolTable.findId(arrayElem.id).get)))
                var i = 0
                arrayElem.exprs.map(expr => {
                    generateExpr(expr, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                    textMap(label).addAll(List(
                        LDR(None, R(register), ZeroOffset(R(register))), 
                        MOV(None, false, R(0), R(register + 1)),
                        MOV(None, false, R(1), R(register)),
                        BL(None, "p_check_array_bounds"),
                        ADD(None, false, R(register), R(register), Immed("", 4)),
                        symbolTable.find(arrayElem.id) match {
                            case Some(value) => value match {
                                case BoolCheck(nested) => 
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(register), R(register + 1))
                                    } else {
                                        ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed("", 2)))
                                    }
                                case CharCheck(nested) =>
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(register), R(register + 1))
                                    } else {
                                        ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed("", 2)))
                                    }
                                case _ =>
                                    ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed("", 2)))
                            }
                            case None => 
                                ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed("", 2))) //HEYYYY BROTHER MAYBE CHECK THAT THIS IS RIGHTTT OHHH 
                        }
                    ))
                    generateCheckArrayBounds(dataMap, textMap)
                    i += 1
                })
                val elemSize = getBytes(arrayElem, symbolTable)
                if (elemSize == 4) {
                    textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register + 1))))
                } else {
                    textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register + 1))))
                }
                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
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
                if (!dataMap.contains(PrintString(string))) {
                    dataMap(PrintString(string)) = Msg(msg, string.length(), string)
                    textMap(label).addOne(LDR(None, R(register), Label(s"msg_${msg}")))
                    msg += 1
                } else {
                    textMap(label).addOne(LDR(None, R(register), Label(s"msg_${dataMap(PrintString(string)).id}")))
                }
            case PairLiter() =>
                textMap(label).addOne(LDR(None, R(register), Immed("", 0)))
            case Not(expr1) =>
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(EOR(None, false, R(register), R(register), Immed("", 1)))
            case Neg(expr1) => 
                generateOverflow(dataMap, textMap)
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(RSB(None, false, R(register), R(register), Immed("", 0)))
                textMap(label).addOne(BL(Some(VSCOND()), "p_throw_overflow_error"))
            case Len(expr1) => 
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
            case Ord(expr1) => 
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
            case Chr(expr1) => 
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
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
            case binOpComp: BinOpComp => 
                generateExpr(binOpComp.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                generateExpr(binOpComp.expr2, symbolTable, functionTable, label , register + 1, dataMap, textMap)
                textMap(label).addOne(CMP(None, R(register), R(register + 1)))
                binOpComp match {
                    case GT(expr1, expr2) => 
                        textMap(label).addAll(List(
                            MOV(Some(GTCOND()), false, R(register), Immed("", 1)),
                            MOV(Some(LECOND()), false, R(register), Immed("", 0))
                        ))
                    case GTE(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(GECOND()), false, R(register), Immed("", 1)),
                            MOV(Some(LTCOND()), false, R(register), Immed("", 0))
                        ))
                    case LT(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(LTCOND()), false, R(register), Immed("", 1)),
                            MOV(Some(GECOND()), false, R(register), Immed("", 0))
                        ))
                    case LTE(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(LECOND()), false, R(register), Immed("", 1)),
                            MOV(Some(GTCOND()), false, R(register), Immed("", 0))
                        ))
                }
            case binOpEqs: BinOpEqs => 
                generateExpr(binOpEqs.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                generateExpr(binOpEqs.expr2, symbolTable, functionTable, label , register + 1, dataMap, textMap)
                textMap(label).addOne(CMP(None, R(register), R(register + 1)))
                binOpEqs match {
                    case EQ(expr1, expr2) => 
                        textMap(label).addAll(List(
                            MOV(Some(EQCOND()), false, R(register), Immed("", 1)),
                            MOV(Some(NECOND()), false, R(register), Immed("", 0))
                        ))
                    case NEQ(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(NECOND()), false, R(register), Immed("", 1)),
                            MOV(Some(EQCOND()), false, R(register), Immed("", 0))
                        ))
                }
            case binOpBool: BinOpBool => 
                generateExpr(binOpBool.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                generateExpr(binOpBool.expr2, symbolTable, functionTable, label , register + 1, dataMap, textMap)
                binOpBool match {
                    case And(expr1, expr2) => 
                        textMap(label).addOne(AND(None, false, R(register), R(register), R(register + 1)))
                    case Or(expr1, expr2) =>
                        textMap(label).addOne(ORR(None, false, R(register), R(register), R(register + 1)))
                }
        }
    }



    def generateStackStart(symbolTable: SymbolTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed("", symbolTable.getSize())))
    }

    def generateStackEnd(symbolTable: SymbolTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed("", symbolTable.getSize())))
    }

    def generateReference(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_reference")
        if (!dataMap.contains(func)) {
            val string = s"%p\\0"
            dataMap(func) = Msg(msg, 3, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateLine(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_ln")
        if (!dataMap.contains(func)) {
            val string = s"\\0"

            dataMap(func) = Msg(msg, 1, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "puts"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateString(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_string")
        if (!dataMap.contains(func)) {
            val string = s"%.*s\\0"
            dataMap(func) = Msg(msg, 5, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                LDR(None, R(1), ZeroOffset(R(0))),
                ADD(None, false, R(2), R(0), Immed("", 4)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateInt(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_int")
        if (!dataMap.contains(func)) {
            val string = s"%d\\0"
            dataMap(func) = Msg(msg, 3, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateBool(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val trueFunc = P("print_bool_true")
        val falseFunc = P("print_bool_false")
        if (!dataMap.contains(trueFunc)) {
            val trueString = s"true\\0"
            val falseString = s"false\\0"
            dataMap(trueFunc) = Msg(msg, 5, trueString)
            msg += 1
            dataMap(falseFunc) = Msg(msg, 6, falseString)
            msg += 1
            textMap(trueFunc) = ListBuffer(
                PUSH(List(LR())), 
                CMP(None, R(0), Immed("", 0)),
                LDR(Some(NECOND()), R(0), Label(s"msg_${dataMap(trueFunc).id}")),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(falseFunc).id}")),
                ADD(None, false, R(0), R(0), Immed("", 4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed("", 0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    def generateOverflow(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("throw_overflow_error")
        if (!dataMap.contains(func)) {
            val string = s"OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n\\0"
            dataMap(func) = Msg(msg, 83, string)
            msg += 1
            textMap(func) = ListBuffer(
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                BL(None, "p_throw_runtime_error")
            )
            generateString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    def generateCheckArrayBounds(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val negativeFunc = P("p_check_array_bounds_negative")
        val largeFunc = P("p_check_array_bounds_large")
        if (!dataMap.contains(negativeFunc)) {
            val negativeString = s"ArrayIndexOutOfBoundsError: negative index\\n\\0"
            val largeString = s"ArrayIndexOutOfBoundsError: index too large\\n\\0"
            dataMap(negativeFunc) = Msg(msg, 44, negativeString)
            msg += 1
            dataMap(largeFunc) = Msg(msg, 45, largeString)
            msg += 1
            textMap(negativeFunc) = ListBuffer(
                PUSH(List(LR())), 
                CMP(None, R(0), Immed("", 0)),
                LDR(Some(LTCOND()), R(0), Label(s"msg_${dataMap(negativeFunc).id}")),
                BL(Some(LTCOND()), "p_throw_runtime_error"),
                LDR(None, R(1), ZeroOffset(R(1))),
                CMP(None, R(0), R(1)),
                LDR(Some(CSCOND()), R(0), Label(s"msg_${dataMap(largeFunc).id}")),
                BL(Some(CSCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generateString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    def generateCheckNullPointer(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("check_null_pointer")
        if (!dataMap.contains(func)) {
            val string = s"NullReferenceError: dereference a null reference\\n\\0"
            dataMap(func) = Msg(msg, 45, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(1), Immed("", 0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                BL(Some(EQCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generateString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    def generateCheckDivZero(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("check_divide_by_zero")
        if (!dataMap.contains(func)) {
            val string = s"DivideByZeroError: divide or modulo by zero\\n\\0"
            dataMap(func) = Msg(msg, 45, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(1), Immed("", 0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                BL(Some(EQCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generateString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }
}
