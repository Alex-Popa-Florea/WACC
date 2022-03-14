package backend

import backend.data._
import backend.instructions._
import backend.lines._
import backend.operators._
import parsley.registers
import wacc.ast._
import wacc.functionTable._
import wacc.section._
import wacc.symbolTable._
import wacc.types.BoolCheck
import wacc.types.CharCheck
import wacc.types.EmptyPairCheck
import wacc.types.IntCheck
import wacc.types.PairCheck
import wacc.types.StrCheck
import wacc.types._

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

object codeGenerator {
    
    var msg = 0
    var scopeLabels = 0
    var MAX_NUM_BYTES = 1024

    val runtimeError: ListBuffer[Instruction] = ListBuffer(
        BL(None, "p_print_string"), 
        MOV(None, false, R(0), 
        Immed(-1)), 
        BL(None, "exit")
    )

    var functionStackSize = 0
    var stackOffset = 0

    /*
        Method that writes the given lines to a file with given
        file name.
    */
    def writeToFile(lines: List[Line], fileName: String, dir: Boolean): Unit = {
        val file = new File(fileName)
        if (dir) {
            if (!file.getParentFile.exists()){
                file.getParentFile.mkdirs()
            }
        }
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        lines.map(line => bw.write(line.toString()))
        bw.close()
    }

    /*
        Method that iterates through the program and returns a list of lines representing
        the lines of the program in assembly language. 
    */
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

    /* 
        Method that generates the assembly level code for all statements and functions between keywords begin and end.
    */
    def generateNode(node: Node, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {

        node match {
            case Begin(func, stat) =>
                /* 
                    Generates code for user defined functions.
                */
                func.map(function => generateNode(function, symbolTable, functionTable, F(function.id.variable), dataMap, textMap))
                textMap(label) = ListBuffer(PUSH(List(LR())))

                /*
                    Decrements stack based on number of assignments in symbol table.
                */
                var i = decrementStack(symbolTable.getSize(), textMap, label)
                
                /*
                    Generates code for statements within main.
                */
                stat.map(statement => generateNode(statement, symbolTable, functionTable, label, dataMap, textMap))

                /*
                    Increments stack back to its original position.
                */
                i = incrementStack(symbolTable.getSize(), textMap, label)

                textMap(label).addOne(LDR(None, R(0), Immed(0)))
                textMap(label).addOne(POP(List(PC())))
                textMap(label).addOne(Ltorg())

            case function: Function =>
                val funcLabel = F(function.id.variable)
                textMap(funcLabel) = ListBuffer(PUSH(List(LR())))
                functionStackSize = function.semanticTable.getOrElse(symbolTable).getSize() 
                                  - functionTable.getFuncMap()(function.id.variable)._2.foldLeft(0)((x, y) => x + getBytesFromType(y)) - 4
                
                /*
                    Decrements stack based on input size.
                    If more that 1024 bytes are needed, decrements stack in multiple subtractions.
                */
                var i = decrementStack(functionStackSize, textMap, label)
                
                function.stat.map(statement => {
                    generateNode(statement, function.semanticTable.get, functionTable, funcLabel, dataMap, textMap)
                })
                textMap(funcLabel).addOne(POP(List(PC())))
                textMap(funcLabel).addOne(Ltorg()) 

            case AssignType(t, id, rhs) => 
                generateRHS(rhs, symbolTable, functionTable, label, 4, dataMap, textMap)
                val a_mode2 = if (symbolTable.getSizeWithIdent(id).get - id.symbolTable.get.findId(id).get + stackOffset == 0) {
                    ZeroOffset(SP())       
                } else {
                    ImmediateOffset(SP(), Immed(symbolTable.getSizeWithIdent(id).get - id.symbolTable.get.findId(id).get + stackOffset))
                }
                val lhsSize = getBytes(id, symbolTable)
                if (lhsSize == 4) {
                    textMap(label).addOne(STR(None, R(4), a_mode2))
                } else {
                    textMap(label).addOne(STRB(None, R(4), a_mode2))
                }

            case Assign(lhs, rhs) => 
                generateRHS(rhs, symbolTable, functionTable, label, 4, dataMap, textMap)
                generateLHS(lhs, symbolTable, functionTable, label, 5, dataMap, textMap, false)

            case Read(lhs) => 
                generateLHS(lhs, symbolTable, functionTable, label, 4, dataMap, textMap, true)
                textMap(label).addOne(MOV(None, false, R(0), R(4)))
                lhs match {
                    case ident: Ident => 
                        generateReadIdentAndArrayElem(ident, dataMap, textMap, label)

                    case ArrayElem(id, _) =>
                        generateReadIdentAndArrayElem(id, dataMap, textMap, label)

                    case Fst(expr) =>
                        generateReadPairCheck(expr, dataMap, textMap, label, true)
                        
                    case Snd(expr) =>
                        generateReadPairCheck(expr, dataMap, textMap, label, false)
                }

            case Free(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(4)))
                expr match {
                    case ident: Ident => ident.symbolTable.get.find(ident) match {
                        case Some(value) => value match {
                            case baseTypeCheck: BaseTypeCheck => 
                                if (baseTypeCheck.nested != 0) {
                                    textMap(label).addOne(BL(None, "p_free_array"))
                                    generateFreeArray(dataMap, textMap)
                                }

                            case PairCheck(_, _, nested) =>
                                if (nested != 0) {
                                    textMap(label).addOne(BL(None, "p_free_array"))
                                    generateFreeArray(dataMap, textMap)
                                } else {
                                    textMap(label).addOne(BL(None, "p_free_pair"))
                                    generateFreePair(dataMap, textMap)
                                }

                            case EmptyPairCheck() => 
                                textMap(label).addOne(BL(None, "p_free_pair"))
                                generateFreePair(dataMap, textMap)
                        }
                        case None =>
                    }
                    case ArrayElem(id, exprs) => id.symbolTable.get.find(id) match {
                        case Some(value) => (value: @unchecked) match {
                            /*
                                Uses @unchecked to avoid warnings of non exhaustive case matching.
                                Value will always be an array type after semantic analysis
                            */
                            case baseTypeCheck: BaseTypeCheck => 
                                if (baseTypeCheck.nested != exprs.size) {
                                    textMap(label).addOne(BL(None, "p_free_array"))
                                    generateFreeArray(dataMap, textMap)
                                }

                            case PairCheck(_, _, nested) =>
                                if (nested != exprs.size) {
                                    textMap(label).addOne(BL(None, "p_free_array"))
                                    generateFreeArray(dataMap, textMap)
                                } else {
                                    textMap(label).addOne(BL(None, "p_free_pair"))
                                    generateFreePair(dataMap, textMap)
                                }
                        }
                        case None => 
                    }
                    case PairLiter() => 
                        textMap(label).addOne(BL(None, "p_free_pair"))
                        generateFreePair(dataMap, textMap)
                    case _ =>
                }

            case Return(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(4)))
                
                /*
                    Increments stack back to its original position.
                    If more that 1024 bytes are needed, increments stack in multiple subtractions.
                */
                var i = incrementStack(functionStackSize, textMap, label)
                textMap(label).addOne(POP(List(PC())))

            case Exit(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addAll(ListBuffer(
                    MOV(None, false, R(0), R(4)), 
                    BL(None, "exit")
                ))

            case print: PrintTrait =>
                generateExpr(print.expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(4)))
                textMap(label).addOne(print.expr match {
                    case ident: Ident => 
                        (ident.symbolTable.get.find(ident): @unchecked) match {
                            /*
                                Uses @unchecked to avoid warnings of non exhaustive case matching.
                                Ident always has a type in the symbol table.
                            */
                            case Some(typeCheck) => typeCheck match {
                                case IntCheck(nested) => 
                                    if (nested == 0) {
                                        generatePrintInt(dataMap, textMap)
                                        BL(None, "p_print_int")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case BoolCheck(nested) =>
                                    if (nested == 0) {
                                        generatePrintBool(dataMap, textMap)
                                        BL(None, "p_print_bool")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case CharCheck(nested) =>
                                    if (nested == 0) {
                                        BL(None, "putchar")
                                    } else if (nested == 1){
                                        generatePrintString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case StrCheck(nested) =>
                                    if (nested == 0) {
                                        generatePrintString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case PairCheck(type1, type2, nested) =>
                                    generatePrintReference(dataMap, textMap)
                                    BL(None, "p_print_reference")

                                case EmptyPairCheck() => BL(None, "p_print_reference")
                            }
                        }

                    case ArrayElem(id, exprs) => 
                        val size = exprs.size
                        (id.symbolTable.get.find(id): @unchecked) match {
                            /*
                                Uses @unchecked to avoid warnings of non exhaustive case matching.
                                Id always has a type in the symbol table.
                            */
                            case Some(value) => value match {
                                case IntCheck(nested) => 
                                    if (nested == size) {
                                        generatePrintInt(dataMap, textMap)
                                        BL(None, "p_print_int")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case BoolCheck(nested) =>
                                    if (nested == size) {
                                        generatePrintBool(dataMap, textMap)
                                        BL(None, "p_print_bool")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case CharCheck(nested) =>
                                    if (nested == size) {
                                        BL(None, "putchar")
                                    } else if (nested == size - 1){
                                        generatePrintString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case StrCheck(nested) =>
                                    if (nested == size) {
                                        generatePrintString(dataMap, textMap)
                                        BL(None, "p_print_string")
                                    } else {
                                        generatePrintReference(dataMap, textMap)
                                        BL(None, "p_print_reference")
                                    }

                                case PairCheck(type1, type2, nested) =>
                                    generatePrintReference(dataMap, textMap)
                                    BL(None, "p_print_reference")

                                case EmptyPairCheck() => BL(None, "p_print_reference")
                            }
                        }
                    case IntLiter(x) => 
                        generatePrintInt(dataMap, textMap)
                        BL(None, "p_print_int")

                    case BoolLiter(bool) => 
                        generatePrintBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    
                    case CharLiter(char) =>
                        BL(None, "putchar")
                    
                    case StrLiter(string) => 
                        generatePrintString(dataMap, textMap)
                        BL(None, "p_print_string")
                    
                    case PairLiter() => 
                        generatePrintReference(dataMap, textMap)
                        BL(None, "p_print_reference")
                    
                    case Not(expr1) =>
                        generatePrintBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    
                    case Neg(expr1) =>
                        generatePrintInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    
                    case Len(expr1) =>
                        generatePrintInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    
                    case Ord(expr1) => 
                        generatePrintInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    
                    case Chr(expr1) => 
                        BL(None, "putchar")
                    
                    case _: BinOpInt => 
                        generatePrintInt(dataMap, textMap)
                        BL(None, "p_print_int")
                    
                    case _: BinOpBool => 
                        generatePrintBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    
                    case _: BinOpEqs => 
                        generatePrintBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                    
                    case _: BinOpComp => 
                        generatePrintBool(dataMap, textMap)
                        BL(None, "p_print_bool")
                })
                print match {
                    case Println(expr) => 
                        generatePrintLine(dataMap, textMap)
                        textMap(label).addOne(BL(None, "p_print_ln"))
                    
                    case _ =>
                }

            case ifStatement: If => 
                generateExpr(ifStatement.cond, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(CMP(None, R(4), Immed(0)))
                val falseLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(B(Some(EQCOND()), s"L${falseLabel}"))
                
                /*
                    Decrements stack based on input size.
                    If more that 1024 bytes are needed, decrements stack in multiple subtractions.
                */
                var i = decrementStack(ifStatement.trueSemanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                functionStackSize += i
                ifStatement.trueStat.map(statement => generateNode(statement, ifStatement.trueSemanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                
                /*
                    Increments stack back to its original position.
                    If more that 1024 bytes are needed, increments stack in multiple subtractions.
                */
                i = incrementStack(ifStatement.trueSemanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                functionStackSize -= i
                val contLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(B(None, s"L${contLabel}"))
                textMap(label).addOne(L(falseLabel))
                
                /*
                    Decrements stack based on input size.
                    If more that 1024 bytes are needed, decrements stack in multiple subtractions.
                */
                i = decrementStack(ifStatement.falseSemanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                
                functionStackSize += i
                ifStatement.falseStat.map(statement => generateNode(statement, ifStatement.falseSemanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                
                /*
                    Increments stack back to its original position.
                    If more that 1024 bytes are needed, increments stack in multiple subtractions.
                */
                i = incrementStack(ifStatement.falseSemanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                
                functionStackSize -= i                
                textMap(label).addOne(L(contLabel))

            case whileStatement: While => 
                val condLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(B(None, s"L${condLabel}"))
                val bodyLabel = scopeLabels
                scopeLabels += 1
                textMap(label).addOne(L(bodyLabel))

                /*
                    Decrements stack based on input size.
                    If more that 1024 bytes are needed, decrements stack in multiple subtractions.
                */
                var i = decrementStack(whileStatement.semanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                
                functionStackSize += i
                whileStatement.stat.map(statement => generateNode(statement, whileStatement.semanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                
                /*
                    Increments stack back to its original position.
                    If more that 1024 bytes are needed, increments stack in multiple subtractions.
                */
                i = incrementStack(whileStatement.semanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                
                functionStackSize -= i  
                textMap(label).addOne(L(condLabel))
                generateExpr(whileStatement.cond, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(CMP(None, R(4), Immed(1)))
                textMap(label).addOne(B(Some(EQCOND()), s"L${bodyLabel}"))

            case nestedBegin: NestedBegin =>
                
                /*
                    Decrements stack based on input size.
                    If more that 1024 bytes are needed, decrements stack in multiple subtractions.
                */
                var i = decrementStack(nestedBegin.semanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                
                functionStackSize += i
                nestedBegin.stat.map(statement => generateNode(statement, nestedBegin.semanticTable.getOrElse(symbolTable), functionTable, label, dataMap, textMap))
                
                /*
                    Increments stack back to its original position.
                    If more that 1024 bytes are needed, increments stack in multiple subtractions.
                */
                i = incrementStack(nestedBegin.semanticTable.getOrElse(symbolTable).getSize(), textMap, label)
                functionStackSize -= i
                
            case _ => 
        }
    }

    /*
        Method that generates assembly level code for LHS expressions.
    */
    def generateLHS(lhs: AssignLHS, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]], read: Boolean): Unit = {
        lhs match {
            case ident: Ident => 
                if (read) {
                    textMap(label).addOne(ADD(None, false, R(register), SP(), Immed(symbolTable.getSizeWithIdent(ident).get - ident.symbolTable.get.findId(ident).get + stackOffset)))
                } else {
                    val a_mode2 = if (symbolTable.getSizeWithIdent(ident).get - ident.symbolTable.get.findId(ident).get + stackOffset == 0) {
                        ZeroOffset(SP())       
                    } else {
                        ImmediateOffset(SP(), Immed(symbolTable.getSizeWithIdent(ident).get - ident.symbolTable.get.findId(ident).get + stackOffset))
                    }
                    val lhsSize = getBytes(ident, symbolTable)
                    if (lhsSize == 4) {
                        textMap(label).addOne(STR(None, R(register - 1), a_mode2))
                    } else {
                        textMap(label).addOne(STRB(None, R(register - 1), a_mode2))
                    }
                }
            
            case arrayElem: ArrayElem =>
                textMap(label).addOne(ADD(None, false, R(register), SP(), Immed(symbolTable.getSizeWithIdent(arrayElem.id).get - arrayElem.id.symbolTable.get.findId(arrayElem.id).get + stackOffset)))
                var i = 0
                arrayElem.exprs.map(expr => {
                    generateExpr(expr, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                    textMap(label).addAll(List(
                        LDR(None, R(register), ZeroOffset(R(register))),
                        MOV(None, false, R(0), R(register + 1)),
                        MOV(None, false, R(1), R(register)),
                        BL(None, "p_check_array_bounds"),
                        ADD(None, false, R(register), R(register), Immed(4)),
                        (arrayElem.id.symbolTable.get.find(arrayElem.id): @unchecked) match {
                            /*
                                Uses @unchecked to avoid warnings of non exhaustive case matching.
                                Id always has a type in the symbol table after semantic analysis.
                            */
                            case Some(value) => value match {
                                case BoolCheck(nested) => 
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(register), R(register + 1))
                                    } else {
                                        ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed(2)))
                                    }
                                
                                case CharCheck(nested) =>
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(register), R(register + 1))
                                    } else {
                                        ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed(2)))
                                    }
                                
                                case _ =>
                                    ADD(None, false, R(register), R(register), LogicalShiftLeft(R(register + 1), Immed(2)))
                            }
                        }
                    ))
                    generateCheckArrayBounds(dataMap, textMap)
                    i += 1
                })
                if (read) {
                    textMap(label).addOne(MOV(None, false, R(register), R(register)))
                } else {
                    val elemSize = getBytes(arrayElem, symbolTable)
                    if (elemSize == 4) {
                        textMap(label).addOne(STR(None, R(register - 1), ZeroOffset(R(register))))
                    } else {
                        textMap(label).addOne(STRB(None, R(register - 1), ZeroOffset(R(register))))
                    }
                }
            
            case Fst(expr) =>
                generateExpr(expr, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                generateLHSFstAndSnd(read, register, expr, dataMap, textMap, label, true)

            case Snd(expr) =>
                generateExpr(expr, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), ImmediateOffset(R(register), Immed(4))))
                generateLHSFstAndSnd(read, register, expr, dataMap, textMap, label, true)
        }
    }

    /*
        Method that generates assembly level code for RHS expressions.
    */
    def generateRHS(assignRHS: AssignRHS, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        assignRHS match {
            case expr: Expr => generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap) 
            
            case ArrayLiter(array) => 
                var elemSize = 0
                if (array.isEmpty) {
                    textMap(label).addOne(LDR(None, R(0), Immed(4)))
                } else {
                    elemSize = getBytes(array.head, symbolTable)
                    textMap(label).addOne(LDR(None, R(0), Immed(array.size * elemSize + 4)))
                }
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(MOV(None, false, R(register), R(0)))
                if (!array.isEmpty) {
                    var i = 4;
                    array.map(expr => {
                        generateExpr(expr, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                        textMap(label).addOne(
                            if (elemSize == 4) {
                                STR(None, R(register + 1), ImmediateOffset(R(register), Immed(i)))
                            } else {
                                STRB(None, R(register + 1), ImmediateOffset(R(register), Immed(i)))
                            })
                        i += elemSize
                    })
                }
                textMap(label).addOne(LDR(None, R(register + 1), Immed(array.size)))
                textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(register))))
            
            case NewPair(expr1, expr2) => 
                textMap(label).addOne(LDR(None, R(0), Immed(8)))
                textMap(label).addOne(BL(None, "malloc"))
                textMap(label).addOne(MOV(None, false, R(register), R(0)))
                generateExpr(expr1, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                val expr1Bytes = getBytes(expr1, symbolTable)
                textMap(label).addOne(LDR(None, R(0), Immed(expr1Bytes)))
                textMap(label).addOne(BL(None, "malloc"))
                if (expr1Bytes == 4) {
                    textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(0))))
                } else {
                    textMap(label).addOne(STRB(None, R(register + 1), ZeroOffset(R(0))))
                }
                textMap(label).addOne(STR(None, R(0), ZeroOffset(R(register))))
                generateExpr(expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                val expr2Bytes = getBytes(expr2, symbolTable)
                textMap(label).addOne(LDR(None, R(0), Immed(getBytes(expr2, symbolTable))))
                textMap(label).addOne(BL(None, "malloc"))
                if (expr2Bytes == 4) {
                    textMap(label).addOne(STR(None, R(register + 1), ZeroOffset(R(0))))
                } else {
                    textMap(label).addOne(STRB(None, R(register + 1), ZeroOffset(R(0))))
                }
                textMap(label).addOne(STR(None, R(0), ImmediateOffset(R(register), Immed(4))))
                
            case Fst(expr) => 
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                generateRHSFstAndSnd(expr, register, dataMap, textMap, label, true)
            
            case Snd(expr) =>
                generateExpr(expr, symbolTable, functionTable, label, 4, dataMap, textMap)
                textMap(label).addOne(MOV(None, false, R(0), R(register)))
                textMap(label).addOne(BL(None, "p_check_null_pointer"))
                textMap(label).addOne(LDR(None, R(register), ImmediateOffset(R(register), Immed(4))))
                generateRHSFstAndSnd(expr, register, dataMap, textMap, label, false)
            
            case Call(id, args) => 
                args.reverse.map(arg => {
                generateExpr(arg, symbolTable, functionTable, label, 4, dataMap, textMap)
                    if (getBytes(arg, symbolTable) == 1) {
                        textMap(label).addOne(STRB(None, R(4), RegisterWriteBack(SP(), Immed(-1))))
                        stackOffset += 1
                    } else {
                        textMap(label).addOne(STR(None, R(4), RegisterWriteBack(SP(), Immed(-4))))
                        stackOffset += 4
                    }
                })
                stackOffset = 0
                if (preDefFunc.contains(id.variable)) {
                    id.variable match {
                        case "max_int" => 
                            if (args.length == 2) {
                                args(0) match {
                                    case IntLiter(_) => args(1) match {
                                        case IntLiter(_) =>
                                            generateMax(textMap, "int")
                                        case _ => // TODO: WHAT TO DO HERE
                                    }
                                    case _ => // TODO: WHAT TO DO HERE
                                }
                            }
                        case "max_char" =>
                            if (args.length == 2) {
                                args(0) match {
                                    case CharLiter(_) => args(1) match {
                                        case CharLiter(_) =>
                                            generateMax(textMap, "char")
                                        case _ => // TODO: WHAT TO DO HERE
                                    }
                                    case _ => // TODO: WHAT TO DO HERE
                                }
                            }
                        case "min_int" => 
                            if (args.length == 2) {
                                args(0) match {
                                    case IntLiter(_) => args(1) match {
                                        case IntLiter(_) =>
                                            generateMin(textMap, "int")
                                        case _ => // TODO: WHAT TO DO HERE
                                    }
                                    case _ => // TODO: WHAT TO DO HERE
                                }
                            }
                        case "min_char" =>
                            if (args.length == 2) {
                                args(0) match {
                                    case CharLiter(_) => args(1) match {
                                        case CharLiter(_) =>
                                            generateMin(textMap, "char")
                                        case _ => // TODO: WHAT TO DO HERE
                                    }
                                    case _ => // TODO: WHAT TO DO HERE
                                }
                            }
                    }
                    textMap(label).addOne(BL(None, "def_" + id.variable)) 
                } else {
                        textMap(label).addOne(BL(None, "f_" + id.variable))
                }
                textMap(label).addOne(ADD(None, false, SP(), SP(), Immed(args.foldLeft(0)((arg1, arg2) => arg1 + getBytes(arg2, symbolTable)))))
                textMap(label).addOne(MOV(None, false, R(4), R(0)))    
        }
    }

    /*
        Decrements stack based on input size.
        If more that 1024 bytes are needed, decrements stack in multiple subtractions.
    */
    def decrementStack(size: Int, textMap: Map[Scope, ListBuffer[Instruction]], label: Scope): Int = {
        var i = size
        while (i > 0) {
            if (i > MAX_NUM_BYTES) {
                textMap(label).addOne(SUB(None, false, SP(), SP(), Immed(MAX_NUM_BYTES)))
                i -= MAX_NUM_BYTES
            } else {
                textMap(label).addOne(SUB(None, false, SP(), SP(), Immed(i)))
                i = 0
            }
        }
        return i
    }

    /*
        Increments stack back to its original position.
        If more that 1024 bytes are needed, increments stack in multiple subtractions.
    */
    def incrementStack(size: Int, textMap: Map[Scope, ListBuffer[Instruction]], label: Scope): Int = {
        var i = size
        while (i > 0) {
            if (i > MAX_NUM_BYTES) {
                textMap(label).addOne(ADD(None, false, SP(), SP(), Immed(MAX_NUM_BYTES)))
                i -= MAX_NUM_BYTES
            } else {
                textMap(label).addOne(ADD(None, false, SP(), SP(), Immed(i)))
                i = 0
            }
        }
        return i
    }

    /*
        Method that returns the number of bytes required by Expr.
    */
    def getBytes(expr: Expr, symbolTable: SymbolTable): Int = {
        expr match {
            case ident: Ident => 
                ident.symbolTable.get.find(ident) match {
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
                        
                        case _ => 4
                    }
                    case None => 0
                }
            case ArrayElem(id, exprs) => 
                val size = exprs.size
                id.symbolTable.get.find(id) match {
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
                    case None => 0
                }
            case IntLiter(x) => 4
            
            case BoolLiter(bool) => 1
            
            case CharLiter(char) => 1
            
            case StrLiter(string) => 4
            
            case PairLiter() => 4
            
            case Not(expr1) => 1
            
            case Neg(expr1) => 4
            
            case Len(expr1) => 4
            
            case Ord(expr1) => 4
            
            case Chr(expr1) => 1
            
            case _: BinOpInt => 4
            
            case _: BinOpBool => 1
            
            case _: BinOpEqs => 1
            
            case _: BinOpComp => 1
        }
    }

    /*
        Method that returns number of bytes required for input type.  
    */
    def getBytesFromType(typeCheck: TypeCheck): Int = {
        typeCheck match {
            case IntCheck(nested) => 4
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
            case StrCheck(nested) => 4
            case PairCheck(type1, type2, nested) => 4
            case EmptyPairCheck() => 4
        }
    }

    /*
        Method that generates the assembly level code for all expressions.
    */
    def generateExpr(expr: Expr, symbolTable: SymbolTable, functionTable: FunctionTable, label: Scope, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        
        val reg1 = if (register < 10) {
            register
        } else {
            register + 1
        }
        val reg2 = if (register < 10) {
            register + 1
        } else {
            register
        }

        expr match {
            case ident: Ident => 
                val a_mode2 = if (symbolTable.getSizeWithIdent(ident).get - ident.symbolTable.get.findId(ident).get + stackOffset == 0) {
                    ZeroOffset(SP())
                } else {
                    ImmediateOffset(SP(), Immed(symbolTable.getSizeWithIdent(ident).get - ident.symbolTable.get.findId(ident).get + stackOffset))
                }
                val exprSize = getBytes(expr, symbolTable)
                if (exprSize == 4) {
                    textMap(label).addOne(LDR(None, R(register), a_mode2))
                } else {
                    textMap(label).addOne(LDRSB(None, R(register), a_mode2))
                }
            
            case arrayElem: ArrayElem => 
                textMap(label).addOne(ADD(None, false, R(register), SP(), Immed(symbolTable.getSizeWithIdent(arrayElem.id).get - arrayElem.id.symbolTable.get.findId(arrayElem.id).get + stackOffset)))
                var i = 0
                arrayElem.exprs.map(expr => {
                    if (register + 1 > 10) {
                        textMap(label).addOne(PUSH(List(R(10))))
                        stackOffset += 4
                        generateExpr(expr, symbolTable, functionTable, label, register, dataMap, textMap)
                    } else {
                        generateExpr(expr, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                    }
                    if (register + 1 > 10) {
                        stackOffset -= 4
                        textMap(label).addOne(POP(List(R(11))))
                    }
                    textMap(label).addAll(List(
                        LDR(None, R(reg1), ZeroOffset(R(reg1))),
                        MOV(None, false, R(0), R(reg2)),
                        MOV(None, false, R(1), R(reg1)),
                        BL(None, "p_check_array_bounds"),
                        ADD(None, false, R(reg1), R(reg1), Immed(4)),
                        (arrayElem.id.symbolTable.get.find(arrayElem.id): @unchecked) match {
                            /*
                                Uses @unchecked to avoid warnings of non exhaustive case matching.
                                Id always has a type in the symbol table after semantic analysis.
                            */
                            case Some(value) => value match {
                                case BoolCheck(nested) => 
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(reg1), R(reg2))
                                    } else {
                                        ADD(None, false, R(register), R(reg1), LogicalShiftLeft(R(reg2), Immed(2)))
                                    }
                                
                                case CharCheck(nested) =>
                                    if (nested - i == 1) {
                                        ADD(None, false, R(register), R(reg1), R(reg2))
                                    } else {
                                        ADD(None, false, R(register), R(reg1), LogicalShiftLeft(R(reg2), Immed(2)))
                                    }
                                
                                case _ =>
                                    ADD(None, false, R(register), R(reg1), LogicalShiftLeft(R(reg2), Immed(2)))
                            }
                        }
                    ))
                    generateCheckArrayBounds(dataMap, textMap)
                    i += 1
                })
                val elemSize = getBytes(arrayElem, symbolTable)
                if (elemSize == 4) {
                    textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                } else {
                    textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                }
            case IntLiter(x) => 
                textMap(label).addOne(LDR(None, R(register), Immed(x)))
            
            case BoolLiter(bool) =>
                if (bool) {
                    textMap(label).addOne(MOV(None, false, R(register), Immed(1)))
                } else {
                    textMap(label).addOne(MOV(None, false, R(register), Immed(0)))
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
                textMap(label).addOne(LDR(None, R(register), Immed(0)))
            
            case Not(expr1) =>
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(EOR(None, false, R(register), R(register), Immed(1)))
            
            case Neg(expr1) => 
                generateOverflow(dataMap, textMap)
                generateExpr(expr1, symbolTable, functionTable, label, register, dataMap, textMap)
                textMap(label).addOne(RSB(None, true, R(register), R(register), Immed(0)))
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
                if (register + 1 > 10) {
                    textMap(label).addOne(PUSH(List(R(10))))
                    stackOffset += 4
                    generateExpr(binOpInt.expr2, symbolTable, functionTable, label, register, dataMap, textMap)
                } else {
                    generateExpr(binOpInt.expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                }
                if (register + 1 > 10) {
                    stackOffset -= 4
                    textMap(label).addOne(POP(List(R(11))))
                }
                binOpInt match {
                    case Mul(expr1, expr2) => 
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(SMULL(None, false, R(register), R(register + 1), R(reg1), R(reg2)))
                        textMap(label).addOne(CMP(None, R(register + 1), ArithmeticShiftRight(R(register), Immed(31))))
                        textMap(label).addOne(BL(Some(NECOND()), "p_throw_overflow_error"))
                    
                    case Div(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(reg1)))
                        textMap(label).addOne(MOV(None, false, R(1), R(reg2)))
                        textMap(label).addOne(BL(None, "p_check_divide_by_zero"))
                        textMap(label).addOne(BL(None, "__aeabi_idiv"))
                        textMap(label).addOne(MOV(None, false, R(register), R(0)))
                    
                    case Mod(expr1, expr2) =>
                        generateCheckDivZero(dataMap, textMap)
                        textMap(label).addOne(MOV(None, false, R(0), R(reg1)))
                        textMap(label).addOne(MOV(None, false, R(1), R(reg2)))
                        textMap(label).addOne(BL(None, "p_check_divide_by_zero"))
                        textMap(label).addOne(BL(None, "__aeabi_idivmod"))
                        textMap(label).addOne(MOV(None, false, R(register), R(1)))
                    
                    case Add(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(ADD(None, true, R(register), R(reg1), R(reg2)))
                        textMap(label).addOne(BL(Some(VSCOND()), "p_throw_overflow_error"))
                    
                    case Sub(expr1, expr2) =>
                        generateOverflow(dataMap, textMap)
                        textMap(label).addOne(SUB(None, true, R(register), R(reg1), R(reg2)))
                        textMap(label).addOne(BL(Some(VSCOND()), "p_throw_overflow_error"))
                }
                
            case binOpComp: BinOpComp => 
                generateExpr(binOpComp.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                if (register + 1 > 10) {
                    textMap(label).addOne(PUSH(List(R(10))))
                    stackOffset += 4
                    generateExpr(binOpComp.expr2, symbolTable, functionTable, label, register, dataMap, textMap)
                } else {
                    generateExpr(binOpComp.expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                }
                if (register + 1 > 10) {
                    stackOffset -= 4
                    textMap(label).addOne(POP(List(R(11))))
                }
                textMap(label).addOne(CMP(None, R(reg1), R(reg2)))
                binOpComp match {
                    case GT(expr1, expr2) => 
                        textMap(label).addAll(List(
                            MOV(Some(GTCOND()), false, R(register), Immed(1)),
                            MOV(Some(LECOND()), false, R(register), Immed(0))
                        ))
                    
                    case GTE(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(GECOND()), false, R(register), Immed(1)),
                            MOV(Some(LTCOND()), false, R(register), Immed(0))
                        ))
                    
                    case LT(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(LTCOND()), false, R(register), Immed(1)),
                            MOV(Some(GECOND()), false, R(register), Immed(0))
                        ))
                    
                    case LTE(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(LECOND()), false, R(register), Immed(1)),
                            MOV(Some(GTCOND()), false, R(register), Immed(0))
                        ))
                }

            case binOpEqs: BinOpEqs => 
                generateExpr(binOpEqs.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                if (register + 1 > 10) {
                    textMap(label).addOne(PUSH(List(R(10))))
                    stackOffset += 4
                    generateExpr(binOpEqs.expr2, symbolTable, functionTable, label, register, dataMap, textMap)
                } else {
                    generateExpr(binOpEqs.expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                }
                if (register + 1 > 10) {
                    stackOffset -= 4
                    textMap(label).addOne(POP(List(R(11))))
                }
                textMap(label).addOne(CMP(None, R(reg1), R(reg2)))
                binOpEqs match {
                    case EQ(expr1, expr2) => 
                        textMap(label).addAll(List(
                            MOV(Some(EQCOND()), false, R(register), Immed(1)),
                            MOV(Some(NECOND()), false, R(register), Immed(0))
                        ))
                    
                    case NEQ(expr1, expr2) =>
                        textMap(label).addAll(List(
                            MOV(Some(NECOND()), false, R(register), Immed(1)),
                            MOV(Some(EQCOND()), false, R(register), Immed(0))
                        ))
                }
            case binOpBool: BinOpBool => 
                generateExpr(binOpBool.expr1, symbolTable, functionTable, label, register, dataMap, textMap) 
                if (register + 1 > 10) {
                    textMap(label).addOne(PUSH(List(R(10))))
                    stackOffset += 4
                    generateExpr(binOpBool.expr2, symbolTable, functionTable, label, register, dataMap, textMap)
                } else {
                    generateExpr(binOpBool.expr2, symbolTable, functionTable, label, register + 1, dataMap, textMap)
                }
                if (register + 1 > 10) {
                    stackOffset -= 4
                    textMap(label).addOne(POP(List(R(11))))
                }
                binOpBool match {
                    case And(expr1, expr2) => 
                        textMap(label).addOne(AND(None, false, R(register), R(reg1), R(reg2)))
                    
                    case Or(expr1, expr2) =>
                        textMap(label).addOne(ORR(None, false, R(register), R(reg1), R(reg2)))
                }
        }
    }

    /*
        Method that generates start of stack.
    */
    def generateStackStart(symbolTable: SymbolTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(SUB(None, false, SP(), SP(), Immed(symbolTable.getSize())))
    }

    /*
        Method that generates end of stack.
    */
    def generateStackEnd(symbolTable: SymbolTable, label: Scope, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        textMap(label).addOne(ADD(None, false, SP(), SP(), Immed(symbolTable.getSize())))
    }

    /*
        Method that generates assembly level code for printing reference addresses. 
    */
    def generatePrintReference(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_reference")
        if (!dataMap.contains(func)) {
            val string = s"%p\\0"
            dataMap(func) = Msg(msg, 3, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for printing lines appended with escape character new line. 
    */
    def generatePrintLine(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_ln")
        if (!dataMap.contains(func)) {
            val string = s"\\0"

            dataMap(func) = Msg(msg, 1, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "puts"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for printing strings.
    */
    def generatePrintString(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_string")
        if (!dataMap.contains(func)) {
            val string = s"%.*s\\0"
            dataMap(func) = Msg(msg, 5, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                LDR(None, R(1), ZeroOffset(R(0))),
                ADD(None, false, R(2), R(0), Immed(4)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for printing integers of type int.
    */
    def generatePrintInt(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("print_int")
        if (!dataMap.contains(func)) {
            val string = s"%d\\0"
            dataMap(func) = Msg(msg, 3, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for printing booleans.
    */
    def generatePrintBool(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val trueFunc = P("print_bool_true")
        val falseFunc = P("print_bool_false")
        if (!dataMap.contains(trueFunc)) {
            val trueString = s"true\\0"
            val falseString = s"false\\0"
            dataMap(trueFunc) = Msg(msg, 5, trueString)
            msg += 1
            dataMap(falseFunc) = Msg(msg, 6, falseString)
            msg += 1
            textMap(P("print_bool")) = ListBuffer(
                PUSH(List(LR())), 
                CMP(None, R(0), Immed(0)),
                LDR(Some(NECOND()), R(0), Label(s"msg_${dataMap(trueFunc).id}")),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(falseFunc).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "printf"),
                MOV(None, false, R(0), Immed(0)),
                BL(None, "fflush"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for reading integers of type int.
    */
    def generateReadInt(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("read_int")
        if (!dataMap.contains(func)) {
            val string = s"%d\\0"
            dataMap(func) = Msg(msg, 3, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "scanf"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for reading characters.
    */
    def generateReadChar(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("read_char")
        if (!dataMap.contains(func)) {
            val string = s" %c\\0"
            dataMap(func) = Msg(msg, 4, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())), 
                MOV(None, false, R(1), R(0)),
                LDR(None, R(0), Label(s"msg_${dataMap(func).id}")),
                ADD(None, false, R(0), R(0), Immed(4)),
                BL(None, "scanf"),
                POP(List(PC()))
            )
        }
    }

    /*
        Method that generates assembly level code for overflow errors. 
    */
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
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates assembly level code for checking array bounds. 
    */
    def generateCheckArrayBounds(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val negativeFunc = P("check_array_bounds_negative")
        val largeFunc = P("check_array_bounds_large")
        if (!dataMap.contains(negativeFunc)) {
            val negativeString = s"ArrayIndexOutOfBoundsError: negative index\\n\\0"
            val largeString = s"ArrayIndexOutOfBoundsError: index too large\\n\\0"
            dataMap(negativeFunc) = Msg(msg, 44, negativeString)
            msg += 1
            dataMap(largeFunc) = Msg(msg, 45, largeString)
            msg += 1
            textMap(P("check_array_bounds")) = ListBuffer(
                PUSH(List(LR())), 
                CMP(None, R(0), Immed(0)),
                LDR(Some(LTCOND()), R(0), Label(s"msg_${dataMap(negativeFunc).id}")),
                BL(Some(LTCOND()), "p_throw_runtime_error"),
                LDR(None, R(1), ZeroOffset(R(1))),
                CMP(None, R(0), R(1)),
                LDR(Some(CSCOND()), R(0), Label(s"msg_${dataMap(largeFunc).id}")),
                BL(Some(CSCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates assembly level code for checking null pointer errors.   
    */
    def generateCheckNullPointer(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("check_null_pointer")
        if (!dataMap.contains(func)) {
            val string = s"NullReferenceError: dereference a null reference\\n\\0"
            dataMap(func) = Msg(msg, 50, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(0), Immed(0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                BL(Some(EQCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates assembly level code for checking division by zero.
    */
    def generateCheckDivZero(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("check_divide_by_zero")
        if (!dataMap.contains(func)) {
            val string = s"DivideByZeroError: divide or modulo by zero\\n\\0"
            dataMap(func) = Msg(msg, 45, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(1), Immed(0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                BL(Some(EQCOND()), "p_throw_runtime_error"),
                POP(List(PC()))
            )
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates assembly level code for freeing pairs. 
    */
    def generateFreePair(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("free_pair")
        if (!dataMap.contains(func)) {
            val string = s"NullReferenceError: dereference a null reference\\n\\0"
            dataMap(func) = Msg(msg, 50, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(0), Immed(0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                B(Some(EQCOND()), "p_throw_runtime_error"),
                PUSH(List(R(0))),
                LDR(None, R(0), ZeroOffset(R(0))),
                BL(None, "free"),
                LDR(None, R(0), ZeroOffset(SP())),
                LDR(None, R(0), ImmediateOffset(R(0), Immed(4))),
                BL(None, "free"),
                POP(List(R(0))),
                BL(None, "free"),
                POP(List(PC()))
            )
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates assembly level code for freeing arrays.
    */
    def generateFreeArray(dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]]): Unit = {
        val func = P("free_array")
        if (!dataMap.contains(func)) {
            val string = s"NullReferenceError: dereference a null reference\\n\\0"
            dataMap(func) = Msg(msg, 50, string)
            msg += 1
            textMap(func) = ListBuffer(
                PUSH(List(LR())),
                CMP(None, R(0), Immed(0)),
                LDR(Some(EQCOND()), R(0), Label(s"msg_${dataMap(func).id}")),
                B(Some(EQCOND()), "p_throw_runtime_error"),
                BL(None, "free"),
                POP(List(PC()))
            )
            generatePrintString(dataMap, textMap)
            textMap(P("throw_runtime_error")) = runtimeError
        }
    }

    /*
        Method that generates Ident and ArrayElem types for Read.
    */
    def generateReadIdentAndArrayElem(ident: Ident, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]], label: Scope): Unit = {
        ident.symbolTable.get.find(ident) match {
            case Some(typeCheck) => typeCheck match {
                case IntCheck(_) => 
                    generateReadInt(dataMap, textMap)
                    textMap(label).addOne(BL(None, "p_read_int"))
                                    
                case CharCheck(_) =>
                    generateReadChar(dataMap, textMap)
                    textMap(label).addOne(BL(None, "p_read_char"))
                                
                case _ =>
            }
            case None =>
        }
    }

    /*
        Method that generates Fst and Snd of Pairs for Read.
    */
    def generateReadPairCheck(expr: Expr, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]], label: Scope, fst: Boolean): Unit = {
        expr match {
            case ident: Ident => 
                ident.symbolTable.get.find(ident) match {
                    case Some(typeCheck) => typeCheck match {
                        case PairCheck(type1, _, _) if (fst) => type1 match {
                            case IntCheck(_) => 
                                generateReadInt(dataMap, textMap)
                                textMap(label).addOne(BL(None, "p_read_int"))
                                                    
                            case CharCheck(_) =>
                                generateReadChar(dataMap, textMap)
                                textMap(label).addOne(BL(None, "p_read_char"))
                            
                            case _ =>
                        }
                        
                        case PairCheck(_, type2, _) if (!fst) => type2 match {
                            case IntCheck(_) => 
                                generateReadInt(dataMap, textMap)
                                textMap(label).addOne(BL(None, "p_read_int"))
                                                    
                            case CharCheck(_) =>
                                generateReadChar(dataMap, textMap)
                                textMap(label).addOne(BL(None, "p_read_char"))
                            
                            case _ =>
                        }
                        
                        case _ =>
                    }
                    case None => 
                }
            case _ =>
        }
    }

    /*
        Method that generates Fst and Snd of PairCheck type for LHS expressions.
    */
    def generateLHSFstAndSnd(read: Boolean, register: Int, expr: Expr, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]], label: Scope, fst: Boolean): Unit = {
        if (!read) {
            expr match {
                case ident: Ident => 
                    ident.symbolTable.get.find(ident) match {
                        case Some(typeCheck) => typeCheck match {
                            case PairCheck(type1, _, _) if (fst) => 
                                if (getBytesFromType(type1) == 4) {
                                    textMap(label).addOne(STR(None, R(register - 1), ZeroOffset(R(register))))
                                } else {
                                    textMap(label).addOne(STRB(None, R(register - 1), ZeroOffset(R(register))))
                                }
                            
                            case PairCheck(_, type2, _) if (!fst) => 
                                if (getBytesFromType(type2) == 4) {
                                    textMap(label).addOne(STR(None, R(register - 1), ZeroOffset(R(register))))
                                } else {
                                    textMap(label).addOne(STRB(None, R(register - 1), ZeroOffset(R(register))))
                                }

                            case _ =>
                        }
                        case None => 
                    }
                case _ =>
            }
        }
        generateCheckNullPointer(dataMap, textMap)
    }

    /*
        Method that generates Fst and Snd of PairCheck type for RHS expressions.
    */
    def generateRHSFstAndSnd(expr: Expr, register: Int, dataMap: Map[Scope, Msg], textMap: Map[Scope, ListBuffer[Instruction]], label: Scope, fst: Boolean): Unit = {
        expr match {
            case ident: Ident => 
                ident.symbolTable.get.find(ident) match {
                    case Some(typeCheck) => typeCheck match {
                        case PairCheck(type1, _, _) if (fst) => 
                            if (getBytesFromType(type1) == 4) {
                                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            } else {
                                textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            }

                        case PairCheck(_, type2, _) if (!fst) => 
                            if (getBytesFromType(type2) == 4) {
                                textMap(label).addOne(LDR(None, R(register), ZeroOffset(R(register))))
                            } else {
                                textMap(label).addOne(LDRSB(None, R(register), ZeroOffset(R(register))))
                            }    
                    
                        case _ =>
                    }
                    case None => 
                }
            case _ =>
        }
        generateCheckNullPointer(dataMap, textMap)
    }

    def generateMax(textMap: Map[Scope, ListBuffer[Instruction]], argType: String): Unit = {
        val funcName = "max_" + argType
        
        textMap(F(funcName)) = ListBuffer(
            PUSH(List(LR()))
        )

        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(4))))
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(5))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(4))))
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(8))))
        }

        textMap(F(funcName)).addOne(CMP(None, R(4), R(5)))
        textMap(F(funcName)).addOne(MOV(Some(GTCOND()), false, R(4), Immed(1)))
        textMap(F(funcName)).addOne(MOV(Some(LECOND()), false, R(4), Immed(0)))
        textMap(F(funcName)).addOne(CMP(None, R(4), Immed(0)))
        textMap(F(funcName)).addOne(B(Some(EQCOND()), s"L${scopeLabels}"))

        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(4))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(4))))
        }

        textMap(F(funcName)).addOne(MOV(None, false, R(0), R(4)))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(B(None, s"L${scopeLabels + 1}"))
        textMap(F(funcName)).addOne(L(scopeLabels))
        
        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(5))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(8))))
        }
        
        textMap(F(funcName)).addOne(MOV(None, false, R(0), R(4)))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(L(scopeLabels + 1))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(Ltorg())
        
        scopeLabels += 2
    }

    def generateMin(textMap: Map[Scope, ListBuffer[Instruction]], argType: String): Unit = {
        val funcName = "min_" + argType
        
        textMap(F(funcName)) = ListBuffer(
            PUSH(List(LR()))
        )

        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(4))))
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(5))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(4))))
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(8))))
        }

        textMap(F(funcName)).addOne(CMP(None, R(4), R(5)))
        textMap(F(funcName)).addOne(MOV(Some(LTCOND()), false, R(4), Immed(1)))
        textMap(F(funcName)).addOne(MOV(Some(GECOND()), false, R(4), Immed(0)))
        textMap(F(funcName)).addOne(CMP(None, R(4), Immed(0)))
        textMap(F(funcName)).addOne(B(Some(EQCOND()), s"L${scopeLabels}"))

        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(4))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(4))))
        }

        textMap(F(funcName)).addOne(MOV(None, false, R(0), R(4)))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(B(None, s"L${scopeLabels + 1}"))
        textMap(F(funcName)).addOne(L(scopeLabels))
        
        if (argType.equals("char")) {
            textMap(F(funcName)).addOne(LDRSB(None, R(4), ImmediateOffset(SP(), Immed(5))))
        } else {
            textMap(F(funcName)).addOne(LDR(None, R(4), ImmediateOffset(SP(), Immed(8))))
        }
        
        textMap(F(funcName)).addOne(MOV(None, false, R(0), R(4)))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(L(scopeLabels + 1))
        textMap(F(funcName)).addOne(POP(List(PC())))
        textMap(F(funcName)).addOne(Ltorg())
        
        scopeLabels += 2
    }
}