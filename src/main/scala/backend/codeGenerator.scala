package backend

import java.io.FileWriter
import java.io.File
import java.io.BufferedWriter
import wacc.ast._

object codeGenerator {
    
    val text = 
        """|.text
           |
           |""".stripMargin
    val globalMain = 
        """|.global main
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

    def generate(program: Node, fileName: String): Unit = {
        val fileWriter = new FileWriter(new File(fileName))
        val bw = new BufferedWriter(fileWriter)
        generateNode(program, bw)
        bw.close()
    }

    def generateNode(node: Node, bw: BufferedWriter): Unit = {
        node match {
            case Begin(func, stat) => 
                bw.write(text)
                bw.write(globalMain)
                func.map(function => generateNode(function, bw))
                bw.write(mainStart)
                stat.map(statement => generateNode(statement, bw))
                bw.write(mainEnd)
            case Skip() => 
            case _ => 
        }
    }
}
