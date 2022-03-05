package shell

import scala.io.StdIn.readLine
import scala.util.control.Breaks._
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.BufferedReader
import java.io.FileReader
import io.Source._
import wacc.main._

object shell{

    def main(args: Array[String]) = {
        var command = ""
        val dir = new File("./tmp/")
        dir.mkdir()
        val file = File.createTempFile("tmp",".wacc",dir)//maybe change to /tmp?
        val fileWriter = new FileWriter(file)
        val bw = new BufferedWriter(fileWriter)
        bw.write("begin ")
        breakable{
            while (command != "exit"){
            
                command = readLine(">>> ")
                if(command == "exit") break()
                bw.write(command)
                bw.write(" end")
                val p = file.getPath()
                println(p)
                bw.close()
                wacc.main.main(Array(p,"tmp"))
            }
        }
        
        
        //val content = fromFile(file).mkString
        
        
        //delete tmp file and folder
    }
}