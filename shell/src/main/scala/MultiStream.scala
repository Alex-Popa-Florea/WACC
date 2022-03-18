package qwacc
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.io.OutputStream

object multiStream{
    class MultiStream(sysOut: PrintStream, bufferOut: ByteArrayOutputStream) extends OutputStream{
        
        override def write(b: Int): Unit = {
            sysOut.write(b)
            bufferOut.write(b)
        }

        override def write(b: Array[Byte]): Unit = {
            sysOut.write(b)
            bufferOut.write(b)
        }
        override def write(b: Array[Byte], off: Int, len: Int): Unit = {
            sysOut.write(b,off,len)
            bufferOut.write(b,off,len)
            
        }
        override def flush(): Unit = {
            sysOut.flush()
            bufferOut.flush()
        }
        override def close(): Unit = {
            sysOut.close()
            bufferOut.close()
        }
    }
}