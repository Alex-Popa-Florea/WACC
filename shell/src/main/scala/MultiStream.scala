package qwacc
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.io.OutputStream
//Tee
object multiStream{
    class MultiStream(sysOut: PrintStream, bufferOut: ByteArrayOutputStream) extends OutputStream{
        override def write(b: Int): Unit = {
            sysOut.write(b)
            bufferOut.write(b)
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