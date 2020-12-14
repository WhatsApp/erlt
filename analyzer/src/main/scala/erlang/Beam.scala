package erlang

import com.ericsson.otp.erlang.OtpInputStream
import erlang.Data.{EObject, ETuple}

import java.io.DataInputStream
import java.nio.file.{Files, Paths}

object Beam {
  val Abst = 1096971124
  val Dbgi = 1147299689

  def loadAbstractForms(beamPath: String): Option[EObject] = {
    val bytes = Files.readAllBytes(Paths.get(beamPath))
    val byteInputStream = new OtpInputStream(bytes)
    val input = new DataInputStream(byteInputStream)
    // "FOR1"
    input.readInt()
    // length
    input.readInt()
    // BEAM
    input.readInt()
    while (true) {
      val intTag = input.readInt()
      if (intTag == 0) {
        return None
      }
      val length = input.readInt()
      if (intTag == Dbgi || intTag == Abst) {
        val ETuple(elems1) = DataConvert.fromJava(byteInputStream.read_any())
        val ETuple(elems2) = elems1(2)
        return Some(elems2.head)
      }
      byteInputStream.skip((length + 3) & ~3)
    }
    None
  }

}
