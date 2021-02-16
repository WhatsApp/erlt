package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang._
import com.whatsapp.eqwalizer.io.EData.EObject

import java.io.DataInputStream
import java.nio.file.{Files, Paths}

object Beam {
  val Abst = 1096971124
  val Dbgi = 1147299689

  def loadAbstractForms(beamPath: String): Option[EObject] =
    Option(loadAbstractFormsJ(beamPath)).map(EData.fromJava)

  def loadAbstractFormsJ(beamPath: String): OtpErlangList = {
    val bytes = Files.readAllBytes(Paths.get(beamPath))
    val byteInputStream = new OtpInputStream(bytes)
    val input = new DataInputStream(byteInputStream)
    // "FOR1"
    input.readInt
    // length
    input.readInt
    // BEAM
    input.readInt
    var result: OtpErlangList = null
    while (result == null && input.available() > 0) {
      val intTag = input.readInt
      val length = input.readInt
      if (intTag == Dbgi || intTag == Abst) {
        val t1 = byteInputStream.read_any.asInstanceOf[OtpErlangTuple]
        val t2 = t1.elementAt(2).asInstanceOf[OtpErlangTuple]
        result = t2.elementAt(0).asInstanceOf[OtpErlangList]
      } else byteInputStream.skip((length + 3) & ~3)
    }
    result
  }
}
