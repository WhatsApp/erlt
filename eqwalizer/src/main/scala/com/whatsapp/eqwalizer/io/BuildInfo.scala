package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang.OtpInputStream
import com.whatsapp.eqwalizer.io.EData._

import java.nio.file.{Files, Paths}

case class BuildInfo(otpLibRoot: String, apps: Map[String, String])

object BuildInfo {
  def load(path: String): BuildInfo = {
    val bytes = Files.readAllBytes(Paths.get(path))
    val otpObject = new OtpInputStream(bytes).read_any()
    val buildInfoEtf = fromJava(otpObject)
    fromETF(buildInfoEtf)
  }

  private def fromETF(etf: EObject): BuildInfo = {
    val map = etf.asInstanceOf[EMap].entries.toMap
    val otpLibRoot = new String(map(EAtom("otp_lib_dir")).asInstanceOf[EBitStr].bin)
    val appKVs = map(EAtom("apps")).asInstanceOf[EList].elems
    val apps = appKVs.collect { case ETuple(List(EBitStr(nameBits, _), EBitStr(pathBits, _))) =>
      new String(nameBits) -> new String(pathBits)
    }.toMap
    BuildInfo(otpLibRoot, apps)
  }
}
