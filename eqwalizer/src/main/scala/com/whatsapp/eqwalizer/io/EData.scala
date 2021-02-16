package com.whatsapp.eqwalizer.io

import com.ericsson.otp.erlang._

object EData {
  sealed trait EObject
  case class EAtom(atom: String) extends EObject
  case class EBitStr(bin: Array[Byte], pad_bits: Int) extends EObject
  case class EDouble(d: Double) extends EObject
  case class EExternalFun(module: String, function: String, arity: Int) extends EObject
  case class EList(elems: List[EObject], lastTail: Option[EObject]) extends EObject
  case class ELong(value: BigInt) extends EObject
  case class EMap(entries: List[(EObject, EObject)]) extends EObject
  case class EString(str: String) extends EObject
  case class ETuple(elems: List[EObject]) extends EObject

  def fromJava(jObject: OtpErlangObject): EObject =
    jObject match {
      case otpAtom: OtpErlangAtom =>
        EAtom(otpAtom.atomValue())
      case otpBitstr: OtpErlangBitstr =>
        EBitStr(otpBitstr.binaryValue(), otpBitstr.pad_bits())
      case otpDouble: OtpErlangDouble =>
        EDouble(otpDouble.doubleValue())
      case otpList: OtpErlangList =>
        val elems = otpList.elements().toList.map(fromJava)
        val lastTail = Option(otpList.getLastTail).map(fromJava)
        EList(elems, lastTail)
      case otpLong: OtpErlangLong =>
        ELong(otpLong.bigIntegerValue())
      case otpMap: OtpErlangMap =>
        val otpKeys = otpMap.keys()
        val otpValues = otpKeys.map(k => otpMap.get(k))
        val eKeys = otpKeys.toList.map(fromJava)
        val eValues = otpValues.toList.map(fromJava)
        EMap(eKeys.zip(eValues))
      case otpString: OtpErlangString =>
        EString(otpString.stringValue())
      case otpTuple: OtpErlangTuple =>
        val elems = otpTuple.elements().toList.map(fromJava)
        ETuple(elems)
    }
}
