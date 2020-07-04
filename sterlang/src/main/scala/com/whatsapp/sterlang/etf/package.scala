/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.sterlang

import sys.process._
import java.nio.file.{Files, Path, Paths}

import com.ericsson.otp.erlang._
import com.whatsapp.sterlang.forms.FormsConvert

package object etf {
  private val debug = false

  sealed trait ETerm
  case class EAtom(atom: String) extends ETerm
  case class EBitStr(bin: Array[Byte], pad_bits: Int) extends ETerm
  case class EDouble(d: Double) extends ETerm
  case class EList(elems: List[ETerm]) extends ETerm
  case class ELong(value: BigInt) extends ETerm
  case class EMap(entries: List[(ETerm, ETerm)]) extends ETerm
  case class EString(str: String) extends ETerm
  case class ETuple(elems: List[ETerm]) extends ETerm

  def rawProgramFromFile(path: String): Ast.RawProgram = {
    val etf = etfFromFile(path)
    val forms = FormsConvert.fromEtf(etf)
    val elems = forms.flatMap(Convert.convert)
    Ast.RawProgram(elems)
  }

  def etfFromFile(path: String): ETerm = {
    val etfPath =
      if (path.endsWith(".etf")) {
        Paths.get(path)
      } else {
        val tmp =
          if (debug) Paths.get(path.substring(0, path.length - 3) + "etf")
          else Files.createTempFile("etf_reader", ".etf")
        s"./erl2etf -erl $path -etf $tmp".!!
        tmp
      }
    readEtf(etfPath)
  }

  private def readEtf(etfPath: Path): ETerm = {
    val etfBytes = Files.readAllBytes(etfPath)
    val otpObject = new OtpInputStream(etfBytes).read_any()
    val eTerm = fromJava(otpObject)
    eTerm
  }

  private def fromJava(jObject: OtpErlangObject): ETerm =
    jObject match {
      case otpAtom: OtpErlangAtom =>
        EAtom(otpAtom.atomValue())
      case otpBitstr: OtpErlangBitstr =>
        EBitStr(otpBitstr.binaryValue(), otpBitstr.pad_bits())
      case otpDouble: OtpErlangDouble =>
        EDouble(otpDouble.doubleValue())
      case otpList: OtpErlangList =>
        val elems = otpList.elements().toList.map(fromJava)
        assert(otpList.getLastTail == null)
        EList(elems)
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
      case _ =>
        sys.error(s"${jObject.getClass} is not expected")
    }
}
