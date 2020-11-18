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

import java.nio.file.{Files, Path, Paths}

import com.ericsson.otp.erlang._
import com.whatsapp.sterlang.Doc.HoverSpec
import com.whatsapp.sterlang.forms.FormsConvert

object Etf {

  sealed trait ETerm
  case class EAtom(atom: String) extends ETerm
  case class EBinary(str: String) extends ETerm
  case class EDouble(d: Double) extends ETerm
  case class EList(elems: List[ETerm]) extends ETerm
  case class ELong(value: BigInt) extends ETerm
  case class EString(str: String) extends ETerm
  case class ETuple(elems: List[ETerm]) extends ETerm
  case class EPid(pid: OtpErlangPid) extends ETerm
  case class ERef(ref: OtpErlangRef) extends ETerm
  case class EMap(keys: List[ETerm], values: List[ETerm]) extends ETerm

  def readEtf(etfPath: Path): ETerm = {
    val etfBytes = Files.readAllBytes(etfPath)
    val otpObject = new OtpInputStream(etfBytes).read_any()
    val eTerm = fromJava(otpObject)
    eTerm
  }

  def fromJava(jObject: OtpErlangObject): ETerm =
    jObject match {
      case otpTuple: OtpErlangTuple =>
        val elems = otpTuple.elements().toList.map(fromJava)
        ETuple(elems)
      case otpAtom: OtpErlangAtom =>
        EAtom(otpAtom.atomValue())
      case otpList: OtpErlangList =>
        val elems = otpList.elements().toList.map(fromJava)
        assert(otpList.getLastTail == null)
        EList(elems)
      case otpLong: OtpErlangLong =>
        ELong(otpLong.bigIntegerValue())
      case otpDouble: OtpErlangDouble =>
        EDouble(otpDouble.doubleValue())
      case otpString: OtpErlangString =>
        EString(otpString.stringValue())
      case otpPid: OtpErlangPid =>
        EPid(otpPid)
      case otpRef: OtpErlangRef =>
        ERef(otpRef)
    }

  def toJava(eTerm: ETerm): OtpErlangObject =
    eTerm match {
      case EAtom(atom) =>
        new OtpErlangAtom(atom)
      case EDouble(d) =>
        new OtpErlangDouble(d)
      case EList(elems) =>
        new OtpErlangList(elems.map(toJava).toArray)
      case ELong(value) =>
        new OtpErlangLong(value.bigInteger)
      case EString(str) =>
        new OtpErlangString(str)
      case ETuple(elems) =>
        new OtpErlangTuple(elems.map(toJava).toArray)
      case EPid(pid) =>
        pid
      case ERef(ref) =>
        ref
      case EMap(keys, values) =>
        new OtpErlangMap(keys.map(toJava).toArray, values.map(toJava).toArray)
      case EBinary(str) =>
        new OtpErlangBinary(str.getBytes())
    }

  def hoverSpecToMap(hoverSpec: HoverSpec): EMap = {
    EMap(
      List(EAtom("range"), EAtom("kind"), EAtom("id"), EAtom("data")),
      List(
        EMap(
          List(EAtom("from"), EAtom("to")),
          List(
            ETuple(List(ELong(hoverSpec.range.start.line), ELong(hoverSpec.range.start.column))),
            ETuple(List(ELong(hoverSpec.range.end.line), ELong(hoverSpec.range.end.column))),
          ),
        ),
        EAtom("hover"),
        EAtom("undefined"),
        EBinary(hoverSpec.spec),
      ),
    )
  }

}
