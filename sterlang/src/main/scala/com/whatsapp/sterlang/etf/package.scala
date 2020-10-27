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
import com.whatsapp.sterlang.forms.{FormsConvertDev, FormsConvertErlt}

package object etf {

  sealed trait ETerm
  case class EAtom(atom: String) extends ETerm
  case class EDouble(d: Double) extends ETerm
  case class EList(elems: List[ETerm]) extends ETerm
  case class ELong(value: BigInt) extends ETerm
  case class EString(str: String) extends ETerm
  case class ETuple(elems: List[ETerm]) extends ETerm

  def programFromFileDev(path: String): Ast.Program = {
    val etf = etfFromFileDev(path)
    val forms = FormsConvertDev.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  def programFromFileErlt(path: String): Ast.Program = {
    val etf = etfFromFileErlt(path)
    val forms = FormsConvertErlt.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  private def etfFromFileErlt(file: String): ETerm = {
    val etfPath = {
      val module = Paths.get(file).getFileName.toString.dropRight(5)
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./erltc -o $oDirPath +etf $file".!!
      Paths.get(s"$oDirPath/$module.etf")
    }
    readEtf(etfPath)
  }

  private def etfFromFileDev(path: String): ETerm = {
    val etfPath =
      if (path.endsWith(".etf")) {
        Paths.get(path)
      } else {
        val tmp = Files.createTempFile("sterlang", ".etf")
        s"./parser -ifile $path -ofile $tmp".!!
        tmp
      }
    readEtf(etfPath)
  }

  def programFromString(text: String): Ast.Program = {
    val etf = etfFromString(text)
    val forms = FormsConvertDev.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  private def etfFromString(text: String): ETerm = {
    val tmpErlT = Files.createTempFile("sterlang", ".erlt")
    Files.write(tmpErlT, text.getBytes)
    val tmpEtf = Files.createTempFile("sterlang", ".etf")
    s"./parser -ifile $tmpErlT -ofile $tmpEtf".!!
    readEtf(tmpEtf)
  }

  private def readEtf(etfPath: Path): ETerm = {
    val etfBytes = Files.readAllBytes(etfPath)
    val otpObject = new OtpInputStream(etfBytes).read_any()
    val eTerm = fromJava(otpObject)
    eTerm
  }

  private def fromJava(jObject: OtpErlangObject): ETerm =
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
    }
}
