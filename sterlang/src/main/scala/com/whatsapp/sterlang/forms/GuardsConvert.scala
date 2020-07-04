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

package com.whatsapp.sterlang.forms

import com.whatsapp.sterlang.etf._

import com.whatsapp.sterlang.forms.Guards._
import com.whatsapp.sterlang.forms.Exprs._

object GuardsConvert {
  def convertGuard(term: ETerm): Guard =
    term match {
      case EList(tests) =>
        Guard(tests.map(convertTest))
      case _ =>
        ???
    }

  def convertTest(term: ETerm): Test =
    term match {
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        TestVariable(name)
      case ETuple(List(EAtom("tuple"), _anno, EList(eTests))) =>
        TestTuple(eTests.map(convertTest))
      case ETuple(List(EAtom("nil"), _anno)) =>
        TestNil
      case ETuple(List(EAtom("cons"), _anno, eTest1, eTest2)) =>
        val hd = convertTest(eTest1)
        val tl = convertTest(eTest2)
        TestCons(hd, tl)
      case ETuple(List(EAtom("bin"), _anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertPatternBinElement)
        TestBin(binElements)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eTest1, eTest2)) =>
        TestBinaryOp(op, convertTest(eTest1), convertTest(eTest2))
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eTest1)) =>
        TestUnaryOp(op, convertTest(eTest1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldTests))) =>
        TestRecordCreation(recordName, eRecordFieldTests.map(convertRecordFieldPattern))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        TestRecordIndex(recordName, fieldName)
      case ETuple(List(EAtom("record_field"), _anno, eTest, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        TestRecordFieldAccess(convertTest(eTest), recordName, fieldName)
      case ETuple(List(EAtom("map"), _anno, EList(eAssocs))) =>
        TestMapCreate(eAssocs.map(ExprsConvert.convertAssoc))
      case ETuple(List(EAtom("map"), _anno, eExp, EList(eAssocs))) =>
        TestMapUpdate(ExprsConvert.convertExp(eExp), eAssocs.map(ExprsConvert.convertAssoc))
      case ETuple(
            List(EAtom("call"), _anno, ETuple(List(EAtom("remote"), _anno1, erlang, eFun)), EList(eTests))
          ) =>
        val Some(AtomLiteral("erlang")) = ExprsConvert.maybeLiteral(erlang)
        val Some(AtomLiteral(funName)) = ExprsConvert.maybeLiteral(eFun)
        TestGuardErlangCall(funName, eTests.map(convertTest))
      case ETuple(List(EAtom("call"), _anno, eFun, EList(eTests))) =>
        val Some(AtomLiteral(funName)) = ExprsConvert.maybeLiteral(eFun)
        TestCall(funName, eTests.map(convertTest))
      case _ =>
        ExprsConvert.maybeLiteral(term) match {
          case Some(literal) =>
            TestLiteral(literal)
          case None =>
            sys.error(s"cannot parse guard test: $term")
        }
    }

  def convertPatternBinElement(term: ETerm): TestBinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, eTest, eSize, eTypeSpecifiers)) =>
        TestBinElement(convertTest(eTest), eSize, ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertRecordFieldPattern(term: ETerm): RecordFieldTest =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val Some(AtomLiteral(name)) = ExprsConvert.maybeLiteral(eName)
        RecordFieldTest(name, convertTest(ePat))
    }
}
