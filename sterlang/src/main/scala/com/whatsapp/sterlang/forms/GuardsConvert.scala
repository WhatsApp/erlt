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
        Guard(tests.map(convertGExpr))
      case _ =>
        ???
    }

  def convertGExpr(term: ETerm): GExpr =
    term match {
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        GVariable(name)
      case ETuple(List(EAtom("tuple"), _anno, EList(eTests))) =>
        GTuple(eTests.map(convertGExpr))
      case ETuple(List(EAtom("nil"), _anno)) =>
        GNil
      case ETuple(List(EAtom("cons"), _anno, eTest1, eTest2)) =>
        val hd = convertGExpr(eTest1)
        val tl = convertGExpr(eTest2)
        GCons(hd, tl)
      case ETuple(List(EAtom("bin"), _anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertGBinElement)
        GBin(binElements)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eTest1, eTest2)) =>
        GBinaryOp(op, convertGExpr(eTest1), convertGExpr(eTest2))
      case ETuple(List(EAtom("op"), _anno, EAtom(op), eTest1)) =>
        GUnaryOp(op, convertGExpr(eTest1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldTests))) =>
        GRecordCreate(recordName, eRecordFieldTests.map(convertGRecordField))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        GRecordIndex(recordName, fieldName)
      case ETuple(List(EAtom("record_field"), _anno, eTest, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        GRecordFieldAccess(convertGExpr(eTest), recordName, fieldName)
      case ETuple(List(EAtom("map"), _anno, EList(eAssocs))) =>
        GMapCreate(eAssocs.map(convertGAssoc))
      case ETuple(List(EAtom("map"), _anno, eExp, EList(eAssocs))) =>
        GMapUpdate(convertGExpr(eExp), eAssocs.map(convertGAssoc))
      case ETuple(
            List(EAtom("call"), _anno, ETuple(List(EAtom("remote"), _anno1, erlang, eFun)), EList(eTests))
          ) =>
        val Some(AtomLiteral("erlang")) = ExprsConvert.maybeLiteral(erlang)
        val Some(AtomLiteral(funName)) = ExprsConvert.maybeLiteral(eFun)
        GCall(funName, eTests.map(convertGExpr))
      case ETuple(List(EAtom("call"), _anno, eFun, EList(eTests))) =>
        val Some(AtomLiteral(funName)) = ExprsConvert.maybeLiteral(eFun)
        GCall(funName, eTests.map(convertGExpr))
      case ETuple(
            List(
              EAtom("enum"),
              _anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        GLocalEnumCtr(enum, ctr, eArgs.map(convertGExpr))
      case ETuple(
            List(
              EAtom("enum"),
              _anno,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(enum))),
                )
              ),
              ETuple(List(EAtom("atom"), _anno3, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        GRemoteEnumCtr(module, enum, ctr, eArgs.map(convertGExpr))
      case _ =>
        ExprsConvert.maybeLiteral(term) match {
          case Some(literal) =>
            GLiteral(literal)
          case None =>
            sys.error(s"cannot parse guard test: $term")
        }
    }

  def convertGAssoc(term: ETerm): GAssoc =
    term match {
      case ETuple(List(EAtom("map_field_assoc"), _anno, eExp1, eExp2)) =>
        GAssocOpt(convertGExpr(eExp1), convertGExpr(eExp2))
      case ETuple(List(EAtom("map_field_exact"), _anno, eExp1, eExp2)) =>
        GAssocExact(convertGExpr(eExp1), convertGExpr(eExp2))
    }

  def convertGBinElement(term: ETerm): GBinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, eTest, eSize, eTypeSpecifiers)) =>
        GBinElement(convertGExpr(eTest), eSize, ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertGRecordField(term: ETerm): GRecordField =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val Some(AtomLiteral(name)) = ExprsConvert.maybeLiteral(eName)
        GRecordField(name, convertGExpr(ePat))
    }
}
