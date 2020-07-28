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
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        GVariable(sp(anno), name)
      case ETuple(List(EAtom("tuple"), anno, EList(eTests))) =>
        GTuple(sp(anno), eTests.map(convertGExpr))
      case ETuple(List(EAtom("nil"), anno)) =>
        GNil(sp(anno))
      case ETuple(List(EAtom("cons"), anno, eTest1, eTest2)) =>
        val hd = convertGExpr(eTest1)
        val tl = convertGExpr(eTest2)
        GCons(sp(anno), hd, tl)
      case ETuple(List(EAtom("bin"), anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertGBinElement)
        GBin(sp(anno), binElements)
      case ETuple(List(EAtom("op"), anno, EAtom(op), eTest1, eTest2)) =>
        GBinaryOp(sp(anno), op, convertGExpr(eTest1), convertGExpr(eTest2))
      case ETuple(List(EAtom("op"), anno, EAtom(op), eTest1)) =>
        GUnaryOp(sp(anno), op, convertGExpr(eTest1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldTests))) =>
        GRecordCreate(recordName, eRecordFieldTests.map(convertGRecordField))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(_, fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        GRecordIndex(recordName, fieldName)
      case ETuple(List(EAtom("record_field"), _anno, eTest, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(_, fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        GRecordFieldAccess(convertGExpr(eTest), recordName, fieldName)
      case ETuple(List(EAtom("map"), anno, EList(eAssocs))) =>
        GMapCreate(sp(anno), eAssocs.map(convertGAssoc))
      case ETuple(List(EAtom("map"), anno, eExp, EList(eAssocs))) =>
        GMapUpdate(sp(anno), convertGExpr(eExp), eAssocs.map(convertGAssoc))
      case ETuple(
            List(EAtom("call"), anno, ETuple(List(EAtom("remote"), _anno1, erlang, eFun)), EList(eTests))
          ) =>
        val Some(AtomLiteral(p1, "erlang")) = ExprsConvert.maybeLiteral(erlang)
        val Some(AtomLiteral(p2, funName)) = ExprsConvert.maybeLiteral(eFun)
        GCall(sp(anno), (p1 ! p2, funName), eTests.map(convertGExpr))
      case ETuple(List(EAtom("call"), anno, eFun, EList(eTests))) =>
        val Some(AtomLiteral(p1, funName)) = ExprsConvert.maybeLiteral(eFun)
        GCall(sp(anno), (p1, funName), eTests.map(convertGExpr))
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        GLocalEnumCtr(sp(anno), enum, ctr, eArgs.map(convertGExpr))
      case ETuple(
            List(
              EAtom("enum"),
              anno,
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
        GRemoteEnumCtr(sp(anno), module, enum, ctr, eArgs.map(convertGExpr))
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
      case ETuple(List(EAtom("map_field_assoc"), anno, eExp1, eExp2)) =>
        GAssocOpt(sp(anno), convertGExpr(eExp1), convertGExpr(eExp2))
      case ETuple(List(EAtom("map_field_exact"), anno, eExp1, eExp2)) =>
        GAssocExact(sp(anno), convertGExpr(eExp1), convertGExpr(eExp2))
    }

  def convertGBinElement(term: ETerm): GBinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, eExpr, eSize, eTypeSpecifiers)) =>
        val size = eSize match {
          case EAtom("default") => None
          case other            => Some(convertGExpr(other))
        }
        GBinElement(convertGExpr(eExpr), size, ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertGRecordField(term: ETerm): GRecordField =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val Some(AtomLiteral(_, name)) = ExprsConvert.maybeLiteral(eName)
        GRecordField(name, convertGExpr(ePat))
    }
}
