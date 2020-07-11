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

import com.whatsapp.sterlang.forms.Exprs._
import com.whatsapp.sterlang.forms.Patterns._

object PatternsConvert {
  def convertPat(term: ETerm): Pattern =
    term match {
      case ETuple(List(EAtom("match"), _anno, ePat1, ePat2)) =>
        MatchPattern(convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        VariablePattern(sp(anno), name)
      case ETuple(List(EAtom("tuple"), anno, EList(ePats))) =>
        TuplePattern(sp(anno), ePats.map(convertPat))
      case ETuple(List(EAtom("nil"), anno)) =>
        NilPattern(sp(anno))
      case ETuple(List(EAtom("cons"), anno, ePat1, ePat2)) =>
        ConsPattern(sp(anno), convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("bin"), _anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertPatternBinElement)
        BinPattern(binElements)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), ePat1, ePat2)) =>
        BinOpPattern(op, convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("op"), _anno, EAtom(op), ePat1)) =>
        UnOpPattern(op, convertPat(ePat1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldPatterns))) =>
        RecordPattern(recordName, eRecordFieldPatterns.map(convertRecordFieldPattern))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AtomLiteral(_, fieldName)) = ExprsConvert.maybeLiteral(eFieldName)
        RecordIndexPattern(recordName, fieldName)
      case ETuple(List(EAtom("map"), _anno, EList(eAssocs))) =>
        MapPattern(eAssocs.map(convertAssocExact))
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              EList(eArgs),
            )
          ) =>
        LocalEnumCtrPattern(sp(anno), enum, ctr, eArgs.map(convertPat))
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
        RemoteEnumCtrPattern(sp(anno), module, enum, ctr, eArgs.map(convertPat))
      case _ =>
        ExprsConvert.maybeLiteral(term) match {
          case Some(literal) =>
            LiteralPattern(literal)
          case None =>
            sys.error(s"cannot parse pattern: $term")
        }
    }

  def convertPatternBinElement(term: ETerm): BinElementPattern =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, ePat, eSize, eTypeSpecifiers)) =>
        BinElementPattern(convertPat(ePat), eSize, ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertRecordFieldPattern(term: ETerm): RecordFieldPattern =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val Some(AtomLiteral(_, name)) = ExprsConvert.maybeLiteral(eName)
        RecordFieldPattern(name, convertPat(ePat))
    }

  def convertAssocExact(term: ETerm): (Pattern, Pattern) =
    term match {
      case ETuple(List(EAtom("map_field_exact"), _anno, e1, e2)) =>
        (convertPat(e1), convertPat(e2))
    }

}
