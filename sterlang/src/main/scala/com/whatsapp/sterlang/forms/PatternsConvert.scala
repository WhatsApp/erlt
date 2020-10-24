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

import com.whatsapp.sterlang.forms.Patterns._

object PatternsConvert {
  def convertPat(term: ETerm): Pattern =
    term match {
      case ETuple(List(EAtom("match"), anno, ePat1, ePat2)) =>
        MatchPattern(r(anno), convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("var"), anno, EAtom(name))) =>
        VariablePattern(r(anno), name)
      case ETuple(List(EAtom("pinned_var"), anno, EAtom(name))) =>
        PinnedVariablePattern(r(anno), name)
      case ETuple(List(EAtom("tuple"), anno, EList(ePats))) =>
        TuplePattern(r(anno), ePats.map(convertPat))
      case ETuple(List(EAtom("nil"), anno)) =>
        NilPattern(r(anno))
      case ETuple(List(EAtom("cons"), anno, ePat1, ePat2)) =>
        ConsPattern(r(anno), convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("bin"), anno, EList(eBinElements))) =>
        val binElements = eBinElements.map(convertPatternBinElement)
        BinPattern(r(anno), binElements)
      case ETuple(List(EAtom("op"), anno, EAtom(op), ePat1, ePat2)) =>
        BinOpPattern(r(anno), op, convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("op"), anno, EAtom(op), ePat1)) =>
        UnOpPattern(r(anno), op, convertPat(ePat1))
      case ETuple(List(EAtom("struct"), anno, EAtom(structName), EList(eStructFieldPatterns))) =>
        LocalStructPattern(r(anno), structName, eStructFieldPatterns.map(fieldPattern))
      case ETuple(
            List(
              EAtom("struct"),
              anno,
              ETuple(
                List(
                  EAtom("remote"),
                  _,
                  ETuple(List(EAtom("atom"), _anno1, EAtom(module))),
                  ETuple(List(EAtom("atom"), _anno2, EAtom(structName))),
                )
              ),
              EList(eStructFieldPatterns),
            )
          ) =>
        RemoteStructPattern(r(anno), module, structName, eStructFieldPatterns.map(fieldPattern))
      case ETuple(List(EAtom("shape"), anno, EList(eAssocs))) =>
        ShapePattern(r(anno), eAssocs.map(convertShapeFieldPattern))
      case ETuple(
            List(
              EAtom("enum"),
              anno,
              ETuple(List(EAtom("atom"), _anno1, EAtom(enum))),
              ETuple(List(EAtom("atom"), _anno2, EAtom(ctr))),
              EList(eFields),
            )
          ) =>
        LocalEnumPattern(r(anno), enum, ctr, eFields.map(fieldPattern))
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
              EList(eFields),
            )
          ) =>
        RemoteEnumPattern(r(anno), module, enum, ctr, eFields.map(fieldPattern))
      case _ =>
        val literal = ExprsConvert.literal(term)
        LiteralPattern(literal)
    }

  def convertPatternBinElement(term: ETerm): BinElementPattern =
    term match {
      case ETuple(List(EAtom("bin_element"), anno, ePat, eSize, eTypeSpecifiers)) =>
        val size = eSize match {
          case EAtom("default") => None
          case other            => Some(ExprsConvert.convertExp(other))
        }
        BinElementPattern(
          r(anno),
          convertPat(ePat),
          size,
          ExprsConvert.convertTypeSpecifiers(eTypeSpecifiers),
        )
    }

  def fieldPattern(term: ETerm): FieldPattern =
    term match {
      case ETuple(List(EAtom("field"), anno, EAtom("undefined"), exp)) =>
        PosFieldPattern(r(anno), convertPat(exp))
      case ETuple(List(EAtom("field"), anno, ETuple(List(EAtom("atom"), _, EAtom(name))), exp)) =>
        LblFieldPattern(r(anno), name, convertPat(exp))
    }

  def convertShapeFieldPattern(term: ETerm): ShapeFieldPattern =
    term match {
      case ETuple(List(EAtom("shape_field"), anno, e1, e2)) =>
        ShapeFieldPattern(r(anno), convertPat(e1), convertPat(e2))
    }

}
