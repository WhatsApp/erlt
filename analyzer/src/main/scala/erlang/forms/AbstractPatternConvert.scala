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

package erlang.forms

import erlang.Data._
import erlang.forms.AbstractExpr.AF_LiteralAtom
import erlang.forms.AbstractPattern._

object AbstractPatternConvert {
  def convertPat(term: EObject): Pattern =
    term match {
      case ETuple(List(EAtom("match"), _anno, ePat1, ePat2)) =>
        PatternMatch(convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("var"), _anno, EAtom(name))) =>
        PatternVariable(name)
      case ETuple(List(EAtom("tuple"), _anno, EList(ePats, None))) =>
        PatternTuple(ePats.map(convertPat))
      case ETuple(List(EAtom("nil"), _anno)) =>
        PatternNil
      case ETuple(List(EAtom("cons"), _anno, ePat1, ePat2)) =>
        PatternCons(convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("bin"), _anno, EList(eBinElements, None))) =>
        val binElements = eBinElements.map(convertPatternBinElement)
        PatternBin(binElements)
      case ETuple(List(EAtom("op"), _anno, EAtom(op), ePat1, ePat2)) =>
        PatternBinaryOp(op, convertPat(ePat1), convertPat(ePat2))
      case ETuple(List(EAtom("op"), _anno, EAtom(op), ePat1)) =>
        PatternUnaryOp(op, convertPat(ePat1))
      case ETuple(List(EAtom("record"), _anno, EAtom(recordName), EList(eRecordFieldPatterns, None))) =>
        PatternRecordCreation(recordName, eRecordFieldPatterns.map(convertRecordFieldPattern))
      case ETuple(List(EAtom("record_index"), _anno, EAtom(recordName), eFieldName)) =>
        val Some(AF_LiteralAtom(fieldName)) = AbstractExprConvert.maybeLiteral(eFieldName)
        PatternRecordIndex(recordName, fieldName)
      case ETuple(List(EAtom("map"), _anno, EList(eAssocs, None))) =>
        MapPattern(eAssocs.map(convertAssocExact))
      case _ =>
        AbstractExprConvert.maybeLiteral(term) match {
          case Some(literal) =>
            PatternLiteral(literal)
          case None =>
            sys.error(s"cannot parse pattern: $term")
        }
    }

  def convertPatternBinElement(term: EObject): PatternBinElement =
    term match {
      case ETuple(List(EAtom("bin_element"), _anno, ePat, eSize, eTypeSpecifiers)) =>
        PatternBinElement(convertPat(ePat), eSize, AbstractExprConvert.convertTypeSpecifiers(eTypeSpecifiers))
    }

  def convertRecordFieldPattern(term: EObject): RecordFieldPattern =
    term match {
      case ETuple(List(EAtom("record_field"), _anno, eName, ePat)) =>
        val Some(AF_LiteralAtom(name)) = AbstractExprConvert.maybeLiteral(eName)
        RecordFieldPattern(name, convertPat(ePat))
    }

  def convertAssocExact(term: EObject): (Pattern, Pattern) =
    term match {
      case ETuple(List(EAtom("map_field_exact"), _anno, e1, e2)) =>
        (convertPat(e1), convertPat(e2))
    }

}
