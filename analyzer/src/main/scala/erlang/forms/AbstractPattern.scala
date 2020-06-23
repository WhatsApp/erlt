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

import erlang.Data.EObject
import erlang.forms.AbstractExpr.{AF_Literal, TypeSpecifiers}

object AbstractPattern {
  sealed trait Pattern
  case class PatternLiteral(literal: AF_Literal) extends Pattern
  case class PatternMatch(pat: Pattern, arg: Pattern) extends Pattern
  case class PatternVariable(name: String) extends Pattern
  case class PatternTuple(elems: List[Pattern]) extends Pattern
  case object PatternNil extends Pattern
  case class PatternCons(hd: Pattern, tl: Pattern) extends Pattern
  case class PatternBin(elems: List[PatternBinElement]) extends Pattern
  case class PatternBinaryOp(op: String, pat1: Pattern, pat2: Pattern) extends Pattern
  case class PatternUnaryOp(op: String, pat1: Pattern) extends Pattern
  case class PatternRecordCreation(recordName: String, fields: List[RecordFieldPattern]) extends Pattern
  case class PatternRecordIndex(recordName: String, fieldName: String) extends Pattern
  case class MapPattern(assocs: List[(Pattern, Pattern)]) extends Pattern

  // additional classes for patterns
  // TODO - proper size
  case class PatternBinElement(pat: Pattern, size: EObject, typeSpecifiers: TypeSpecifiers)

  case class RecordFieldPattern(fieldName: String, pat: Pattern)
}
