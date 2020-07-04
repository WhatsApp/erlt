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

import com.whatsapp.sterlang.etf.ETerm
import com.whatsapp.sterlang.forms.Exprs._

object Patterns {
  sealed trait Pattern
  case class LiteralPattern(literal: Literal) extends Pattern
  case class MatchPattern(pat: Pattern, arg: Pattern) extends Pattern
  case class VariablePattern(name: String) extends Pattern
  case class TuplePattern(elems: List[Pattern]) extends Pattern
  case object NilPattern extends Pattern
  case class ConsPattern(hd: Pattern, tl: Pattern) extends Pattern
  case class BinPattern(elems: List[BinElementPattern]) extends Pattern
  case class BinOpPattern(op: String, pat1: Pattern, pat2: Pattern) extends Pattern
  case class UnOpPattern(op: String, pat1: Pattern) extends Pattern
  case class RecordPattern(recordName: String, fields: List[RecordFieldPattern]) extends Pattern
  case class RecordIndexPattern(recordName: String, fieldName: String) extends Pattern
  case class MapPattern(assocs: List[(Pattern, Pattern)]) extends Pattern
  case class LocalEnumCtrPattern(enum: String, ctr: String, args: List[Pattern]) extends Pattern
  case class RemoteEnumCtrPattern(module: String, enum: String, ctr: String, args: List[Pattern]) extends Pattern

  // additional classes for patterns
  // TODO - proper size
  case class BinElementPattern(pat: Pattern, size: ETerm, typeSpecifiers: TypeSpecifiers)
  case class RecordFieldPattern(fieldName: String, pat: Pattern)
}
