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

import com.whatsapp.sterlang.Doc
import com.whatsapp.sterlang.forms.Exprs._

object Patterns {
  sealed trait Pattern { val r: Doc.Range }
  case class LiteralPattern(literal: Literal) extends Pattern { val r: Doc.Range = literal.r }
  case class MatchPattern(r: Doc.Range, pat: Pattern, arg: Pattern) extends Pattern
  case class PinnedVariablePattern(r: Doc.Range, name: String) extends Pattern
  case class VariablePattern(r: Doc.Range, name: String) extends Pattern
  case class TuplePattern(r: Doc.Range, elems: List[Pattern]) extends Pattern
  case class NilPattern(r: Doc.Range) extends Pattern
  case class ConsPattern(r: Doc.Range, hd: Pattern, tl: Pattern) extends Pattern
  case class BinPattern(r: Doc.Range, elems: List[BinElementPattern]) extends Pattern
  case class BinOpPattern(r: Doc.Range, op: String, pat1: Pattern, pat2: Pattern) extends Pattern
  case class UnOpPattern(r: Doc.Range, op: String, pat1: Pattern) extends Pattern
  case class LocalStructPattern(r: Doc.Range, structName: String, fields: List[FieldPattern]) extends Pattern
  case class RemoteStructPattern(r: Doc.Range, module: String, structName: String, fields: List[FieldPattern])
      extends Pattern
  case class ShapePattern(r: Doc.Range, elems: List[ShapeFieldPattern]) extends Pattern
  case class LocalEnumPattern(r: Doc.Range, enum: String, ctr: String, fields: List[FieldPattern]) extends Pattern
  case class RemoteEnumPattern(r: Doc.Range, module: String, enum: String, ctr: String, fields: List[FieldPattern])
      extends Pattern

  case class BinElementPattern(r: Doc.Range, pat: Pattern, size: Option[Expr], typeSpecifiers: TypeSpecifiers)
  case class ShapeFieldPattern(r: Doc.Range, key: Pattern, value: Pattern)

  sealed trait FieldPattern
  case class LblFieldPattern(r: Doc.Range, fieldName: String, value: Pattern) extends FieldPattern
  case class PosFieldPattern(r: Doc.Range, value: Pattern) extends FieldPattern
}
