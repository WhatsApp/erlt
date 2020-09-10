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

import com.whatsapp.sterlang.Pos
import com.whatsapp.sterlang.forms.Exprs._

object Patterns {
  sealed trait Pattern { val p: Pos.SP }
  case class LiteralPattern(literal: Literal) extends Pattern { val p: Pos.SP = literal.p }
  case class MatchPattern(p: Pos.SP, pat: Pattern, arg: Pattern) extends Pattern
  case class VariablePattern(p: Pos.SP, name: String) extends Pattern
  case class TuplePattern(p: Pos.SP, elems: List[Pattern]) extends Pattern
  case class NilPattern(p: Pos.SP) extends Pattern
  case class ConsPattern(p: Pos.SP, hd: Pattern, tl: Pattern) extends Pattern
  case class BinPattern(p: Pos.SP, elems: List[BinElementPattern]) extends Pattern
  case class BinOpPattern(p: Pos.SP, op: String, pat1: Pattern, pat2: Pattern) extends Pattern
  case class UnOpPattern(p: Pos.SP, op: String, pat1: Pattern) extends Pattern
  case class StructPattern(p: Pos.SP, structName: String, fields: List[StructFieldPattern]) extends Pattern
  case class MapPattern(p: Pos.SP, elems: List[MapFieldPattern]) extends Pattern
  case class LocalEnumCtrPattern(p: Pos.SP, enum: String, ctr: String, args: List[Pattern]) extends Pattern
  case class RemoteEnumCtrPattern(p: Pos.SP, module: String, enum: String, ctr: String, args: List[Pattern])
      extends Pattern

  case class BinElementPattern(p: Pos.SP, pat: Pattern, size: Option[Expr], typeSpecifiers: TypeSpecifiers)
  case class StructFieldPattern(p: Pos.SP, fieldName: String, pat: Pattern)
  case class MapFieldPattern(p: Pos.SP, key: Pattern, value: Pattern)
}
