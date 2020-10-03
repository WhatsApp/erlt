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
import com.whatsapp.sterlang.etf.ETerm
import com.whatsapp.sterlang.forms.Types._
import com.whatsapp.sterlang.forms.Exprs._

object Forms {
  sealed trait StructKind
  case object StrStruct extends StructKind
  case object ExnStruct extends StructKind
  case object MsgStruct extends StructKind

  // name/arity
  type IdWithArity = (String, Int)

  sealed trait TypeAttr
  case object Enum extends TypeAttr
  case object Type extends TypeAttr
  case object Opaque extends TypeAttr

  sealed trait Form

  case class Lang(lang: String) extends Form
  case class Module(name: String) extends Form
  case class Export(ids: List[IdWithArity]) extends Form
  case class Import(module: String, ids: List[IdWithArity]) extends Form
  case class ExportType(ids: List[IdWithArity]) extends Form
  case class ImportType(module: String, ids: List[IdWithArity]) extends Form
  case class StructDecl(
      r: Doc.Range,
      name: String,
      params: List[TypeVariable],
      fields: List[StructFieldDecl],
      kind: StructKind,
  ) extends Form
  case class TypeDecl(
      r: Doc.Range,
      typeAttr: TypeAttr,
      typeName: String,
      params: List[TypeVariable],
      abstractType: Type,
  ) extends Form
  case class FunctionSpec(r: Doc.Range, id: IdWithArity, types: List[FunType]) extends Form
  case class FunctionDecl(r: Doc.Range, name: String, arity: Int, clauses: List[Clause]) extends Form
  case object EOF extends Form
  case class Error(pos: Doc.Pos) extends Form

  case class StructFieldDecl(r: Doc.Range, name: String, initValue: Option[Exprs.Expr], tp: Type)
}
