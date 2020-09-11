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

  sealed trait SpecAttr
  case object Spec extends SpecAttr
  case object Callback extends SpecAttr

  sealed trait Form

  case class Lang(lang: String) extends Form
  case class Module(name: String) extends Form
  case class Behaviour(name: String) extends Form
  case class Export(ids: List[IdWithArity]) extends Form
  case class Import(module: String, ids: List[IdWithArity]) extends Form
  case class ExportType(ids: List[IdWithArity]) extends Form
  case class ImportType(module: String, ids: List[IdWithArity]) extends Form
  case class Compile(options: ETerm) extends Form
  case class File(file: String) extends Form
  case class StructDecl(p: Pos.SP, name: String, fields: List[StructFieldDecl], kind: StructKind) extends Form
  case class TypeDecl(p: Pos.SP, typeAttr: TypeAttr, typeName: String, params: List[TypeVariable], abstractType: Type)
      extends Form
  case class FunctionSpec(p: Pos.SP, specAttr: SpecAttr, id: IdWithArity, types: List[FunSpecType]) extends Form
  case class FunctionDecl(p: Pos.SP, name: String, arity: Int, clauses: List[Clause]) extends Form
  case class Require(modules: List[String]) extends Form
  case object EOF extends Form
  case class WildAttribute(p: Pos.SP, name: String) extends Form
  case class Error(loc: Pos.Loc) extends Form

  sealed trait StructFieldDecl
  case class StructFieldUntyped(p: Pos.SP, name: String, initValue: Option[Exprs.Expr]) extends StructFieldDecl
  case class StructFieldTyped(p: Pos.SP, name: String, initValue: Option[Exprs.Expr], tp: Type) extends StructFieldDecl
}
