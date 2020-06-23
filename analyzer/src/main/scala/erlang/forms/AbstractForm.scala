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
import erlang.forms.AbstractExpr.AF_Clause
import erlang.forms.AbstractType.{AbstractType, FunSpecType}

object AbstractForm {
  // name/arity
  type IdWithArity = (String, Int)

  sealed trait TypeAttr
  case object Opaque extends TypeAttr
  case object Type extends TypeAttr

  sealed trait SpecAttr
  case object Spec extends SpecAttr
  case object Callback extends SpecAttr

  sealed trait AbstractForm

  case class AF_Module(name: String) extends AbstractForm
  case class AF_Behaviour(name: String) extends AbstractForm
  case class AF_Export(funs: List[IdWithArity]) extends AbstractForm
  case class AF_Import(module: String, funs: List[IdWithArity]) extends AbstractForm
  case class AF_ExportType(types: List[IdWithArity]) extends AbstractForm
  case class AF_Compile(options: EObject) extends AbstractForm
  case class AF_File(file: String) extends AbstractForm
  case class AF_RecordDecl(name: String, fields: List[AF_FieldDecl]) extends AbstractForm
  case class AF_TypeDecl(typeAttr: TypeAttr, typeName: String, abstractType: AbstractType, params: List[String])
      extends AbstractForm
  case class AF_FunctionSpec(specAttr: SpecAttr, id: IdWithArity, types: List[FunSpecType]) extends AbstractForm
  case class AF_WildAttribute(name: String, value: EObject) extends AbstractForm
  case class AF_FunctionDecl(name: String, arity: Int, clauses: List[AF_Clause]) extends AbstractForm
  case object AF_EOF extends AbstractForm

  sealed trait AF_FieldDecl
  case class AF_FieldUntyped(name: String) extends AF_FieldDecl
  case class AF_FieldTyped(name: String, tp: AbstractType) extends AF_FieldDecl

  def module(form: AbstractForm): Option[AF_Module] =
    form match {
      case module: AF_Module =>
        Some(module)
      case _ =>
        None
    }

  def functionDecl(form: AbstractForm): Option[AF_FunctionDecl] =
    form match {
      case functionDecl: AF_FunctionDecl =>
        Some(functionDecl)
      case _ =>
        None
    }

  def export(form: AbstractForm): Option[AF_Export] =
    form match {
      case export: AF_Export =>
        Some(export)
      case _ =>
        None
    }

  def functionSpec(form: AbstractForm): Option[AF_FunctionSpec] =
    form match {
      case functionSpec: AF_FunctionSpec =>
        Some(functionSpec)
      case _ =>
        None
    }

  def recordDecl(form: AbstractForm): Option[AF_RecordDecl] =
    form match {
      case recordDecl: AF_RecordDecl =>
        Some(recordDecl)
      case _ =>
        None
    }

  def typeDecl(form: AbstractForm): Option[AF_TypeDecl] =
    form match {
      case typeDecl: AF_TypeDecl =>
        Some(typeDecl)
      case _ =>
        None
    }
}
