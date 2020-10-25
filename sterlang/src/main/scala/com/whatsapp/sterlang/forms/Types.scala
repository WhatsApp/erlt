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

object Types {

  sealed trait Type { val r: Doc.Range }
  // For use cases like spec id(Input :: atom()) -> atom.
  case class AnnotatedType(r: Doc.Range, tv: TypeVariable, tp: Type) extends Type
  // Interesting thing: types are non-empty in the whole OTP just once (inet)
  case class BitstringType(r: Doc.Range) extends Type
  case class FunType(r: Doc.Range, args: List[Type], resType: Type) extends Type
  case class AnyMap(r: Doc.Range) extends Type

  case class Shape(r: Doc.Range, assocs: List[ShapeField]) extends Type
  case class OpenShape(r: Doc.Range, assocs: List[ShapeField], restType: TypeVariable) extends Type
  case class ShapeField(r: Doc.Range, keyType: String, valueType: Type)

  case class PredefinedType(r: Doc.Range, name: String, params: List[Type]) extends Type
  case class RemoteType(r: Doc.Range, module: String, name: String, params: List[Type]) extends Type

  sealed trait TupleType extends Type
  case class TupleTypeAny(r: Doc.Range) extends TupleType
  case class TupleTypeTyped(r: Doc.Range, params: List[Type]) extends TupleType

  case class TypeVariable(r: Doc.Range, v: String) extends Type
  case class UserType(r: Doc.Range, name: String, params: List[Type]) extends Type

  case class StructFieldType(name: String, tp: Type)

  val predefinedTypes = Set(
    "any",
    "arity",
    "atom",
    "bitstring",
    "boolean",
    "byte",
    "char",
    "float",
    "function",
    "identifier",
    "integer",
    "iodata",
    "iolist",
    "list",
    "maybe_improper_list",
    "mfa",
    "module",
    "neg_integer",
    "no_return",
    "node",
    "non_neg_integer",
    "none",
    "nonempty_improper_list",
    "nonempty_list",
    "nonempty_maybe_improper_list",
    "nonempty_string",
    "number",
    "pid",
    "port",
    "pos_integer",
    "reference",
    "string",
    "term",
    "timeout",
  )
}
