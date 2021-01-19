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

package com.whatsapp.eqwalizer.ast

object Types {
  sealed trait Type
  case class AtomLitType(atom: String) extends Type
  case class FunType(argTys: List[Type], resTy: Type) extends Type
  case class TupleType(argTys: List[Type]) extends Type
  case object NilType extends Type
  case class ListType(t: Type) extends Type
  case class UnionType(tys: List[Type]) extends Type
  case class LocalType(id: Id, args: List[Type]) extends Type
  case class RemoteType(id: RemoteId, args: List[Type]) extends Type
  case class VarType(name: String) extends Type

  sealed trait BuiltinType extends Type
  case object AnyType extends BuiltinType
  case object AtomType extends BuiltinType
  case object NoneType extends BuiltinType
  case object NumberType extends BuiltinType
  case object PidType extends BuiltinType
  case object PortType extends BuiltinType
  case object ReferenceType extends BuiltinType

  case class ConstrainedFunType(ty: FunType, constraints: List[Constraint])
  case class Constraint(tVar: String, ty: Type)

  val booleanType: Type =
    UnionType(List(AtomLitType("false"), AtomLitType("true")))

  val builtinTypes: Map[String, Type] =
    Map(
      "any" -> AnyType,
      "atom" -> AtomType,
      "boolean" -> booleanType,
      "byte" -> NumberType,
      "char" -> NumberType,
      "float" -> NumberType,
      "pos_integer" -> NumberType,
      "neg_integer" -> NumberType,
      "non_neg_integer" -> NumberType,
      "integer" -> NumberType,
      "none" -> NoneType,
      "number" -> NumberType,
      "pid" -> PidType,
      "port" -> PortType,
      "reference" -> ReferenceType,
      "term" -> AnyType,
      "arity" -> NumberType,
      "identifier" -> UnionType(List(PidType, PortType, ReferenceType)),
      "mfa" -> TupleType(List(AtomType, AtomType, NumberType)),
      "module" -> AtomType,
      "node" -> AtomType,
      "no_return" -> NoneType,
      "timeout" -> UnionType(List(AtomLitType("infinity"), NumberType)),
    )
}
