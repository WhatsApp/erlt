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

object Types {

  sealed trait Type { val p: Pos.P }
  // For use cases like spec id(Input :: atom()) -> atom.
  case class AnnotatedType(p: Pos.SP, tv: TypeVariable, tp: Type) extends Type
  case class AtomType(p: Pos.SP, atom: String) extends Type
  // Interesting thing: types are non-empty in the whole OTP just once (inet)
  case class BitstringType(p: Pos.SP, types: List[SingletonIntegerType]) extends Type
  case class EmptyListType(p: Pos.SP) extends Type
  sealed trait FunType extends Type
  // fun()
  case class FunTypeAny(p: Pos.SP) extends FunType
  case class FunTypeAnyArgs(p: Pos.SP, tp: Type) extends FunType
  case class FunctionType(p: Pos.SP, args: List[Type], resType: Type) extends FunType with FunSpecType
  case class IntegerRangeType(p: Pos.SP, t1: SingletonIntegerType, t2: SingletonIntegerType) extends Type

  sealed trait MapType extends Type
  // map()
  case class AnyMap(p: Pos.SP) extends MapType
  case class AssocMap(p: Pos.SP, assocs: List[Assoc]) extends MapType

  case class Assoc(p: Pos.P, kind: AssocKind, keyType: Type, valueType: Type)
  sealed trait AssocKind
  // X := Y - required association
  case object ReqAssoc extends AssocKind
  // X => Y - optional association
  case object OptAssoc extends AssocKind

  case class PredefinedType(p: Pos.SP, name: String, params: List[Type]) extends Type
  case class RecordType(p: Pos.SP, name: String, fieldTypes: List[RecordFieldType]) extends Type
  case class RemoteType(p: Pos.SP, module: String, name: String, params: List[Type]) extends Type

  sealed trait SingletonIntegerType extends Type
  case class SinlgeInteger(p: Pos.SP, int: Int) extends SingletonIntegerType
  case class SingleCharacter(p: Pos.SP, char: Char) extends SingletonIntegerType
  case class UnaryOpIntegerType(p: Pos.SP, op: String, arg: SingletonIntegerType) extends SingletonIntegerType
  case class BinaryOpIntegerType(p: Pos.SP, op: String, arg1: SingletonIntegerType, arg2: SingletonIntegerType)
      extends SingletonIntegerType

  sealed trait TupleType extends Type
  case class TupleTypeAny(p: Pos.SP) extends TupleType
  case class TupleTypeTyped(p: Pos.SP, params: List[Type]) extends TupleType

  case class UnionType(p: Pos.SP, elems: List[Type]) extends Type
  case class TypeVariable(p: Pos.SP, v: String) extends Type
  case class UserType(p: Pos.SP, name: String, params: List[Type]) extends Type
  case class EnumCtr(p: Pos.SP, ctr: String, types: List[Type]) extends Type

  case class RecordFieldType(name: String, tp: Type)

  sealed trait FunSpecType
  case class AF_ContrainedFunctionType(functionType: FunctionType, constraints: List[Constraint]) extends FunSpecType
  case class Constraint(tVar: TypeVariable, tp: Type)

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
