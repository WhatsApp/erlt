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

object Types {

  sealed trait Type
  // For use cases like spec id(Input :: atom()) -> atom.
  case class AnnotatedType(anno: String, tp: Type) extends Type
  case class AtomType(atom: String) extends Type
  // Interesting thing: types are non-empty in the whole OTP just once (inet)
  case class BitstringType(types: List[SingletonIntegerType]) extends Type
  case object EmptyListType extends Type
  sealed trait FunType extends Type
  // fun()
  case object FunTypeAny extends FunType
  case class FunTypeAnyArgs(tp: Type) extends FunType
  case class FunctionType(args: List[Type], resType: Type) extends FunType with FunSpecType
  case class IntegerRangeType(t1: SingletonIntegerType, t2: SingletonIntegerType) extends Type

  sealed trait MapType extends Type
  // map()
  case object AnyMap extends MapType
  case class AssocMap(assocs: List[AssocType]) extends MapType

  sealed trait AssocType {
    val types: List[Type]
  }
  // X := Y - mandatory association
  case class MapFieldExact(types: List[Type]) extends AssocType
  // X => Y - optional association
  case class MapFieldOpt(types: List[Type]) extends AssocType

  case class PredefinedType(name: String, params: List[Type]) extends Type
  case class RecordType(name: String, fieldTypes: List[RecordFieldType]) extends Type
  case class RemoteType(module: String, name: String, params: List[Type]) extends Type

  sealed trait SingletonIntegerType extends Type
  case class SinlgeInteger(int: Int) extends SingletonIntegerType
  case class SingleCharacter(char: Char) extends SingletonIntegerType
  case class UnaryOpIntegerType(op: String, arg: SingletonIntegerType) extends SingletonIntegerType
  case class BinaryOpIntegerType(op: String, arg1: SingletonIntegerType, arg2: SingletonIntegerType)
      extends SingletonIntegerType

  sealed trait TupleType extends Type
  case object TupleTypeAny extends TupleType
  case class TupleTypeTyped(params: List[Type]) extends TupleType

  case class UnionType(elems: List[Type]) extends Type
  case class TypeVariable(v: String) extends Type
  case class UserType(name: String, params: List[Type]) extends Type
  case class EnumCtr(ctr: String, types: List[Type]) extends Type

  case class RecordFieldType(name: String, tp: Type)

  sealed trait FunSpecType
  case class AF_ContrainedFunctionType(functionType: FunctionType, constraints: List[Constraint]) extends FunSpecType
  case class Constraint(tVar: String, tp: Type)

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