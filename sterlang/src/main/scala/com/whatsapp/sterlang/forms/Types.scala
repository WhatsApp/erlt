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
  case class AtomType(r: Doc.Range, atom: String) extends Type
  // Interesting thing: types are non-empty in the whole OTP just once (inet)
  case class BitstringType(r: Doc.Range, types: List[SingletonIntegerType]) extends Type
  case class EmptyListType(r: Doc.Range) extends Type
  sealed trait FunType extends Type
  // fun()
  case class FunTypeAny(r: Doc.Range) extends FunType
  case class FunTypeAnyArgs(r: Doc.Range, tp: Type) extends FunType
  case class FunctionType(r: Doc.Range, args: List[Type], resType: Type) extends FunType with FunSpecType
  case class IntegerRangeType(r: Doc.Range, t1: SingletonIntegerType, t2: SingletonIntegerType) extends Type

  sealed trait MapType extends Type
  // map()
  case class AnyMap(r: Doc.Range) extends MapType
  case class AssocMap(r: Doc.Range, assocs: List[Assoc]) extends MapType
  case class OpenAssocMap(r: Doc.Range, assocs: List[Assoc], restType: Type) extends MapType

  case class Assoc(r: Doc.Range, keyType: Type, valueType: Type)

  case class PredefinedType(r: Doc.Range, name: String, params: List[Type]) extends Type
  case class StructType(r: Doc.Range, name: String, fieldTypes: List[StructFieldType]) extends Type
  case class RemoteType(r: Doc.Range, module: String, name: String, params: List[Type]) extends Type

  sealed trait SingletonIntegerType extends Type
  case class SinlgeInteger(r: Doc.Range, int: Int) extends SingletonIntegerType
  case class SingleCharacter(r: Doc.Range, char: Char) extends SingletonIntegerType
  case class UnaryOpIntegerType(r: Doc.Range, op: String, arg: SingletonIntegerType) extends SingletonIntegerType
  case class BinaryOpIntegerType(r: Doc.Range, op: String, arg1: SingletonIntegerType, arg2: SingletonIntegerType)
      extends SingletonIntegerType

  sealed trait TupleType extends Type
  case class TupleTypeAny(r: Doc.Range) extends TupleType
  case class TupleTypeTyped(r: Doc.Range, params: List[Type]) extends TupleType

  case class UnionType(r: Doc.Range, elems: List[Type]) extends Type
  case class TypeVariable(r: Doc.Range, v: String) extends Type
  case class UserType(r: Doc.Range, name: String, params: List[Type]) extends Type
  case class EnumCtr(r: Doc.Range, ctr: String, types: List[Type]) extends Type

  case class StructFieldType(name: String, tp: Type)

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
