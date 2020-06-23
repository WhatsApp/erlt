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

object AbstractType {

  sealed trait AbstractType
  // For use cases like spec id(Input :: atom()) -> atom.
  case class AF_AnnotatedType(anno: String, tp: AbstractType) extends AbstractType
  case class AF_AtomType(atom: String) extends AbstractType
  // Interesting thing: types are non-empty in the whole OTP just once (inet)
  case class AF_BitstringType(types: List[AF_SingletonIntegerType]) extends AbstractType
  case object AF_EmptyListType extends AbstractType
  sealed trait AF_FunType extends AbstractType
  case object AF_FunTypeAny extends AF_FunType
  case class AF_FunTypeAnyArgs(tp: AbstractType) extends AF_FunType
  case class AF_FunctionType(args: List[AbstractType], resType: AbstractType) extends AF_FunType with FunSpecType
  case class AF_IntegerRangeType(t1: AF_SingletonIntegerType, t2: AF_SingletonIntegerType) extends AbstractType

  sealed trait AF_MapType extends AbstractType
  case object AF_AnyMap extends AF_MapType
  case class AF_AssocMap(assocs: List[AF_AssocType]) extends AF_MapType

  sealed trait AF_AssocType {
    val types: List[AbstractType]
  }
  // X := Y - mandatory association
  case class MapFieldExact(types: List[AbstractType]) extends AF_AssocType
  // X => Y - optional association
  case class MapFieldAssoc(types: List[AbstractType]) extends AF_AssocType

  case class AF_PredefinedType(typeName: String, params: List[AbstractType]) extends AbstractType
  case class AF_RecordType(recordName: String, fieldTypes: List[AF_RecordFieldType]) extends AbstractType
  case class AF_RemoteType(module: String, typeName: String, params: List[AbstractType]) extends AbstractType

  sealed trait AF_SingletonIntegerType extends AbstractType
  case class AF_Integer(int: Int) extends AF_SingletonIntegerType
  case class AF_Character(char: Char) extends AF_SingletonIntegerType
  case class AF_UnaryOp(op: String, arg: AF_SingletonIntegerType) extends AF_SingletonIntegerType
  case class AF_BinaryOp(op: String, arg1: AF_SingletonIntegerType, arg2: AF_SingletonIntegerType)
      extends AF_SingletonIntegerType

  sealed trait AF_TupleType extends AbstractType
  case object AF_TupleTypeAny extends AF_TupleType
  case class AF_TupleTypeTyped(params: List[AbstractType]) extends AF_TupleType

  case class AF_TypeUnion(params: List[AbstractType]) extends AbstractType
  case class AF_TypeVariable(v: String) extends AbstractType
  case class AF_UserDefinedType(typeName: String, types: List[AbstractType]) extends AbstractType

  case class AF_RecordFieldType(name: String, tp: AbstractType)

  sealed trait FunSpecType
  case class AF_ContrainedFunctionType(functionType: AF_FunctionType, constraints: List[Constraint]) extends FunSpecType
  case class Constraint(tVar: String, tp: AbstractType)

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
