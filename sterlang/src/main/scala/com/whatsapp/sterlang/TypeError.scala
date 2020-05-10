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

package com.whatsapp.sterlang

// Low-lever (encapsulate) unify error
class UnifyError(msg: String) extends Exception(msg)
case object Circularity extends UnifyError("Circularity")
case object RowCircularity extends UnifyError("RowCircularity")
case class FieldMismatch(label: String)
  extends UnifyError(s"Field mismatch: $label")
case class TyConMismatch(tc1: TyCons.TyCon, tc2: TyCons.TyCon)
  extends UnifyError(s"TyCon mismatch $tc1 <> $tc2")

class TypeError(msg: String) extends Exception(msg)

case class Cycle(aliasName: String)
  extends Exception(s"The type alias $aliasName is cyclic")

case class PositionedError(pos: Pos.P, title: String, description: Option[String]) extends Exception(title)
class InfiniteTypeError(pos: Pos.P, t1: String, t2: String)
  extends PositionedError(pos, s"Infinite type", Some(s"$t1 <> $t2"))
class TypeMismatchError(pos: Pos.P, required: String, found: String)
  extends PositionedError(pos, s"Type Mismatch", Some(s"Found:    $found\nRequired: $required"))
class UnboundVar(pos: Pos.P, val name: String)
  extends PositionedError(pos, s"Unbound variable: $name", None)
class UnknownEnum(pos: Pos.P, val enumName:String)
  extends PositionedError(pos, s"Unknown enum: $enumName", None)
class UnknownEnumCon(pos: Pos.P, val enumConName:String)
  extends PositionedError(pos, s"Unknown enum constructor: $enumConName", None)
class SpecError(pos: Pos.P, val fName: String, val specType: String, val elabType: String)
  extends PositionedError(pos, s"Spec mismatch for $fName", Some(s"Specified:  `$specType`\nElaborated: `$elabType`"))
class DuplicateFields(pos: Pos.P, names: List[String])
  extends PositionedError(pos, s"Duplicate fields: ${names.mkString(", ")}", None)
class CyclicTypeAlias(pos: Pos.P, aliasName: String)
  extends PositionedError(pos, s"Cyclic type alias: $aliasName", None)
class UnknownType(pos: Pos.P, name: String, arity: Int)
  extends PositionedError(pos, s"Unknown type: $name/$arity", None)
class DuplicateTypeVar(pos: Pos.P, v: String)
  extends PositionedError(pos, s"Duplicate type var: $v", None)
class UnboundTypeVariable(pos: Pos.P, name: String)
  extends PositionedError(pos, s"Unbound type variable: $name", None)
class UselessTypeVar(pos: Pos.P, name: String)
  extends PositionedError(pos, s"Useless type variable: $name", None)
class DuplicateEnumCon(pos: Pos.P, name: String)
  extends PositionedError(pos, s"Duplicate enum constructor: $name", None)
class DuplicateType(pos: Pos.P, name: String)
  extends PositionedError(pos, s"Duplicate type: $name", None)
class DuplicateFun(pos: Pos.P, name: String)
  extends PositionedError(pos, s"Duplicate fun: $name", None)
