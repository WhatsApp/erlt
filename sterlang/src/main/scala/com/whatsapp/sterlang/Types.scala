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

object TyCons {
  sealed trait TyCon
  case class FunTyCon(arity: Int) extends TyCon
  case object RecordTyCon extends TyCon
  case class TupleCon(arity: Int) extends TyCon
  case class NamedTyCon(name: String) extends TyCon
  case class ERecordTyCon(name: String) extends TyCon
}

object Types {

  final type Depth = Int
  final type RtVarKind = Set[String]

  final type TypeVar = UnionFind.Var[TypeVarValue]
  final type RowTypeVar = UnionFind.Var[RowTypeVarValue]
  case class Field(label: String, value: Type)

  sealed trait Type
  case class VarType(tv: TypeVar) extends Type
  case class ConType(tyCon: TyCons.TyCon, ts: List[Type], rts: List[RowType]) extends Type

  // Row types
  sealed trait RowType
  case class RowVarType(rTyVar: RowTypeVar) extends RowType
  case object RowEmptyType extends RowType
  case class RowFieldType(f: Field, rt: RowType) extends RowType

  sealed trait TypeVarValue
  // Link of typ
  case class Instance(tp: ConType) extends TypeVarValue
  // Unbound
  case class Open(d: Depth) extends TypeVarValue

  sealed trait RowTypeVarValue
  case class RowInstance(rTyp: RowType) extends RowTypeVarValue
  case class RowOpen(d: Depth, kind: RtVarKind) extends RowTypeVarValue

  def sameTyc(t1: TyCons.TyCon, t2: TyCons.TyCon): Boolean =
    t1 == t2

  val empty: RtVarKind =
    Set()
  def single(label: String): RtVarKind =
    empty + label
}

object METypes {
  import Types._
  import TyCons._

  // Type construction
  val IntType: ConType =
    ConType(NamedTyCon("integer"), List(), List())
  val FloatType: ConType =
    ConType(NamedTyCon("float"), List(), List())
  val CharType: ConType =
    ConType(NamedTyCon("char"), List(), List())
  val BoolType: ConType =
    ConType(NamedTyCon("boolean"), List(), List())
  val StringType: ConType =
    ConType(NamedTyCon("string"), List(), List())
  val BinaryType: ConType =
    ConType(NamedTyCon("binary"), List(), List())
  val BitstringType: ConType =
    ConType(NamedTyCon("bitstring"), List(), List())
  val ExceptionType: ConType =
    ConType(NamedTyCon("exception"), List(), List())
  val MessageType: ConType =
    ConType(NamedTyCon("message"), List(), List())
  def FunType(argTypes: List[Type], resultType: Type): ConType =
    ConType(FunTyCon(argTypes.length), argTypes ++ List(resultType), List())
  def ListType(t: Type): ConType =
    ConType(NamedTyCon("list"), List(t), List())
  def RecordType(rt: RowType): ConType =
    ConType(RecordTyCon, List(), List(rt))
  val UnitType: ConType =
    TupleType(List())
  def TupleType(ts: List[Type]): ConType =
    ConType(TupleCon(ts.size), ts, List())
  def NamedType(name: String, ts: List[Type]): ConType =
    ConType(NamedTyCon(name), ts, Nil)
  def ERecordType(name: String): ConType =
    ConType(ERecordTyCon(name), Nil, Nil)
}

// Schematic types
object STypes {

  // bound type in type schemas
  case class TypeVar(id: Int)
  // bound row type variable in type schema
  case class RowTypeVar(id: Int)

  // Types used in type schemes
  sealed trait Type
  case class PlainType(typ: Types.Type) extends Type
  case class ConType(tyCon: TyCons.TyCon, typs: List[Type], rtys: List[RowType]) extends Type
  // "Substituted"/referenced var
  case class RefType(sTypeVar: TypeVar) extends Type

  // Row types used in type schemes
  sealed trait RowType
  case class RowVarType(rTyVar: Types.RowTypeVar) extends RowType
  case object RowEmptyType extends RowType
  case class RowFieldType(f: Field, rTyps: RowType) extends RowType
  case class RowRefType(sRowTypeVar: RowTypeVar) extends RowType

  case class Field(label: String, value: Type)

  // targs = number/size of type arguments, in this type scheme
  //         Basically for i < targs, - all Open(i) are generalizable
  case class TypeSchema(targs: Int, rargs: List[Types.RtVarKind], body: Type)

  def SFunType(argTypes: List[Type], resultType: Type): Type =
    ConType(TyCons.FunTyCon(argTypes.length), argTypes ++ List(resultType), List())
}

object MESTypes {
  import STypes._
  import TyCons._

  // Type construction
  val IntType: ConType =
    ConType(NamedTyCon("integer"), List(), List())
  val BoolType: ConType =
    ConType(NamedTyCon("boolean"), List(), List())
  val StringType: ConType =
    ConType(NamedTyCon("string"), List(), List())
  def FunType(argTypes: List[Type], resultType: Type): ConType =
    ConType(FunTyCon(argTypes.length), argTypes ++ List(resultType), List())
  def ListType(t: Type): ConType =
    ConType(NamedTyCon("list"), List(t), List())
  def RecordType(rt: RowType): ConType =
    ConType(RecordTyCon, List(), List(rt))
  val UnitType: ConType =
    TupleType(List())
  def TupleType(ts: List[Type]): ConType =
    ConType(TupleCon(ts.size), ts, List())
  def EnumType(name: String, ts: List[Type]): ConType =
    ConType(NamedTyCon(name), ts, Nil)
  def ERecordType(name: String): ConType =
    ConType(ERecordTyCon(name), List(), List())
}
