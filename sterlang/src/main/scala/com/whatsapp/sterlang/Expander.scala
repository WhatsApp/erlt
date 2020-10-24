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

object Expander {
  type SubVal = Either[Types.Type, Types.RowType]
  type Sub = Map[String, SubVal]

  type SSubVal = Either[STypes.Type, STypes.RowType]
  type SSub = Map[String, SSubVal]
}

class Expander(
    val aliases: List[Ast.TypeAlias],
    val tGen: () => Types.Type,
    val rtGen: Set[String] => Types.RowTypeVar,
) {
  import Expander._

  val T = Types
  val MT = METypes
  val ST = STypes
  val MST = MESTypes

  def mkType(t: Ast.Type, sub: Sub): T.Type =
    t match {
      case Ast.TypeVar(n) =>
        val Left(t) = sub(n)
        t
      case Ast.WildTypeVar() =>
        tGen()
      case Ast.UserType(name, params) =>
        MT.NamedType(name.stringId, params.map(mkType(_, sub)))
      case Ast.TupleType(params) =>
        MT.TupleType(params.map(mkType(_, sub)))
      case Ast.ShapeType(fields) =>
        val tFields = fields.map { case Ast.LblField(n, tp) => T.Field(n, mkType(tp, sub)) }
        val zero: T.RowType = T.RowEmptyType
        val rowType = tFields.foldRight(zero)(T.RowFieldType)
        MT.ShapeType(rowType)
      case Ast.OpenShapeType(fields, extType) =>
        val lbls = fields.map(_.label).toSet
        val tFields = fields.map { case Ast.LblField(n, tp) => T.Field(n, mkType(tp, sub)) }
        val zero = extType match {
          case Left(Ast.WildTypeVar()) =>
            T.RowVarType(rtGen(lbls))
          case Right(Ast.TypeVar(n)) =>
            val Right(rt) = sub(n)
            rt
        }
        val rowType = tFields.foldRight(zero)(T.RowFieldType)
        MT.ShapeType(rowType)
      case Ast.FunType(params, res) =>
        MT.FunType(params.map(mkType(_, sub)), mkType(res, sub))
      case Ast.ListType(elemType) =>
        MT.ListType(mkType(elemType, sub))
    }

  def mkSType(t: Ast.Type, sub: SSub): ST.Type =
    t match {
      case Ast.TypeVar(n) =>
        val Left(t) = sub(n)
        t
      case Ast.WildTypeVar() =>
        ST.PlainType(tGen())
      case Ast.UserType(name, params) =>
        MST.EnumType(name.stringId, params.map(mkSType(_, sub)))
      case Ast.TupleType(params) =>
        MST.TupleType(params.map(mkSType(_, sub)))
      case Ast.ShapeType(fields) =>
        val tFields = fields.map { case Ast.LblField(n, tp) => ST.Field(n, mkSType(tp, sub)) }
        val zero: ST.RowType = ST.RowEmptyType
        val rowType = tFields.foldRight(zero)(ST.RowFieldType)
        MST.RecordType(rowType)
      case Ast.OpenShapeType(fields, extType) =>
        val lbls = fields.map(_.label).toSet
        val tFields = fields.map { case Ast.LblField(n, tp) => ST.Field(n, mkSType(tp, sub)) }
        val zero = extType match {
          case Left(Ast.WildTypeVar()) =>
            ST.RowVarType(rtGen(lbls))
          case Right(Ast.TypeVar(n)) =>
            val Right(rt) = sub(n)
            rt
        }
        val rowType = tFields.foldRight(zero)(ST.RowFieldType)
        MST.RecordType(rowType)
      case Ast.FunType(params, res) =>
        MST.FunType(params.map(mkSType(_, sub)), mkSType(res, sub))
      case Ast.ListType(elemType) =>
        MST.ListType(mkSType(elemType, sub))
    }

  def expandSType(ts: ST.Type): ST.Type =
    ts match {
      case ST.PlainType(typ) =>
        ST.PlainType(expandType(typ))
      case ST.ConType(TyCons.NamedTyCon(name), typs, List()) if aliases.exists(_.name == name) =>
        val alias = aliases.find(_.name == name).get
        val expandedParams: List[SSubVal] =
          typs.map(expandSType).map(Left(_))
        val t1 = mkSType(alias.body, alias.params.map(_.name).zip(expandedParams).toMap)
        expandSType(t1)
      case ST.ConType(tyCon, typs, rtys) =>
        ST.ConType(tyCon, typs.map(expandSType), rtys.map(expandRSType))
      // $COVERAGE-OFF$ unreachable
      /** expandSType takes the result of [[mkSType]], so there is no [[ST.RefType]] */
      case ST.RefType(_) =>
        throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def expandRSType(rts: ST.RowType): ST.RowType =
    rts match {
      case ST.RowVarType(rTyVar) =>
        ST.RowVarType(rTyVar)
      case ST.RowEmptyType =>
        ST.RowEmptyType
      case ST.RowFieldType(ST.Field(l, st), rTyps) =>
        ST.RowFieldType(ST.Field(l, expandSType(st)), expandRSType(rTyps))
      // $COVERAGE-OFF$ unreachable
      /** expandRSType takes the result of [[mkSType]], so there is no [[ST.RefType]] */
      case ST.RowRefType(sRowTypeVar) =>
        throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  def expandType(t: T.Type): T.Type =
    t match {
      case T.VarType(tv) =>
        T.VarType(tv)
      case Types.ConType(TyCons.NamedTyCon(name), ts, List()) if aliases.exists(_.name == name) =>
        val alias = aliases.find(_.name == name).get
        val expandedParams: List[SubVal] =
          ts.map(expandType).map(Left(_))
        val t1 = mkType(alias.body, alias.params.map(_.name).zip(expandedParams).toMap[String, SubVal])
        expandType(t1)
      case Types.ConType(tyCon, ts, rts) =>
        Types.ConType(tyCon, ts.map(expandType), rts.map(expandRowType))
    }

  private def expandRowType(rt: T.RowType): T.RowType =
    rt match {
      case T.RowVarType(rTyVar) =>
        T.RowVarType(rTyVar)
      case T.RowEmptyType =>
        T.RowEmptyType
      case T.RowFieldType(T.Field(l, t), rt) =>
        T.RowFieldType(T.Field(l, expandType(t)), expandRowType(rt))
    }
}
