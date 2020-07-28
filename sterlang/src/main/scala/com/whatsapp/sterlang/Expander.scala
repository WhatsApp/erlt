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

class Expander(
    val aliases: List[Ast.TypeAlias],
    val tGen: () => Types.Type,
    val rtGen: (Set[String]) => Types.RowTypeVar,
) {
  val T = Types
  val MT = METypes
  val ST = STypes
  val MST = MESTypes
  val S = Ast

  def mkType(t: S.Type, sub: Map[String, T.Type]): T.Type =
    t match {
      case S.TypeVar(n) =>
        sub(n)
      case S.WildTypeVar() =>
        tGen()
      case S.UserType(name, params) =>
        MT.NamedType(name.stringId, params.map(mkType(_, sub)))
      case S.TupleType(params) =>
        MT.TupleType(params.map(mkType(_, sub)))
      case S.RecordType(fields) =>
        val tFields = fields.map { case S.Field(n, tp) => T.Field(n, mkType(tp, sub)) }
        val zero: T.RowType = T.RowEmptyType
        val rowType = tFields.foldRight(zero)(T.RowFieldType)
        MT.RecordType(rowType)
      case S.OpenRecordType(fields, _) =>
        val lbls = fields.map(_.label).toSet
        val tFields = fields.map { case S.Field(n, tp) => T.Field(n, mkType(tp, sub)) }
        val zero: T.RowType = T.RowVarType(rtGen(lbls))
        val rowType = tFields.foldRight(zero)(T.RowFieldType)
        MT.RecordType(rowType)
      case S.FunType(params, res) =>
        MT.FunType(params.map(mkType(_, sub)), mkType(res, sub))
      case S.ListType(elemType) =>
        MT.ListType(mkType(elemType, sub))
      case S.ERecordType(name) =>
        MT.ERecordType(name)
    }

  def mkSType(t: S.Type, sub: Map[String, ST.Type]): ST.Type =
    t match {
      case S.TypeVar(n) =>
        sub(n)
      case S.WildTypeVar() =>
        ST.PlainType(tGen())
      case S.UserType(name, params) =>
        MST.EnumType(name.stringId, params.map(mkSType(_, sub)))
      case S.TupleType(params) =>
        MST.TupleType(params.map(mkSType(_, sub)))
      case S.RecordType(fields) =>
        val tFields = fields.map { case S.Field(n, tp) => ST.Field(n, mkSType(tp, sub)) }
        val zero: ST.RowType = ST.RowEmptyType
        val rowType = tFields.foldRight(zero)(ST.RowFieldType)
        MST.RecordType(rowType)
      case S.OpenRecordType(fields, _) =>
        val lbls = fields.map(_.label).toSet
        val tFields = fields.map { case S.Field(n, tp) => ST.Field(n, mkSType(tp, sub)) }
        val zero: ST.RowType = ST.RowVarType(rtGen(lbls))
        val rowType = tFields.foldRight(zero)(ST.RowFieldType)
        MST.RecordType(rowType)
      case S.FunType(params, res) =>
        MST.FunType(params.map(mkSType(_, sub)), mkSType(res, sub))
      case S.ListType(elemType) =>
        MST.ListType(mkSType(elemType, sub))
      case S.ERecordType(name) =>
        MST.ERecordType(name)
    }

  def expandSType(ts: ST.Type): ST.Type =
    ts match {
      case ST.PlainType(typ) =>
        ST.PlainType(expandType(typ))
      case ST.ConType(TyCons.NamedTyCon(name), typs, List()) if aliases.exists(_.name == name) =>
        val alias = aliases.find(_.name == name).get
        val expandedParams = typs.map(expandSType)
        val t1 = mkSType(alias.body, alias.params.map(_.name).zip(expandedParams).toMap)
        expandSType(t1)
      case ST.ConType(tyCon, typs, rtys) =>
        ST.ConType(tyCon, typs.map(expandSType), rtys.map(expandRSType))
      case ST.RefType(sTypeVar) =>
        ST.RefType(sTypeVar)
    }

  private def expandRSType(rts: ST.RowType): ST.RowType =
    rts match {
      case ST.RowVarType(rTyVar) =>
        ST.RowVarType(rTyVar)
      case ST.RowEmptyType =>
        ST.RowEmptyType
      case ST.RowFieldType(ST.Field(l, st), rTyps) =>
        ST.RowFieldType(ST.Field(l, expandSType(st)), expandRSType(rTyps))
      case ST.RowRefType(sRowTypeVar) =>
        ST.RowRefType(sRowTypeVar)
    }

  def expandType(t: T.Type): T.Type =
    t match {
      case T.VarType(tv) =>
        T.VarType(tv)
      case Types.ConType(TyCons.NamedTyCon(name), ts, List()) if aliases.exists(_.name == name) =>
        val alias = aliases.find(_.name == name).get
        val expandedParams = ts.map(expandType)
        val t1 = mkType(alias.body, alias.params.map(_.name).zip(expandedParams).toMap)
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
