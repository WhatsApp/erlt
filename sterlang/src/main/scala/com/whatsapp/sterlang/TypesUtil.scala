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

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

class TypesUtil(val vars: Vars) {

  val T = Types
  val ST = STypes

  def generalize(d: Int)
                (t: T.Type): ST.TypeSchema =
    generalize_*(d)(List(t)).head

  def generalize_*(d: Int)
                  (types: List[T.Type]): List[ST.TypeSchema] =
    generalizeImpl(_ >= d)(types)

  // All (suitable according to gen) open variables are transformed into schematic variables
  private def generalizeImpl(gen: T.Depth => Boolean)
                            (types: List[T.Type]): List[ST.TypeSchema] = {

    var nextVarId = 0
    var nextRowVarId = 0

    var tMap: TreeMap[T.TypeVar, ST.TypeVar] =
      TreeMap.empty(vars.TVarOrdering)
    var rMap: TreeMap[T.RowTypeVar, ST.RowTypeVar] =
      TreeMap.empty(vars.RVarOrdering)
    val rargsBuffer: ListBuffer[T.RtVarKind] =
      ListBuffer()

    def mkSTypeVar(v: T.TypeVar): ST.TypeVar = {
      val sTypeVar = ST.TypeVar(nextVarId)
      nextVarId = nextVarId + 1
      tMap = tMap + (v -> sTypeVar)
      sTypeVar
    }

    //  TODO - should not we make kind a part of SRowTypeVar?
    def mkSRowTypeVar(rv: T.RowTypeVar, k: T.RtVarKind): ST.RowTypeVar = {
      val sRowTypeVar = ST.RowTypeVar(nextRowVarId)
      nextRowVarId = nextRowVarId + 1
      rargsBuffer.append(k)
      rMap = rMap + (rv -> sRowTypeVar)
      sRowTypeVar
    }

    def tFind(v: T.TypeVar): Option[ST.TypeVar] =
      tMap.get(v)

    def rFind(rv: T.RowTypeVar): Option[ST.RowTypeVar] =
      rMap.get(rv)

    def typ(tp: T.Type): ST.Type = tp match {
      case T.VarType(typeVar) =>
        tFind(typeVar) match {
          case Some(sTypeVar) =>
            ST.RefType(sTypeVar)
          case None =>
            vars.tGet(typeVar) match {
              case T.Instance(tp) =>
                typ(tp)
              case T.Open(d) =>
                if (gen(d))
                  ST.RefType(mkSTypeVar(typeVar))
                else
                  ST.PlainType(tp)
            }
        }
      case T.ConType(tyCon, types, rowTypes) =>
        ST.ConType(tyCon, types.map(typ), rowTypes.map(rtyp))
    }

    def rtyp(tp: T.RowType): ST.RowType = tp match {
      case T.RowVarType(rowTypeVar) =>
        rFind(rowTypeVar) match {
          case Some(sRowTypeVar) =>
            ST.RowRefType(sRowTypeVar)
          case None =>
            vars.rGet(rowTypeVar) match {
              case T.RowInstance(rowType) =>
                rtyp(rowType)
              case T.RowOpen(d, kind) =>
                if (gen(d))
                  ST.RowRefType(mkSRowTypeVar(rowTypeVar, kind))
                else
                  ST.RowVarType(rowTypeVar)
            }
        }
      case T.RowEmptyType =>
        ST.RowEmptyType
      case T.RowFieldType(T.Field(l, tp), rowType) =>
        ST.RowFieldType(ST.Field(l, typ(tp)), rtyp(rowType))
    }

    val bodies: List[ST.Type] =
      types.map(typ)

    val targs = nextVarId
    val rargs = rargsBuffer.toList

    // TODO: will it be a difference if we generalize them one-by-one?
    bodies.map(ST.TypeSchema(targs, rargs, _))
  }

  // dual to generalize, all schematic variables -> open variables
  def instantiate(d: T.Depth, typSchema: ST.TypeSchema): T.Type = {

    val tMap: Map[ST.TypeVar, T.TypeVar] =
      (0 until typSchema.targs).map {
        i => ST.TypeVar(i) -> vars.tVar(T.Open(d))
      }.toMap
    val rMap: Map[ST.RowTypeVar, T.RowTypeVar] =
      typSchema.rargs.zipWithIndex.map {
        case (k, i) =>  ST.RowTypeVar(i) -> vars.rVar(T.RowOpen(d, k))
      }.toMap

    def tSub(tMap: Map[ST.TypeVar, T.TypeVar])
            (sType: ST.Type): T.Type = sType match {
      case ST.PlainType(t) =>
        t
      case ST.ConType(tc, tl, rtl) =>
        T.ConType(tc, tl.map(tSub(tMap)), rtl.map(rSub(tMap)))
      case ST.RefType(sTypeVar) =>
        T.VarType(tMap(sTypeVar))
    }

    def rSub(tMap: Map[ST.TypeVar, T.TypeVar])
            (sRowType: ST.RowType): T.RowType = sRowType match {
      case ST.RowVarType(rTypeVar) =>
        T.RowVarType(rTypeVar)
      case ST.RowEmptyType =>
        T.RowEmptyType
      case ST.RowFieldType(ST.Field(l, ts), sRowType) =>
        T.RowFieldType(T.Field(l, tSub(tMap)(ts)), rSub(tMap)(sRowType))
      case ST.RowRefType(rTsVar) =>
        T.RowVarType(rMap(rTsVar))
    }

    tSub(tMap)(typSchema.body)
  }

}
