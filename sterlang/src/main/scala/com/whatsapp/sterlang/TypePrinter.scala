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

import scala.collection.immutable.{IntMap, TreeMap, TreeSet}

class TypePrinter(private val vars: Vars, private val tu: TypesUtil) {
  val T = Types
  val ST = STypes
  private val TC = TyCons

  def type2typeSchema(t: T.Type): ST.Type =
    tu.generalize(Int.MaxValue)(t).body

  // sTypeVars:
  //          the set of all encountered sTypeVars
  // freeTypeVars:
  //          all encountered free type vars
  // freeRowTypeVars:
  //          all encountered free row type vars
  private case class Info(
      sTypeVars: Set[ST.TypeVar] = Set.empty,
      freeTypeVars: TreeSet[T.TypeVar] = tyVarSetEmpty,
      freeRowTypeVars: TreeSet[T.RowTypeVar] = rtyVarSetEmpty,
  )

  private def collectInfo(body: ST.Type): (ST.Type, Info) = {

    def join(m1: Info, m2: Info): Info = {
      val sVars = m1.sTypeVars ++ m2.sTypeVars
      val tVars = m1.freeTypeVars union m2.freeTypeVars
      val tRowVars = m1.freeRowTypeVars union m2.freeRowTypeVars
      Info(sVars, tVars, tRowVars)
    }

    def typ(ts: ST.Type): (ST.Type, Info) =
      ts match {
        case ST.PlainType(T.VarType(v)) =>
          vars.tGet(v) match {
            case T.Instance(t) =>
              typ(type2typeSchema(t))
            case T.Open(_) =>
              (ts, Info(freeTypeVars = tyVarSetEmpty + v))
          }
        case ST.PlainType(t: T.ConType) =>
          typ(type2typeSchema(t))
        case ST.ConType(tyc, sTypes, sRowTypes) =>
          val (sTypes1, infos1) = sTypes.map(typ).unzip
          val (sRowTypes1, infos2) = sRowTypes.map(rtyp).unzip
          val info = (infos1 ++ infos2).foldLeft(Info())(join)
          (ST.ConType(tyc, sTypes1, sRowTypes1), info)
        case ts @ ST.RefType(sTypeVar) =>
          (ts, Info(sTypeVars = Set(sTypeVar)))
      }

    def rtyp(rts: ST.RowType): (ST.RowType, Info) =
      rts match {
        case rts @ ST.RowVarType(rowTypeVar) =>
          (rts, Info(freeRowTypeVars = rtyVarSetEmpty + rowTypeVar))
        case rts @ ST.RowEmptyType =>
          (rts, Info())
        case ST.RowFieldType(ST.Field(label, ts), rts) =>
          val (ts1, fm) = typ(ts)
          val (rts1, rm) = rtyp(rts)
          (ST.RowFieldType(ST.Field(label, ts1), rts1), join(fm, rm))
        case rts @ ST.RowRefType(v) =>
          (rts, Info())
      }

    typ(body)
  }

  val globalNames = new TypePrinterUtil
  private val tMapEmpty: TMap = TreeMap.empty(vars.TVarOrdering)
  private val rtMapEmpty: RTMap = TreeMap.empty(vars.RVarOrdering)
  private var freeTypeVarNames: TMap = tMapEmpty
  private var freeRowTypeVarNames: RTMap = rtMapEmpty

  def typeSchema(schema: ST.TypeSchema, mode: TypePrinter2.Mode): String = {

    val localNames = new TypePrinterUtil
    val (body, info) = collectInfo(schema.body)

    val sTypeVarNames: Map[ST.TypeVar, String] =
      info.sTypeVars.toList.sortBy(_.id).map { (_, localNames.nextSchematicTypeName()) }.toMap
    val sRowTypeVarNames: IntMap[String] =
      schema.rargs.indices.foldLeft(IntMap.empty[String]) {
        case (m, i) => m + (i -> localNames.nextSchematicRowTypeName())
      }

    for (ftv <- info.freeTypeVars) {
      if (!freeTypeVarNames.contains(ftv)) {
        freeTypeVarNames = freeTypeVarNames + (ftv -> globalNames.nextftname())
      }
    }

    for (frtv <- info.freeRowTypeVars) {
      if (!freeRowTypeVarNames.contains(frtv)) {
        freeRowTypeVarNames = freeRowTypeVarNames + (frtv -> globalNames.nextfrname())
      }
    }

    mode match {
      case TypePrinter2.TypeSchemes =>
        assert(freeTypeVarNames.isEmpty)
        assert(freeRowTypeVarNames.isEmpty)
      case TypePrinter2.Types =>
        assert(sTypeVarNames.isEmpty)
        assert(sRowTypeVarNames.isEmpty)
    }

    /// ---------- printing ------------------

    def typ(sType: ST.Type): String =
      sType match {
        case ST.PlainType(T.VarType(typeVar)) =>
          freeTypeVarNames(typeVar)
        case ST.ConType(tyCon, types, rowTypes) =>
          conType(tyCon, types, rowTypes)
        case ST.RefType(sTypeVar) =>
          sTypeVarNames(sTypeVar)
      }

    def conType(tyCon: TC.TyCon, sTypes: List[ST.Type], sRowTypes: List[ST.RowType]): String =
      (tyCon, sTypes, sRowTypes) match {
        case (TC.FunTyCon(_), ts, List()) =>
          val args = ts.init
          val result = ts.last
          "fun(" + args.map(typ).mkString("(", ", ", ")") + " -> " + typ(result) + ")"
        case (TC.RecordTyCon, Nil, List(rts)) =>
          recordStr(rts)
        case (TC.NamedTyCon(name), ts, Nil) =>
          ts.map(typ).mkString(name + "(", ", ", ")")
        case (TC.TupleCon(_), ts, Nil) =>
          ts.map(typ).mkString("{", ", ", "}")
        case (TC.StructTyCon(name), Nil, Nil) =>
          s"#$name{}"
      }

    def recordStr(sRowType: ST.RowType): String =
      sRowType match {
        case ST.RowEmptyType => "#()"
        case _               => rowtype(sRowType, lb = "#( ", rb = " )")
      }

    def namedField(sField: ST.Field): String =
      sField.label + " :: " + typ(sField.value)

    def rowtype(sRowType: ST.RowType, lb: String, rb: String): String = {

      def withBaseVar(baseVar: String, fields: List[ST.Field]): String = {
        val elems = fields.sortBy(_.label).map(namedField) ++ List(baseVar)
        elems.mkString(lb, ", ", rb)
      }

      @scala.annotation.tailrec
      def unfold(sRowType: ST.RowType, fields: List[ST.Field]): String =
        sRowType match {
          case ST.RowEmptyType =>
            val sortedFields = fields.sortBy(_.label)
            sortedFields.map(namedField).mkString(lb, ", ", rb)
          case ST.RowFieldType(sField, sRowType) =>
            unfold(sRowType, sField :: fields)
          case ST.RowVarType(rowTypeVar) =>
            withBaseVar(freeRowTypeVarNames(rowTypeVar), fields)
          case ST.RowRefType(sRowTypeVar) =>
            withBaseVar(sRowTypeVarNames(sRowTypeVar.id), fields)
        }

      unfold(sRowType, Nil)
    }

    typ(body)
  }

  // -- utilities ---
  private type TMap = TreeMap[T.TypeVar, String]
  private type RTMap = TreeMap[T.RowTypeVar, String]

  private val tyVarSetEmpty: TreeSet[T.TypeVar] =
    TreeSet.empty(vars.TVarOrdering)
  private val rtyVarSetEmpty: TreeSet[T.RowTypeVar] =
    TreeSet.empty(vars.RVarOrdering)

}
