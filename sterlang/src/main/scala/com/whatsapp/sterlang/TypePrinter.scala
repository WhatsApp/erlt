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

  private val tyVarSetEmpty: TreeSet[Types.TypeVar] =
    TreeSet.empty(vars.TVarOrdering)
  private val rtyVarSetEmpty: TreeSet[Types.RowTypeVar] =
    TreeSet.empty(vars.RVarOrdering)

  private def type2typeSchema(t: Types.Type): STypes.Type =
    tu.generalize(Int.MaxValue)(t).body

  // sTypeVars:
  //          the set of all encountered sTypeVars
  // freeTypeVars:
  //          all encountered free type vars
  // freeRowTypeVars:
  //          all encountered free row type vars
  private case class Info(
      sTypeVars: Set[STypes.TypeVar] = Set.empty,
      freeTypeVars: TreeSet[Types.TypeVar] = tyVarSetEmpty,
      freeRowTypeVars: TreeSet[Types.RowTypeVar] = rtyVarSetEmpty,
  )

  private def collectInfo(body: STypes.Type): (STypes.Type, Info) = {

    def join(m1: Info, m2: Info): Info = {
      val sVars = m1.sTypeVars ++ m2.sTypeVars
      val tVars = m1.freeTypeVars union m2.freeTypeVars
      val tRowVars = m1.freeRowTypeVars union m2.freeRowTypeVars
      Info(sVars, tVars, tRowVars)
    }

    def typ(ts: STypes.Type): (STypes.Type, Info) =
      ts match {
        case STypes.PlainType(Types.VarType(v)) =>
          vars.tGet(v) match {
            case Types.Instance(t) =>
              typ(type2typeSchema(t))
            case Types.Open(_) =>
              (ts, Info(freeTypeVars = tyVarSetEmpty + v))
          }
        case STypes.PlainType(t: Types.ConType) =>
          typ(type2typeSchema(t))
        case STypes.ConType(tyc, sTypes, sRowTypes) =>
          val (sTypes1, infos1) = sTypes.map(typ).unzip
          val (sRowTypes1, infos2) = sRowTypes.map(rtyp).unzip
          val info = (infos1 ++ infos2).foldLeft(Info())(join)
          (STypes.ConType(tyc, sTypes1, sRowTypes1), info)
        case ts @ STypes.RefType(sTypeVar) =>
          (ts, Info(sTypeVars = Set(sTypeVar)))
      }

    def rtyp(rts: STypes.RowType): (STypes.RowType, Info) =
      rts match {
        case rts @ STypes.RowVarType(rowTypeVar) =>
          (rts, Info(freeRowTypeVars = rtyVarSetEmpty + rowTypeVar))
        case rts @ STypes.RowEmptyType =>
          (rts, Info())
        case STypes.RowFieldType(STypes.Field(label, ts), rts) =>
          val (ts1, fm) = typ(ts)
          val (rts1, rm) = rtyp(rts)
          (STypes.RowFieldType(STypes.Field(label, ts1), rts1), join(fm, rm))
        case rts @ STypes.RowRefType(v) =>
          (rts, Info())
      }

    typ(body)
  }

  private type TMap = TreeMap[Types.TypeVar, String]
  private type RTMap = TreeMap[Types.RowTypeVar, String]

  private val globalNames = new TypePrinterUtil
  private val tMapEmpty: TMap = TreeMap.empty(vars.TVarOrdering)
  private val rtMapEmpty: RTMap = TreeMap.empty(vars.RVarOrdering)
  private var freeTypeVarNames: TMap = tMapEmpty
  private var freeRowTypeVarNames: RTMap = rtMapEmpty

  def typeSchema(schema: STypes.TypeSchema, mode: TypePrinter2.Mode): String = {

    val localNames = new TypePrinterUtil
    val (body, info) = collectInfo(schema.body)

    val sTypeVarNames: Map[STypes.TypeVar, String] =
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
      case TypePrinter2.Specs =>
        assert(freeTypeVarNames.isEmpty)
        assert(freeRowTypeVarNames.isEmpty)
      case TypePrinter2.Types =>
        assert(sTypeVarNames.isEmpty)
        assert(sRowTypeVarNames.isEmpty)
    }

    /// ---------- printing ------------------

    def typ(sType: STypes.Type): String =
      sType match {
        case STypes.PlainType(Types.VarType(typeVar)) =>
          freeTypeVarNames(typeVar)
        case STypes.ConType(tyCon, types, rowTypes) =>
          conType(tyCon, types, rowTypes)
        case STypes.RefType(sTypeVar) =>
          sTypeVarNames(sTypeVar)
      }

    def conType(tyCon: TyCons.TyCon, sTypes: List[STypes.Type], sRowTypes: List[STypes.RowType]): String =
      (tyCon, sTypes, sRowTypes) match {
        case (TyCons.FunTyCon(_), ts, List()) =>
          val args = ts.init
          val result = ts.last
          "fun(" + args.map(typ).mkString("(", ", ", ")") + " -> " + typ(result) + ")"
        case (TyCons.ShapeTyCon, Nil, List(rts)) =>
          shapeStr(rts)
        case (TyCons.NamedTyCon(name), ts, Nil) =>
          ts.map(typ).mkString(name + "(", ", ", ")")
        case (TyCons.TupleCon(_), ts, Nil) =>
          ts.map(typ).mkString("{", ", ", "}")
        case (TyCons.StructTyCon(name), Nil, Nil) =>
          s"#$name{}"
      }

    def shapeStr(sRowType: STypes.RowType): String =
      sRowType match {
        case STypes.RowEmptyType => "#()"
        case _                   => rowtype(sRowType, lb = "#( ", rb = " )")
      }

    def namedField(sField: STypes.Field): String =
      sField.label + " :: " + typ(sField.value)

    def rowtype(sRowType: STypes.RowType, lb: String, rb: String): String = {

      def withBaseVar(baseVar: String, fields: List[STypes.Field]): String = {
        val elems = fields.sortBy(_.label).map(namedField) ++ List(baseVar)
        elems.mkString(lb, ", ", rb)
      }

      @scala.annotation.tailrec
      def unfold(sRowType: STypes.RowType, fields: List[STypes.Field]): String =
        sRowType match {
          case STypes.RowEmptyType =>
            val sortedFields = fields.sortBy(_.label)
            sortedFields.map(namedField).mkString(lb, ", ", rb)
          case STypes.RowFieldType(sField, sRowType) =>
            unfold(sRowType, sField :: fields)
          case STypes.RowVarType(rowTypeVar) =>
            withBaseVar(freeRowTypeVarNames(rowTypeVar), fields)
          case STypes.RowRefType(sRowTypeVar) =>
            withBaseVar(sRowTypeVarNames(sRowTypeVar.id), fields)
        }

      unfold(sRowType, Nil)
    }

    typ(body)
  }

}
