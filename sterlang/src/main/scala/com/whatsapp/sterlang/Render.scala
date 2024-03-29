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

import com.whatsapp.sterlang.Ast.Program
import com.whatsapp.sterlang.Doc.LspTypeInfo

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable.ListBuffer

object Render {
  private sealed trait Mode
  private case object TypesMode extends Mode
  private case object SpecsMode extends Mode

  private type TMap = TreeMap[Types.TypeVar, String]
  private type RTMap = TreeMap[Types.RowTypeVar, String]

  private class RenderUtil {

    private var counter = 0

    def genTypeName(): String = {
      val n = counter
      counter = counter + 1
      str(n)
    }

    private def str(n: Int): String =
      if (n < 26) ('A' + n).toChar.toString else "T_" + n
  }
}

case class Render(private val vars: Vars) {
  import Render._

  // Renders specs. A spec per line
  def specs(funs: List[AnnAst.Fun], env: Env): List[String] = {
    funs.map { f =>
      resetContext()
      val ts = env(f.name)
      // fun((ArgType1, ArgType2, ...) -> ResType)
      val raw = typeScheme(ts, SpecsMode)
      val inner = raw.substring(4, raw.length - 1)
      val slashIndex = f.name.lastIndexOf('/')
      val normV = f.name.substring(0, slashIndex)
      val pp = "-spec " + normV + inner + "."
      pp
    }
  }

  def hoverSpecs(program: Program, funs: List[AnnAst.Fun], env: Env): List[LspTypeInfo] =
    funs.flatMap { f =>
      if (!program.specs.exists(_.name.stringId == f.name)) {
        resetContext()
        val ts = env(f.name)
        // fun((ArgType1, ArgType2, ...) -> ResType)
        val raw = typeScheme(ts, SpecsMode)
        // "fun "
        val inner = raw.substring(4, raw.length - 1)
        val slashIndex = f.name.lastIndexOf('/')
        val normV = f.name.substring(0, slashIndex)
        val pp = "-spec " + normV + inner + "."
        val start = f.r.start
        val end = f.r.start.copy(column = f.r.start.column + normV.length)
        Some(LspTypeInfo(Doc.Range(start, end), pp, kind = "lens"))
      } else {
        None
      }
    }

  def typ(tp: Types.Type): String = {
    resetContext()
    val fakeTypeScheme = STypes.TypeScheme(List.empty, List.empty, STypes.PlainType(tp))
    typeScheme(fakeTypeScheme, TypesMode)
  }

  def typs(types: List[Types.Type]): String = {
    resetContext()
    val typeStrings = types.map { tp =>
      val fakeTypeScheme = STypes.TypeScheme(List.empty, List.empty, STypes.PlainType(tp))
      typeScheme(fakeTypeScheme, TypesMode)
    }
    typeStrings.mkString(", ")
  }

  def scheme(s: STypes.TypeScheme): String = {
    resetContext()
    typeScheme(s, SpecsMode)
  }

  def varTypes(funs: List[AnnAst.Fun], includeTopLevelFuns: Boolean = false): List[Doc.ElaboratedTypeInfo] = {
    val buf = new ListBuffer[Doc.ElaboratedTypeInfo]
    funs.foreach { fun =>
      resetContext()
      val AnnAst.Fun(name, clauses, fType) = fun

      if (includeTopLevelFuns) {
        val fakeTypeScheme = STypes.TypeScheme(List.empty, List.empty, STypes.PlainType(fType))
        val typeSchemeString = typeScheme(fakeTypeScheme, TypesMode)

        val slashIndex = name.lastIndexOf('/')
        val normV = name.substring(0, slashIndex)
        val start = fun.r.start
        val end = fun.r.start.copy(column = fun.r.start.column + normV.length)
        val range = Doc.Range(start, end)

        val funInfo = Doc.ElaboratedTypeInfo(name, range, typeSchemeString)
        buf.append(funInfo)
      }

      clauses.foreach { clause =>
        clause.pats.foreach(renderPat(buf))
        renderBody(buf)(clause.body)
      }
    }

    buf.toList
  }

  private def renderPat(buf: ListBuffer[Doc.ElaboratedTypeInfo])(pat: AnnAst.Pat): Unit =
    pat match {
      case AnnAst.WildPat() =>
      // nothing
      case AnnAst.PinnedVarPat(_) =>
      // nothing
      case AnnAst.VarPat(v) =>
        val fakeTypeScheme = STypes.TypeScheme(List.empty, List.empty, STypes.PlainType(pat.typ))
        val typeSchemeString = typeScheme(fakeTypeScheme, TypesMode)
        val info = Doc.ElaboratedTypeInfo(varName = v, range = pat.r, typeRepr = typeSchemeString)
        buf.append(info)
      case AnnAst.AndPat(p1, p2) =>
        renderPat(buf)(p1)
        renderPat(buf)(p2)

      case AnnAst.LiteralPat(_) =>
      //
      case AnnAst.TuplePat(pats) =>
        pats.foreach(renderPat(buf))
      case AnnAst.NilPat() =>
      //
      case AnnAst.ShapePat(fields) =>
        fields.foreach { f => renderPat(buf)(f.value) }

      case AnnAst.ConsPat(hPat, tPat) =>
        renderPat(buf)(hPat)
        renderPat(buf)(tPat)
      case AnnAst.EnumPat(_, _, fields) =>
        fields.foreach { f => renderPat(buf)(f.value) }

      case AnnAst.BinPat(elems) =>
        elems.foreach { elem =>
          renderPat(buf)(elem.pat)
          elem.size.map(renderExp(buf))
        }

      case AnnAst.StructPat(_, fields) =>
        fields.foreach { f => renderPat(buf)(f.value) }
    }

  private def renderBody(buf: ListBuffer[Doc.ElaboratedTypeInfo])(body: AnnAst.Body): Unit = {
    renderValDefs(buf)(body.prelude)
    renderValDef(buf)(body.main)
  }

  private def renderValDefs(buf: ListBuffer[Doc.ElaboratedTypeInfo])(defs: List[AnnAst.ValDef]): Unit = {
    defs.foreach(renderValDef(buf))
  }

  private def renderValDef(buf: ListBuffer[Doc.ElaboratedTypeInfo])(d: AnnAst.ValDef): Unit =
    d match {
      case AnnAst.ValDef(pat, exp, _, _, _) =>
        renderPat(buf)(pat)
        renderExp(buf)(exp)
    }

  private def renderExp(buf: ListBuffer[Doc.ElaboratedTypeInfo])(exp: AnnAst.Exp): Unit = {
    exp match {
      case AnnAst.VarExp(_) =>
      // Nothing

      case AnnAst.LiteralExp(_) =>
      // Nothing
      case AnnAst.TupleExp(elems) =>
        elems.foreach(renderExp(buf))
      case AnnAst.NilExp() =>
      // Nothing
      case AnnAst.BinExp(elems) =>
        elems.foreach { elem =>
          renderExp(buf)(elem.expr)
          elem.size.foreach(renderExp(buf))
        }

      case AnnAst.UOpExp(_, exp) =>
        renderExp(buf)(exp)
      case AnnAst.BinOpExp(_, exp1, exp2) =>
        renderExp(buf)(exp1)
        renderExp(buf)(exp2)

      case AnnAst.CaseExp(selector, branches) =>
        renderExp(buf)(selector)
        branches.foreach { branch =>
          renderPat(buf)(branch.pat)
          renderBody(buf)(branch.body)
        }

      case AnnAst.IfExp(bodies) =>
        bodies.foreach(renderBody(buf))

      case AnnAst.Comprehension(template, qualifiers) =>
        qualifiers.foreach {
          case AnnAst.Filter(exp) =>
            renderExp(buf)(exp)
          case AnnAst.Generator(pat, exp) =>
            renderPat(buf)(pat)
            renderExp(buf)(exp)
          case AnnAst.BGenerator(pat, exp) =>
            renderPat(buf)(pat)
            renderExp(buf)(exp)
        }
        renderExp(buf)(template)
      case AnnAst.BComprehension(template, qualifiers) =>
        qualifiers.foreach {
          case AnnAst.Filter(exp) =>
            renderExp(buf)(exp)
          case AnnAst.Generator(pat, exp) =>
            renderPat(buf)(pat)
            renderExp(buf)(exp)
          case AnnAst.BGenerator(pat, exp) =>
            renderPat(buf)(pat)
            renderExp(buf)(exp)
        }
        renderExp(buf)(template)

      case AnnAst.BlockExp(body) =>
        renderBody(buf)(body)

      case AnnAst.ShapeCreateExp(fields) =>
        fields.foreach(f => renderExp(buf)(f.value))
      case AnnAst.ShapeSelectExp(exp, _) =>
        renderExp(buf)(exp)
      case AnnAst.ShapeUpdateExp(exp, fields) =>
        renderExp(buf)(exp)
        fields.foreach(f => renderExp(buf)(f.value))

      case AnnAst.StructCreate(_, fields) =>
        fields.foreach(f => renderExp(buf)(f.value))
      case AnnAst.StructUpdate(struct, _, fields) =>
        renderExp(buf)(struct)
        fields.foreach(f => renderExp(buf)(f.value))
      case AnnAst.StructSelect(struct, _, _) =>
        renderExp(buf)(struct)

      case AnnAst.FnExp(clauses) =>
        clauses.foreach { clause =>
          clause.pats.foreach(renderPat(buf))
          renderBody(buf)(clause.body)
        }
      case AnnAst.NamedFnExp(name, clauses) =>
        // TODO: the source location is wrong, but I don't think it matters.
        renderPat(buf)(AnnAst.VarPat(name)(exp.r)(exp.typ))
        clauses.foreach { clause =>
          clause.pats.foreach(renderPat(buf))
          renderBody(buf)(clause.body)
        }
      case AnnAst.AppExp(head, args) =>
        renderExp(buf)(head)
        args.foreach(renderExp(buf))

      case AnnAst.ConsExp(h, t) =>
        renderExp(buf)(h)
        renderExp(buf)(t)
      case AnnAst.EnumExp(_, _, fields) =>
        fields.foreach(f => renderExp(buf)(f.value))

      case AnnAst.TryCatchExp(tryBody, catchBranches, after) =>
        renderBody(buf)(tryBody)
        catchBranches.foreach { branch =>
          renderPat(buf)(branch.pat)
          renderBody(buf)(branch.body)
        }
        after.foreach(renderBody(buf))

      case AnnAst.TryOfCatchExp(tryBody, tryBranches, catchBranches, after) =>
        renderBody(buf)(tryBody)
        tryBranches.foreach { branch =>
          renderPat(buf)(branch.pat)
          renderBody(buf)(branch.body)
        }
        catchBranches.foreach { branch =>
          renderPat(buf)(branch.pat)
          renderBody(buf)(branch.body)
        }
        after.foreach(renderBody(buf))

      case AnnAst.ReceiveExp(branches, after) =>
        branches.foreach { branch =>
          renderPat(buf)(branch.pat)
          renderBody(buf)(branch.body)
        }
        after.foreach { afterBody =>
          renderExp(buf)(afterBody.timeout)
          renderBody(buf)(afterBody.body)
        }
    }

  }

  val tu = new TypesUtil(vars)
  private val tyVarSetEmpty: TreeSet[Types.TypeVar] =
    TreeSet.empty(vars.TVarOrdering)
  private val rtyVarSetEmpty: TreeSet[Types.RowTypeVar] =
    TreeSet.empty(vars.RVarOrdering)

  private def type2typeScheme(t: Types.Type): STypes.Type =
    tu.generalize(Int.MaxValue)(t).body

  // sTypeVars:
  //          the set of all encountered schematic type vars
  // sRowTypeVars:
  //          the set of all encountered schematic row type vars
  // freeTypeVars:
  //          all encountered free type vars
  // freeRowTypeVars:
  //          all encountered free row type vars
  private case class Info(
      sTypeVars: Set[STypes.TypeVar] = Set.empty,
      sRowTypeVars: Set[STypes.RowTypeVar] = Set.empty,
      freeTypeVars: TreeSet[Types.TypeVar] = tyVarSetEmpty,
      freeRowTypeVars: TreeSet[Types.RowTypeVar] = rtyVarSetEmpty,
  )

  private def collectInfo(body: STypes.Type): (STypes.Type, Info) = {

    def join(m1: Info, m2: Info): Info = {
      val sVars = m1.sTypeVars ++ m2.sTypeVars
      val sRowVars = m1.sRowTypeVars ++ m2.sRowTypeVars
      val tVars = m1.freeTypeVars union m2.freeTypeVars
      val tRowVars = m1.freeRowTypeVars union m2.freeRowTypeVars
      Info(sVars, sRowVars, tVars, tRowVars)
    }

    def typ(ts: STypes.Type): (STypes.Type, Info) =
      ts match {
        case STypes.PlainType(Types.VarType(v)) =>
          vars.tGet(v) match {
            case Types.Instance(t) =>
              typ(type2typeScheme(t))
            case Types.Open(_) =>
              (ts, Info(freeTypeVars = tyVarSetEmpty + v))
          }
        case STypes.PlainType(t: Types.ConType) =>
          typ(type2typeScheme(t))
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
        case rts @ STypes.RowRefType(sRowTypeVar) =>
          (rts, Info(sRowTypeVars = Set(sRowTypeVar)))
      }

    typ(body)
  }

  private var globalNames: RenderUtil = _
  private var freeTypeVarNames: TMap = _
  private var freeRowTypeVarNames: RTMap = _

  private def resetContext(): Unit = {
    globalNames = new RenderUtil
    freeTypeVarNames = TreeMap.empty(vars.TVarOrdering)
    freeRowTypeVarNames = TreeMap.empty(vars.RVarOrdering)
  }

  private def typeScheme(scheme: STypes.TypeScheme, mode: Mode): String = {

    val localNames = new RenderUtil
    val (body, info) = collectInfo(scheme.body)

    // schematic type vars: specs mode!
    val sVarNames: Map[Int, String] =
      (info.sTypeVars.map(_.id) ++ info.sRowTypeVars.map(_.id)).toList.sorted.map(_ -> localNames.genTypeName()).toMap

    // free type vars - type mode
    for (ftv <- info.freeTypeVars) {
      if (!freeTypeVarNames.contains(ftv)) {
        freeTypeVarNames = freeTypeVarNames + (ftv -> globalNames.genTypeName())
      }
    }

    for (frtv <- info.freeRowTypeVars) {
      if (!freeRowTypeVarNames.contains(frtv)) {
        freeRowTypeVarNames = freeRowTypeVarNames + (frtv -> globalNames.genTypeName())
      }
    }

    mode match {
      case SpecsMode =>
        assert(freeTypeVarNames.isEmpty)
        assert(freeRowTypeVarNames.isEmpty)
      case TypesMode =>
        assert(sVarNames.isEmpty)
    }

    def typ(sType: STypes.Type): String =
      sType match {
        case STypes.PlainType(vt) =>
          val Types.VarType(typeVar) = vt
          freeTypeVarNames(typeVar)
        case STypes.ConType(tyCon, types, rowTypes) =>
          conType(tyCon, types, rowTypes)
        case STypes.RefType(sTypeVar) =>
          sVarNames(sTypeVar.id)
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
            withBaseVar(sVarNames(sRowTypeVar.id), fields)
        }

      unfold(sRowType, Nil)
    }

    typ(body)
  }
}
