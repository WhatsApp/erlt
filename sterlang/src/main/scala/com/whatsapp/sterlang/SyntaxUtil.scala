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

object SyntaxUtil {

  def collectPatVars(pat: S.Pat): List[String] =
    pat match {
      case S.WildPat() =>
        List.empty
      case S.BoolPat(_) =>
        List.empty
      case S.NumberPat(_) =>
        List.empty
      case S.StringPat(_) =>
        List.empty
      case S.VarPat(v) =>
        List(v)
      case S.TuplePat(pats) =>
        pats.flatMap(collectPatVars)
      case S.RecordPat(fields, _) =>
        val allPats = fields.map(_.value)
        allPats.flatMap(collectPatVars)
      case S.AndPat(p1, p2) =>
        collectPatVars(p1) ++ collectPatVars(p2)
      case S.EnumCtrPat(_, _, pats) =>
        pats.flatMap(collectPatVars)
      case S.ListPat(pats) =>
        pats.flatMap(collectPatVars)
      case S.ConsPat(hPat, tPat) =>
        collectPatVars(hPat) ++ collectPatVars(tPat)
    }

  def collectNamedTypeVars(t: S.Type): List[String] =
    t match {
      case S.TypeVar(n) =>
        List(n)
      case S.WildTypeVar() =>
        List.empty
      case S.UserType(_, params) =>
        params.flatMap(collectNamedTypeVars)
      case S.TupleType(params) =>
        params.flatMap(collectNamedTypeVars)
      case S.RecordType(fields) =>
        fields.map(_.value).flatMap(collectNamedTypeVars)
      case S.OpenRecordType(fields, _) =>
        fields.map(_.value).flatMap(collectNamedTypeVars)
      case S.FunType(params, res) =>
        params.flatMap(collectNamedTypeVars) ++ collectNamedTypeVars(res)
      case S.ListType(elemType) =>
        collectNamedTypeVars(elemType)
    }

  @scala.annotation.tailrec
  private def freeVars(defs: List[S.ValDef], valDef: S.ValDef, env: Set[String], acc: Set[String]): Set[String] =
    defs match {
      case Nil =>
        val thisFreeVars = freeVars(valDef.exp) -- env
        acc ++ thisFreeVars
      case S.ValDef(pat, exp) :: defs1 =>
        val thisFreeVars = freeVars(exp) -- env
        val deltaEnv = collectPatVars(pat)
        val env1 = env ++ deltaEnv
        val acc1 = acc ++ thisFreeVars
        freeVars(defs1, valDef, env1, acc1)
    }

  private def freeVars(body: S.Body): Set[String] = {
    freeVars(body.prelude, body.main, Set.empty, Set.empty)
  }

  def freeVars(expr: S.Exp): Set[String] = expr match {
    case S.IfExp(exp1, exp2, exp3) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case S.RecordUpdateExp(exp, delta) =>
      freeVars(exp) ++ freeVars(delta)
    case S.BinOpExp(binOp, exp1, exp2) =>
      freeVars(exp1) ++ freeVars(exp2)
    case S.UOpExp(uOp, exp) =>
      freeVars(exp)
    case S.AppExp(head, args) =>
      freeVars(head) ++ args.flatMap(freeVars)
    case S.SelExp(exp, label) =>
      freeVars(exp)
    case S.BoolExp(bool) =>
      Set.empty
    case S.NumberExp(n) =>
      Set.empty
    case S.CharExp(_) =>
      Set.empty
    case S.StringExp(s) =>
      Set.empty
    case S.VarExp(localVar: S.LocalVarName) =>
      Set(localVar.stringId)
    case S.VarExp(localFun: S.LocalFunName) =>
      Set(localFun.stringId)
    case S.VarExp(_: S.RemoteFunName) =>
      Set.empty
    case S.RecordExp(fields) =>
      fields.flatMap(f => freeVars(f.value)).toSet
    case S.TupleExp(elems) =>
      elems.flatMap(freeVars).toSet
    case S.EnumConExp(enumName, dataCon, args) =>
      args.flatMap(freeVars).toSet
    case S.ListExp(elems) =>
      elems.flatMap(freeVars).toSet
    case S.ConsExp(h, t) =>
      freeVars(h) ++ freeVars(t)
    case S.CaseExp(selector, rules) =>
      val rulesVars = rules.flatMap { rule =>
        val patVars = collectPatVars(rule.pat)
        val bodyVars = freeVars(rule.exp)
        bodyVars -- patVars
      }
      val selectorVars = freeVars(selector)
      selectorVars ++ rulesVars
    case S.FnExp(clauses) =>
      clauses.map { clause =>
        val argVars = clause.pats.flatMap(collectPatVars)
        val bodyVars = freeVars(clause.exp)
        bodyVars -- argVars
      }.reduce(_ ++ _)
    case S.NamedFnExp(varName, clauses) =>
      clauses.map { clause =>
        val argVars = clause.pats.flatMap(collectPatVars)
        val bodyVars = freeVars(clause.exp)
        bodyVars -- argVars
      }.reduce(_ ++ _) - varName.stringId
    case S.BlockExpr(body) =>
      freeVars(body)
  }

  def funFreeVars(fun: S.Fun): Set[String] = {
    fun.clauses.map { clause =>
      val funPatVars = clause.pats.flatMap(collectPatVars)
      val bodyVars = freeVars(clause.exp)
      bodyVars -- funPatVars
    }.reduce(_ ++ _)
  }

  def buildSCC(funs: List[S.Fun]): List[List[String]] = {
    if (funs.isEmpty) {
      return List()
    }

    val names = funs.map(_.name.stringId)

    val nameToIndex = names.zipWithIndex.toMap

    val vertices = names.map(SCC.Vertex)
    val nameToUsedVars: Map[String, Set[String]] =
      funs.map{ f => (f.name.stringId, funFreeVars(f).filter(names.contains)) }.toMap

    val edges = funs.flatMap { fun =>
      val funVertex = SCC.Vertex(fun.name.stringId)
      nameToUsedVars(fun.name.stringId).toList.map(SCC.Vertex).map(SCC.Edge(funVertex, _))
    }

    val g = SCC.G(vertices.head, vertices, edges)
    val components = SCC.components(g)
    val compNames = components.map{ c => c.map(_.label) }

    // each comp is sorted inside
    val compNames1 = compNames.map{c => c.sortBy(nameToIndex)}
    // comps are sorted
    val compNames2 = compNames1.sortBy{ c => nameToIndex(c.head) }
    // proper topo order
    properSort(compNames2, nameToUsedVars)
  }

  private def properSort(sccs: List[List[String]], usage: Map[String, Set[String]]): List[List[String]] =
    sccs match {
      case Nil =>
        Nil
      case _ =>
        // find a component which is not used
        val Some(scc) = sccs.find { scc1 =>
          val used = scc1.flatMap(usage).toSet
          sccs.forall { scc2 =>
            (scc1 == scc2) || used.intersect(scc2.toSet).isEmpty
          }
        }
        val rest = sccs.filter(_ != scc)
        scc :: properSort(rest, usage)
    }

}
