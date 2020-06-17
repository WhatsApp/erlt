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
  val S = Ast
  val A = Absyn

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

  def collectPatVars2(pat: A.Pat): List[String] =
    pat match {
      case A.WildPat =>
        List.empty
      case A.VarPat(v) =>
        List(v)
      case A.TuplePat(tPats) =>
        tPats.map(_.pat1).flatMap(collectPatVars2)
      case A.BoolPat(_) =>
        List.empty
      case A.NumberPat(_) =>
        List.empty
      case A.StringPat(_) =>
        List.empty
      case A.RecordPat(fields, _) =>
        fields.map(_.value.pat1).flatMap(collectPatVars2)
      case A.AndPat(p1, p2) =>
        collectPatVars2(p1.pat1) ++ collectPatVars2(p2.pat1)
      case A.EnumCtrPat(_, _, tPats) =>
        tPats.map(_.pat1).flatMap(collectPatVars2)
      case A.ListPat(tPats) =>
        tPats.map(_.pat1).flatMap(collectPatVars2)
      case A.ConsPat(hPat, tPat) =>
        collectPatVars2(hPat.pat1) ++ collectPatVars2(tPat.pat1)
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

  def freeVars(expr: S.Exp): Set[String] =
    expr match {
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
        clauses
          .map { clause =>
            val argVars = clause.pats.flatMap(collectPatVars)
            val bodyVars = freeVars(clause.exp)
            bodyVars -- argVars
          }
          .reduce(_ ++ _)
      case S.NamedFnExp(varName, clauses) =>
        clauses
          .map { clause =>
            val argVars = clause.pats.flatMap(collectPatVars)
            val bodyVars = freeVars(clause.exp)
            bodyVars -- argVars
          }
          .reduce(_ ++ _) - varName.stringId
      case S.BlockExpr(body) =>
        freeVars(body)
    }

  def funFreeVars(fun: S.Fun): Set[String] = {
    fun.clauses
      .map { clause =>
        val funPatVars = clause.pats.flatMap(collectPatVars)
        val bodyVars = freeVars(clause.exp)
        bodyVars -- funPatVars
      }
      .reduce(_ ++ _)
  }

  def buildSCC(funs: List[S.Fun]): List[List[String]] = {
    if (funs.isEmpty) {
      return List()
    }

    val names = funs.map(_.name.stringId)

    val nameToIndex = names.zipWithIndex.toMap

    val vertices = names.map(SCC.Vertex)
    val nameToUsedVars: Map[String, Set[String]] =
      funs.map { f => (f.name.stringId, funFreeVars(f).filter(names.contains)) }.toMap

    val edges = funs.flatMap { fun =>
      val funVertex = SCC.Vertex(fun.name.stringId)
      nameToUsedVars(fun.name.stringId).toList.map(SCC.Vertex).map(SCC.Edge(funVertex, _))
    }

    val g = SCC.G(vertices.head, vertices, edges)
    val components = SCC.components(g)
    val compNames = components.map { c => c.map(_.label) }

    // each comp is sorted inside
    val compNames1 = compNames.map { c => c.sortBy(nameToIndex) }
    // comps are sorted
    val compNames2 = compNames1.sortBy { c => nameToIndex(c.head) }
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

  def moduleApi(module: String, program: S.Program): ModuleApi = {
    val names =
      (program.enumDefs.map(_.name) ++ program.typeAliases.map(_.name) ++ program.opaques.map(_.name)).toSet
    val enumDefs1 = program.enumDefs.collect {
      case S.EnumDef(name, params, cons) if program.exportTypes((name, params.size)) =>
        val name1 = module + ":" + name
        val cons1 = cons.map {
          case S.EnumCon(cName, tps) =>
            S.EnumCon(cName, tps.map(globalizeType(module, names)))(Pos.NP)
        }
        S.EnumDef(name1, params, cons1)(Pos.NP)
    }
    val aliases1 = program.typeAliases.collect {
      case S.TypeAlias(name, params, tp) if program.exportTypes((name, params.size)) =>
        val name1 = module + ":" + name
        val tp1 = globalizeType(module, names)(tp)
        S.TypeAlias(name1, params, tp1)(Pos.NP)
    }
    val specs1 = program.specs.collect {
      case S.Spec(n: S.LocalFunName, ft @ S.FunType(argTypes, resType)) if program.exports((n.name, n.arity)) =>
        val name1 = new S.RemoteFunName(module, n.name, n.arity)
        val funType1 =
          S.FunType(argTypes.map(globalizeType(module, names)), globalizeType(module, names)(resType))(ft.p)
        S.Spec(name1, funType1)(Pos.NP)
    }
    val opaques = program.opaques.collect {
      case S.Opaque(name, params, _) if program.exportTypes((name, params.size)) =>
        S.TypeId(S.RemoteName(module, name), params.size)
    }
    ModuleApi(enumDefs1, aliases1, specs1, opaques)
  }

  private def globalizeType(module: String, names: Set[String])(tp: S.Type): S.Type =
    tp match {
      case S.TypeVar(_) =>
        tp
      case S.WildTypeVar() =>
        tp
      case S.TupleType(params) =>
        S.TupleType(params.map(globalizeType(module, names)))(tp.p)
      case S.RecordType(fields) =>
        val fields1 = fields.map { f => S.Field(f.label, globalizeType(module, names)(f.value)) }
        S.RecordType(fields1)(tp.p)
      case S.OpenRecordType(fields, rt) =>
        val fields1 = fields.map { f => S.Field(f.label, globalizeType(module, names)(f.value)) }
        S.OpenRecordType(fields1, rt)(tp.p)
      case S.FunType(argTypes, resType) =>
        S.FunType(argTypes.map(globalizeType(module, names)), globalizeType(module, names)(resType))(tp.p)
      case S.ListType(elemType) =>
        S.ListType(globalizeType(module, names)(elemType))(tp.p)
      case S.UserType(name, params) =>
        val name1 = name match {
          case S.LocalName(name) =>
            if (names(name))
              S.RemoteName(module, name)
            else
              S.LocalName(name)
          case _ => name
        }
        S.UserType(name1, params.map(globalizeType(module, names)))(tp.p)
    }

  def normalizeTypes(program: S.Program): S.Program = {
    def normEnumCon(con: S.EnumCon): S.EnumCon =
      con.copy(argTypes = con.argTypes.map(normalizeType(program)))(con.p)
    val enumDefs1 =
      program.enumDefs.map { ed => ed.copy(cons = ed.cons.map(normEnumCon))(ed.p) }
    val typeAliases1 =
      program.typeAliases.map { ta => ta.copy(body = normalizeType(program)(ta.body))(ta.p) }
    val opaques1 =
      program.opaques.map { o => o.copy(body = normalizeType(program)(o.body))(o.p) }
    val specs1 =
      program.specs.map(s => s.copy(funType = normFunType(program, s.funType))(s.p))
    program.copy(
      enumDefs = enumDefs1,
      typeAliases = typeAliases1,
      opaques = opaques1,
      specs = specs1,
    )
  }

  private def normalizeType(program: S.Program)(tp: S.Type): S.Type =
    tp match {
      case S.WildTypeVar() | S.TypeVar(_) => tp
      case S.TupleType(ts) =>
        S.TupleType(ts.map(normalizeType(program)))(tp.p)
      case S.RecordType(fields) =>
        S.RecordType(fields.map(f => S.Field(f.label, normalizeType(program)(f.value))))(tp.p)
      case S.OpenRecordType(fields, rt) =>
        S.OpenRecordType(fields.map(f => S.Field(f.label, normalizeType(program)(f.value))), rt)(tp.p)
      case S.FunType(args, res) =>
        S.FunType(args.map(normalizeType(program)), normalizeType(program)(res))(tp.p)
      case S.ListType(et) =>
        S.ListType(normalizeType(program)(et))(tp.p)
      case S.UserType(tName, ts) =>
        val arity = ts.size
        val name1 = tName match {
          case S.LocalName(n) =>
            val t = new S.LocalFunName(n, arity)
            program.importTypes.get(t) match {
              case Some(rem) =>
                S.RemoteName(rem.module, rem.name)
              case None =>
                tName
            }
          case S.RemoteName(module, name) =>
            if (module == program.module) S.LocalName(name)
            else tName
        }
        S.UserType(name1, ts.map(normalizeType(program)))(tp.p)
    }

  private def normFunType(program: S.Program, tp: S.FunType): S.FunType = {
    val S.FunType(args, res) = tp
    S.FunType(args.map(normalizeType(program)), normalizeType(program)(res))(tp.p)
  }

  def getDeps(program: S.Program): Set[String] = {
    var result = Set.empty[String]
    result ++= getDepsRequire(program.require)
    program.enumDefs.foreach {
      result ++= getDepEnumDef(_)
    }
    program.typeAliases.foreach {
      result ++= getDepTypeAlias(_)
    }
    program.opaques.foreach {
      result ++= getDepOpaque(_)
    }
    program.specs.foreach {
      result ++= getDepSpec(_)
    }
    result ++= getDepImports(program.imports)
    result ++= getDepImports(program.importTypes)
    program.funs.foreach {
      result ++= getDepFun(_)
    }
    result
  }

  private def getDepsRequire(req: S.Require): Set[String] =
    req.modules.toSet

  private def getDepEnumDef(enumDef: S.EnumDef): Set[String] =
    enumDef.cons.flatMap(_.argTypes).map(getDepType).foldLeft(Set.empty[String])(_ ++ _)

  private def getDepTypeAlias(typeAlias: S.TypeAlias): Set[String] =
    getDepType(typeAlias.body)

  private def getDepOpaque(opaque: S.Opaque): Set[String] =
    getDepType(opaque.body)

  private def getDepSpec(spec: S.Spec): Set[String] =
    getDepType(spec.funType)

  private def getDepImports(imports: Map[S.LocalFunName, S.RemoteFunName]): Set[String] =
    imports.values.map(_.module).toSet

  private def getDepFun(fun: S.Fun): Set[String] =
    fun.clauses.map(getDepClause).foldLeft(Set.empty[String])(_ ++ _)

  private def getDepClause(clause: S.Clause): Set[String] = {
    var result = Set.empty[String]
    clause.pats.foreach {
      result ++= getDepPat(_)
    }
    clause.guard.foreach {
      result ++= getDepExp(_)
    }
    result ++= getDepBody(clause.exp)
    result
  }

  private def getDepBody(body: S.Body): Set[String] =
    body.prelude.map(getDepValDef).foldLeft(getDepValDef(body.main))(_ ++ _)

  private def getDepValDef(valDef: S.ValDef): Set[String] =
    getDepPat(valDef.pat) ++ getDepExp(valDef.exp)

  private def getDepPat(pat: S.Pat): Set[String] =
    pat match {
      case S.WildPat() =>
        Set.empty[String]
      case S.VarPat(v) =>
        Set.empty[String]
      case S.TuplePat(pats) =>
        pats.map(getDepPat).foldLeft(Set.empty[String])(_ ++ _)
      case S.RecordPat(fields, _) =>
        fields.map(f => getDepPat(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case S.AndPat(p1, p2) =>
        getDepPat(p1) ++ getDepPat(p2)
      case S.EnumCtrPat(enumName, _, pats) =>
        val ctrDep = enumName match {
          case S.LocalName(_) =>
            Set.empty[String]
          case S.RemoteName(module, _) =>
            Set(module)
        }
        pats.map(getDepPat).foldLeft(ctrDep)(_ ++ _)
      case S.ListPat(pats) =>
        pats.map(getDepPat).foldLeft(Set.empty[String])(_ ++ _)
      case S.ConsPat(hPat, tPat) =>
        getDepPat(hPat) ++ getDepPat(tPat)
      case S.BoolPat(_) | S.NumberPat(_) | S.StringPat(_) =>
        Set.empty[String]
    }

  private def getDepExp(expr: S.Exp): Set[String] =
    expr match {
      case S.IfExp(exp1, exp2, exp3) =>
        getDepExp(exp1) ++ getDepExp(exp2) ++ getDepExp(exp3)
      case S.RecordUpdateExp(exp, delta) =>
        getDepExp(exp) ++ getDepExp(delta)
      case S.BinOpExp(binOp, exp1, exp2) =>
        getDepExp(exp1) ++ getDepExp(exp2)
      case S.UOpExp(uOp, exp) =>
        getDepExp(exp)
      case S.AppExp(head, args) =>
        getDepExp(head) ++ args.flatMap(getDepExp)
      case S.SelExp(exp, label) =>
        getDepExp(exp)
      case S.BoolExp(bool) =>
        Set.empty
      case S.NumberExp(n) =>
        Set.empty
      case S.CharExp(_) =>
        Set.empty
      case S.StringExp(s) =>
        Set.empty
      case S.VarExp(_: S.LocalVarName) =>
        Set.empty
      case S.VarExp(_: S.LocalFunName) =>
        Set.empty
      case S.VarExp(remote: S.RemoteFunName) =>
        Set(remote.module)
      case S.RecordExp(fields) =>
        fields.flatMap(f => getDepExp(f.value)).toSet
      case S.TupleExp(elems) =>
        elems.flatMap(getDepExp).toSet
      case S.EnumConExp(enumName, dataCon, args) =>
        val ctrDep = enumName match {
          case S.LocalName(_) =>
            Set.empty[String]
          case S.RemoteName(module, _) =>
            Set(module)
        }
        args.map(getDepExp).foldLeft(ctrDep)(_ ++ _)
      case S.ListExp(elems) =>
        elems.flatMap(getDepExp).toSet
      case S.ConsExp(h, t) =>
        getDepExp(h) ++ getDepExp(t)
      case S.CaseExp(selector, rules) =>
        val rulesDeps = rules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          bodyDeps ++ patDeps
        }
        val selectorDeps = getDepExp(selector)
        selectorDeps ++ rulesDeps
      case S.FnExp(clauses) =>
        clauses.map(getDepClause).reduce(_ ++ _)
      case S.NamedFnExp(varName, clauses) =>
        clauses.map(getDepClause).reduce(_ ++ _)
      case S.BlockExpr(body) =>
        getDepBody(body)
    }

  private def getDepType(tp: S.Type): Set[String] =
    tp match {
      case S.WildTypeVar() | S.TypeVar(_) =>
        Set.empty[String]
      case S.TupleType(params) =>
        params.map(getDepType).foldLeft(Set.empty[String])(_ ++ _)
      case S.RecordType(fields) =>
        fields.map(f => getDepType(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case S.OpenRecordType(fields, extType) =>
        fields.map(f => getDepType(f.value)).foldLeft(getDepType(extType))(_ ++ _)
      case S.FunType(argTypes, resType) =>
        argTypes.map(getDepType).foldLeft(getDepType(resType))(_ ++ _)
      case S.ListType(elemType) =>
        getDepType(elemType)
      case S.UserType(name, params) =>
        val nameDep = name match {
          case S.LocalName(_) =>
            Set.empty[String]
          case S.RemoteName(module, _) =>
            Set(module)
        }
        params.map(getDepType).foldLeft(nameDep)(_ ++ _)
    }
}
