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

object AstUtil {
  import Ast._

  private def collectPatVars(pat: Pat): List[String] =
    pat match {
      case WildPat() =>
        List.empty
      case BoolPat(_) =>
        List.empty
      case CharPat(_) =>
        List.empty
      case NumberPat(_) =>
        List.empty
      case StringPat(_) =>
        List.empty
      case VarPat(v) =>
        List(v)
      case TuplePat(pats) =>
        pats.flatMap(collectPatVars)
      case ShapePat(fields) =>
        val allPats = fields.map(_.value)
        allPats.flatMap(collectPatVars)
      case AndPat(p1, p2) =>
        collectPatVars(p1) ++ collectPatVars(p2)
      case EnumPat(_, _, pats) =>
        pats.flatMap(collectPatVars)
      case NilPat() =>
        List.empty
      case ConsPat(hPat, tPat) =>
        collectPatVars(hPat) ++ collectPatVars(tPat)
      case BinPat(elems) =>
        elems.flatMap(elem => collectPatVars(elem.pat))
      case StructPat(_, fields) =>
        val allPats = fields.map(_.value)
        allPats.flatMap(collectPatVars)
    }

  def collectNamedTypeVars(t: Type): List[String] =
    t match {
      case TypeVar(n) =>
        List(n)
      case WildTypeVar() =>
        List.empty
      case UserType(_, params) =>
        params.flatMap(collectNamedTypeVars)
      case TupleType(params) =>
        params.flatMap(collectNamedTypeVars)
      case ShapeType(fields) =>
        fields.map(_.value).flatMap(collectNamedTypeVars)
      case OpenShapeType(fields, _) =>
        fields.map(_.value).flatMap(collectNamedTypeVars)
      case FunType(params, res) =>
        params.flatMap(collectNamedTypeVars) ++ collectNamedTypeVars(res)
      case ListType(elemType) =>
        collectNamedTypeVars(elemType)
      case StructType(_) =>
        List.empty
    }

  def collectNamedRowTypeVars(t: Type): List[(TypeVar, Set[String])] =
    t match {
      case TypeVar(_) | WildTypeVar() | StructType(_) =>
        List.empty
      case UserType(_, params) =>
        params.flatMap(collectNamedRowTypeVars)
      case TupleType(params) =>
        params.flatMap(collectNamedRowTypeVars)
      case FunType(params, res) =>
        (params ++ List(res)).flatMap(collectNamedRowTypeVars)
      case ListType(elemType) =>
        collectNamedRowTypeVars(elemType)
      case ShapeType(fields) =>
        fields.map(_.value).flatMap(collectNamedRowTypeVars)
      case OpenShapeType(fields, Left(WildTypeVar())) =>
        fields.map(_.value).flatMap(collectNamedRowTypeVars)
      case OpenShapeType(fields, Right(tv)) =>
        val item = tv -> fields.map(_.label).toSet
        val other = fields.map(_.value).flatMap(collectNamedRowTypeVars)
        other ++ List(item)
    }

  @scala.annotation.tailrec
  private def freeVars(
      defs: List[ValDef],
      valDef: ValDef,
      env: Set[String],
      acc: Set[String],
      module: String,
  ): Set[String] =
    defs match {
      case Nil =>
        val thisFreeVars = freeVars(valDef.exp, module) -- env
        acc ++ thisFreeVars
      case ValDef(pat, exp) :: defs1 =>
        val thisFreeVars = freeVars(exp, module) -- env
        val deltaEnv = collectPatVars(pat)
        val env1 = env ++ deltaEnv
        val acc1 = acc ++ thisFreeVars
        freeVars(defs1, valDef, env1, acc1, module)
    }

  private def freeVars(body: Body, module: String): Set[String] = {
    freeVars(body.prelude, body.main, Set.empty, Set.empty, module)
  }

  private def freeVars(expr: Exp, m: String): Set[String] =
    expr match {
      case ShapeUpdateExp(exp, delta) =>
        freeVars(exp, m) ++ freeVars(delta, m)
      case BinOpExp(binOp, exp1, exp2) =>
        freeVars(exp1, m) ++ freeVars(exp2, m)
      case UOpExp(uOp, exp) =>
        freeVars(exp, m)
      case AppExp(head, args) =>
        freeVars(head, m) ++ args.flatMap(freeVars(_, m))
      case ShapeSelectExp(exp, label) =>
        freeVars(exp, m)
      case BoolExp(bool) =>
        Set.empty
      case NumberExp(n) =>
        Set.empty
      case CharExp(_) =>
        Set.empty
      case StringExp(s) =>
        Set.empty
      case VarExp(localVar: LocalVarName) =>
        Set(localVar.stringId)
      case VarExp(localFun: LocalFunName) =>
        Set(localFun.stringId)
      case VarExp(remote: RemoteFunName) =>
        if (remote.module == m)
          Set(new LocalFunName(remote.name, remote.arity).stringId)
        else
          Set.empty
      case ShapeCreateExp(fields) =>
        fields.flatMap(f => freeVars(f.value, m)).toSet
      case StructCreate(_, fields) =>
        fields.flatMap(f => freeVars(f.value, m)).toSet
      case StructUpdate(struct, _, fields) =>
        freeVars(struct, m) ++ fields.flatMap(f => freeVars(f.value, m))
      case StructSelect(struct, _, _) =>
        freeVars(struct, m)
      case TupleExp(elems) =>
        elems.flatMap(freeVars(_, m)).toSet
      case EnumExp(enumName, dataCon, args) =>
        args.flatMap(freeVars(_, m)).toSet
      case NilExp() =>
        Set.empty
      case Bin(elems) =>
        elems.flatMap(elem => freeVars(elem.expr, m)).toSet
      case ConsExp(h, t) =>
        freeVars(h, m) ++ freeVars(t, m)
      case CaseExp(selector, rules) =>
        val rulesVars = rules.flatMap { rule =>
          val patVars = collectPatVars(rule.pat)
          val bodyVars = freeVars(rule.exp, m)
          val guardVars = rule.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          bodyVars ++ guardVars -- patVars
        }
        val selectorVars = freeVars(selector, m)
        selectorVars ++ rulesVars
      case IfExp(ifClauses) =>
        ifClauses.flatMap { ifClause =>
          val guardVars = ifClause.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          val expVars = freeVars(ifClause.exp, m)
          expVars ++ guardVars
        }.toSet
      case Comprehension(elem, qualifiers) =>
        var vars: Set[String] = Set.empty
        var patVars: Set[String] = Set.empty
        qualifiers.foreach {
          case Filter(exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
          case Generator(pat, exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
            patVars ++= collectPatVars(pat)
          case BGenerator(pat, exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
            patVars ++= collectPatVars(pat)
        }
        freeVars(elem, m) -- patVars
      case BComprehension(elem, qualifiers) =>
        var vars: Set[String] = Set.empty
        var patVars: Set[String] = Set.empty
        qualifiers.foreach {
          case Filter(exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
          case Generator(pat, exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
            patVars ++= collectPatVars(pat)
          case BGenerator(pat, exp) =>
            vars ++= (freeVars(exp, m) -- patVars)
            patVars ++= collectPatVars(pat)
        }
        freeVars(elem, m) -- patVars
      case FnExp(clauses) =>
        clauses
          .map { clause =>
            val argVars = clause.pats.flatMap(collectPatVars)
            val guardVars = clause.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
            val bodyVars = freeVars(clause.exp, m)
            bodyVars ++ guardVars -- argVars
          }
          .reduce(_ ++ _)
      case NamedFnExp(varName, clauses) =>
        clauses
          .map { clause =>
            val argVars = clause.pats.flatMap(collectPatVars)
            val guardVars = clause.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
            val bodyVars = freeVars(clause.exp, m)
            bodyVars ++ guardVars -- argVars
          }
          .reduce(_ ++ _) - varName.stringId
      case BlockExpr(body) =>
        freeVars(body, m)
      case TryCatchExp(tryBody, catchRules, after) =>
        val tryBodyVars = freeVars(tryBody, m)
        val catchRulesVars = catchRules.flatMap { rule =>
          val patVars = collectPatVars(rule.pat)
          val guardVars = rule.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          val bodyVars = freeVars(rule.exp, m)
          bodyVars ++ guardVars -- patVars
        }
        val afterVars = after.map(freeVars(_, m)).getOrElse(Set.empty)
        tryBodyVars ++ catchRulesVars ++ afterVars
      case TryOfCatchExp(tryBody, tryRules, catchRules, after) =>
        val tryBodyVars = freeVars(tryBody, m)
        val tryRulesVars = tryRules.flatMap { rule =>
          val patVars = collectPatVars(rule.pat)
          val bodyVars = freeVars(rule.exp, m)
          val guardVars = rule.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          bodyVars ++ guardVars -- patVars
        }
        val catchRulesVars = catchRules.flatMap { rule =>
          val patVars = collectPatVars(rule.pat)
          val bodyVars = freeVars(rule.exp, m)
          val guardVars = rule.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          bodyVars ++ guardVars -- patVars
        }
        val afterVars = after.map(freeVars(_, m)).getOrElse(Set.empty)
        tryBodyVars ++ tryRulesVars ++ catchRulesVars ++ afterVars
      case ReceiveExp(rules, after) =>
        val rulesVars = rules.flatMap { rule =>
          val patVars = collectPatVars(rule.pat)
          val bodyVars = freeVars(rule.exp, m)
          val guardVars = rule.guards.flatMap(_.exprs).flatMap(freeVars(_, m))
          bodyVars ++ guardVars -- patVars
        }.toSet
        val afterVars =
          after
            .map { case AfterBody(timeout, body) => freeVars(timeout, m) ++ freeVars(body, m) }
            .getOrElse(Set.empty)
        rulesVars ++ afterVars
    }

  private def funFreeVars(fun: Fun, module: String): Set[String] = {
    fun.clauses
      .map { clause =>
        val funPatVars = clause.pats.flatMap(collectPatVars)
        val bodyVars = freeVars(clause.exp, module)
        val guardVars = clause.guards.flatMap(_.exprs).flatMap(freeVars(_, module))
        bodyVars ++ guardVars -- funPatVars
      }
      .reduce(_ ++ _)
  }

  def buildSCC(funs: List[Fun], module: String): List[List[String]] = {
    if (funs.isEmpty) {
      return List()
    }
    val names = funs.map(_.name.stringId)
    val nameToIndex = names.zipWithIndex.toMap
    val nameToUsedVars: Map[String, Set[String]] =
      funs.map { f =>
        (f.name.stringId, funFreeVars(f, module).filter(names.contains))
      }.toMap
    val edges = funs.flatMap { fun =>
      nameToUsedVars(fun.name.stringId).toList.map(SCC.Edge(fun.name.stringId, _))
    }
    val g = SCC.G(names, edges)
    val compNames = SCC.components(g).map(_.sortBy(nameToIndex)).sortBy(c => nameToIndex(c.head))
    properSort(compNames, nameToUsedVars)
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

  def moduleApi(module: String, program: Program): ModuleApi = {
    val names =
      (program.enumDefs.map(_.name) ++ program.typeAliases.map(_.name) ++ program.opaques.map(_.name)).toSet
    val enumDefs1 = program.enumDefs.collect {
      case EnumDef(name, params, cons) if program.exportTypes((name, params.size)) =>
        val name1 = module + ":" + name
        val cons1 = cons.map {
          case EnumCtr(cName, tps) =>
            EnumCtr(cName, tps.map(globalizeType(module, names)))(Doc.ZRange)
        }
        EnumDef(name1, params, cons1)(Doc.ZRange)
    }
    val aliases1 = program.typeAliases.collect {
      case TypeAlias(name, params, tp) if program.exportTypes((name, params.size)) =>
        val name1 = module + ":" + name
        val tp1 = globalizeType(module, names)(tp)
        TypeAlias(name1, params, tp1)(Doc.ZRange)
    }
    val specs1 = program.specs.collect {
      case Spec(n: LocalFunName, ft @ FunType(argTypes, resType)) if program.exports((n.name, n.arity)) =>
        val name1 = new RemoteFunName(module, n.name, n.arity)
        val funType1 =
          FunType(argTypes.map(globalizeType(module, names)), globalizeType(module, names)(resType))(ft.r)
        Spec(name1, funType1)(Doc.ZRange)
    }
    val opaques = program.opaques.collect {
      case Opaque(name, params, _) if program.exportTypes((name, params.size)) =>
        TypeId(RemoteName(module, name), params.size)
    }
    ModuleApi(enumDefs1, aliases1, specs1, opaques)
  }

  private def globalizeType(module: String, names: Set[String])(tp: Type): Type =
    tp match {
      case TypeVar(_) =>
        tp
      case WildTypeVar() =>
        tp
      case TupleType(params) =>
        TupleType(params.map(globalizeType(module, names)))(tp.r)
      case ShapeType(fields) =>
        val fields1 = fields.map { f => Field(f.label, globalizeType(module, names)(f.value))(f.r) }
        ShapeType(fields1)(tp.r)
      case OpenShapeType(fields, rt) =>
        val fields1 = fields.map { f => Field(f.label, globalizeType(module, names)(f.value))(f.r) }
        OpenShapeType(fields1, rt)(tp.r)
      case FunType(argTypes, resType) =>
        FunType(argTypes.map(globalizeType(module, names)), globalizeType(module, names)(resType))(tp.r)
      case ListType(elemType) =>
        ListType(globalizeType(module, names)(elemType))(tp.r)
      case UserType(name, params) =>
        val name1 = name match {
          case LocalName(name) =>
            if (names(name))
              RemoteName(module, name)
            else
              LocalName(name)
          case _ => name
        }
        UserType(name1, params.map(globalizeType(module, names)))(tp.r)
      case StructType(_) =>
        tp
    }

  def normalizeTypes(program: Program): Program = {
    def normEnumCtr(ctr: EnumCtr): EnumCtr =
      ctr.copy(argTypes = ctr.argTypes.map(normalizeType(program)))(ctr.r)
    val enumDefs1 =
      program.enumDefs.map { ed => ed.copy(ctrs = ed.ctrs.map(normEnumCtr))(ed.r) }
    val typeAliases1 =
      program.typeAliases.map { ta => ta.copy(body = normalizeType(program)(ta.body))(ta.r) }
    val opaques1 =
      program.opaques.map { o => o.copy(body = normalizeType(program)(o.body))(o.r) }
    val structDefs1 =
      program.structDefs.map { structDef =>
        structDef
          .copy(fields = structDef.fields.map(f => Field(f.label, normalizeType(program)(f.value))(f.r)))(structDef.r)
      }
    val specs1 =
      program.specs.map(s => s.copy(funType = normFunType(program, s.funType))(s.r))
    program.copy(
      enumDefs = enumDefs1,
      typeAliases = typeAliases1,
      opaques = opaques1,
      structDefs = structDefs1,
      specs = specs1,
    )
  }

  private def normalizeType(program: Program)(tp: Type): Type =
    tp match {
      case WildTypeVar() | TypeVar(_) | StructType(_) => tp
      case TupleType(ts) =>
        TupleType(ts.map(normalizeType(program)))(tp.r)
      case ShapeType(fields) =>
        ShapeType(fields.map(f => Field(f.label, normalizeType(program)(f.value))(f.r)))(tp.r)
      case OpenShapeType(fields, rt) =>
        OpenShapeType(fields.map(f => Field(f.label, normalizeType(program)(f.value))(f.r)), rt)(tp.r)
      case FunType(args, res) =>
        FunType(args.map(normalizeType(program)), normalizeType(program)(res))(tp.r)
      case ListType(et) =>
        ListType(normalizeType(program)(et))(tp.r)
      case UserType(tName, ts) =>
        val arity = ts.size
        val name1 = tName match {
          case LocalName(n) =>
            val t = new LocalFunName(n, arity)
            program.importTypes.get(t) match {
              case Some(rem) =>
                RemoteName(rem.module, rem.name)
              case None =>
                tName
            }
          case RemoteName(module, name) =>
            if (module == program.module) LocalName(name)
            else tName
        }
        UserType(name1, ts.map(normalizeType(program)))(tp.r)
    }

  private def normFunType(program: Program, tp: FunType): FunType = {
    val FunType(args, res) = tp
    FunType(args.map(normalizeType(program)), normalizeType(program)(res))(tp.r)
  }

  def getDeps(program: Program): Set[String] = {
    var result = Set.empty[String]
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
    result -= program.module
    result
  }

  private def getDepEnumDef(enumDef: EnumDef): Set[String] =
    enumDef.ctrs.flatMap(_.argTypes).map(getDepType).foldLeft(Set.empty[String])(_ ++ _)

  private def getDepTypeAlias(typeAlias: TypeAlias): Set[String] =
    getDepType(typeAlias.body)

  private def getDepOpaque(opaque: Opaque): Set[String] =
    getDepType(opaque.body)

  private def getDepSpec(spec: Spec): Set[String] =
    getDepType(spec.funType)

  private def getDepImports(imports: Map[LocalFunName, RemoteFunName]): Set[String] =
    imports.values.map(_.module).toSet

  private def getDepFun(fun: Fun): Set[String] =
    fun.clauses.map(getDepClause).foldLeft(Set.empty[String])(_ ++ _)

  private def getDepClause(clause: Clause): Set[String] = {
    var result = Set.empty[String]
    clause.pats.foreach {
      result ++= getDepPat(_)
    }
    clause.guards.flatMap(_.exprs).foreach {
      result ++= getDepExp(_)
    }
    result ++= getDepBody(clause.exp)
    result
  }

  private def getDepBody(body: Body): Set[String] =
    body.prelude.map(getDepValDef).foldLeft(getDepValDef(body.main))(_ ++ _)

  private def getDepValDef(valDef: ValDef): Set[String] =
    getDepPat(valDef.pat) ++ getDepExp(valDef.exp)

  private def getDepPat(pat: Pat): Set[String] =
    pat match {
      case WildPat() =>
        Set.empty[String]
      case VarPat(v) =>
        Set.empty[String]
      case TuplePat(pats) =>
        pats.map(getDepPat).foldLeft(Set.empty[String])(_ ++ _)
      case ShapePat(fields) =>
        fields.map(f => getDepPat(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case StructPat(_, fields) =>
        fields.map(f => getDepPat(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case AndPat(p1, p2) =>
        getDepPat(p1) ++ getDepPat(p2)
      case EnumPat(enumName, _, pats) =>
        val ctrDep = enumName match {
          case LocalName(_) =>
            Set.empty[String]
          case RemoteName(module, _) =>
            Set(module)
        }
        pats.map(getDepPat).foldLeft(ctrDep)(_ ++ _)
      case NilPat() =>
        Set.empty[String]
      case BinPat(elems) =>
        elems.map(elem => getDepPat(elem.pat)).foldLeft(Set.empty[String])(_ ++ _)
      case ConsPat(hPat, tPat) =>
        getDepPat(hPat) ++ getDepPat(tPat)
      case BoolPat(_) | NumberPat(_) | StringPat(_) | CharPat(_) =>
        Set.empty[String]
    }

  private def getDepExp(expr: Exp): Set[String] =
    expr match {
      case ShapeUpdateExp(exp, delta) =>
        getDepExp(exp) ++ getDepExp(delta)
      case BinOpExp(binOp, exp1, exp2) =>
        getDepExp(exp1) ++ getDepExp(exp2)
      case UOpExp(uOp, exp) =>
        getDepExp(exp)
      case AppExp(head, args) =>
        getDepExp(head) ++ args.flatMap(getDepExp)
      case ShapeSelectExp(exp, label) =>
        getDepExp(exp)
      case BoolExp(bool) =>
        Set.empty
      case NumberExp(n) =>
        Set.empty
      case CharExp(_) =>
        Set.empty
      case StringExp(s) =>
        Set.empty
      case VarExp(_: LocalVarName) =>
        Set.empty
      case VarExp(_: LocalFunName) =>
        Set.empty
      case VarExp(remote: RemoteFunName) =>
        Set(remote.module)
      case ShapeCreateExp(fields) =>
        fields.flatMap(f => getDepExp(f.value)).toSet
      case StructCreate(_, fields) =>
        fields.flatMap(f => getDepExp(f.value)).toSet
      case StructUpdate(struct, _, fields) =>
        getDepExp(struct) ++ fields.flatMap(f => getDepExp(f.value))
      case StructSelect(struct, _, _) =>
        getDepExp(struct)
      case TupleExp(elems) =>
        elems.flatMap(getDepExp).toSet
      case EnumExp(enumName, ctr, args) =>
        val ctrDep = enumName match {
          case LocalName(_) =>
            Set.empty[String]
          case RemoteName(module, _) =>
            Set(module)
        }
        args.map(getDepExp).foldLeft(ctrDep)(_ ++ _)
      case NilExp() =>
        Set.empty
      case Bin(elems) =>
        elems.flatMap(elem => getDepExp(elem.expr)).toSet
      case ConsExp(h, t) =>
        getDepExp(h) ++ getDepExp(t)
      case CaseExp(selector, rules) =>
        val rulesDeps = rules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          val guardDeps = rule.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ patDeps ++ guardDeps
        }
        val selectorDeps = getDepExp(selector)
        selectorDeps ++ rulesDeps
      case IfExp(ifClauses) =>
        ifClauses.flatMap { ifClause =>
          val bodyDeps = getDepBody(ifClause.exp)
          val guardDeps = ifClause.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ guardDeps
        }.toSet
      case Comprehension(elem, qualifiers) =>
        val elemDeps = getDepExp(elem)
        val qualifierDeps = qualifiers.flatMap {
          case Filter(exp) =>
            getDepExp(exp)
          case Generator(pat, exp) =>
            getDepPat(pat) ++ getDepExp(exp)
          case BGenerator(pat, exp) =>
            getDepPat(pat) ++ getDepExp(exp)
        }
        elemDeps ++ qualifierDeps
      case BComprehension(elem, qualifiers) =>
        val elemDeps = getDepExp(elem)
        val qualifierDeps = qualifiers.flatMap {
          case Filter(exp) =>
            getDepExp(exp)
          case Generator(pat, exp) =>
            getDepPat(pat) ++ getDepExp(exp)
          case BGenerator(pat, exp) =>
            getDepPat(pat) ++ getDepExp(exp)
        }
        elemDeps ++ qualifierDeps
      case FnExp(clauses) =>
        clauses.map(getDepClause).reduce(_ ++ _)
      case NamedFnExp(varName, clauses) =>
        clauses.map(getDepClause).reduce(_ ++ _)
      case BlockExpr(body) =>
        getDepBody(body)
      case TryCatchExp(tryBody, catchRules, after) =>
        val tryBodyDeps = getDepBody(tryBody)
        val catchRulesDeps = catchRules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          val guardDeps = rule.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ patDeps ++ guardDeps
        }
        val afterDeps = after.map(getDepBody).getOrElse(Set.empty)
        tryBodyDeps ++ catchRulesDeps ++ afterDeps
      case TryOfCatchExp(tryBody, tryRules, catchRules, after) =>
        val tryBodyDeps = getDepBody(tryBody)
        val tryRulesDeps = tryRules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          val guardDeps = rule.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ patDeps ++ guardDeps
        }
        val catchRulesDeps = catchRules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          val guardDeps = rule.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ patDeps ++ guardDeps
        }
        val afterDeps = after.map(getDepBody).getOrElse(Set.empty)
        tryBodyDeps ++ tryRulesDeps ++ catchRulesDeps ++ afterDeps
      case ReceiveExp(rules, after) =>
        val rulesDeps = rules.flatMap { rule =>
          val patDeps = getDepPat(rule.pat)
          val bodyDeps = getDepBody(rule.exp)
          val guardDeps = rule.guards.flatMap(_.exprs).flatMap(getDepExp)
          bodyDeps ++ patDeps ++ guardDeps
        }.toSet
        val afterDeps =
          after
            .map { case AfterBody(timeout, body) => getDepExp(timeout) ++ getDepBody(body) }
            .getOrElse(Set.empty)
        rulesDeps ++ afterDeps
    }

  private def getDepType(tp: Type): Set[String] =
    tp match {
      case WildTypeVar() | TypeVar(_) | StructType(_) =>
        Set.empty[String]
      case TupleType(params) =>
        params.map(getDepType).foldLeft(Set.empty[String])(_ ++ _)
      case ShapeType(fields) =>
        fields.map(f => getDepType(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case OpenShapeType(fields, extType) =>
        fields.map(f => getDepType(f.value)).foldLeft(Set.empty[String])(_ ++ _)
      case FunType(argTypes, resType) =>
        argTypes.map(getDepType).foldLeft(getDepType(resType))(_ ++ _)
      case ListType(elemType) =>
        getDepType(elemType)
      case UserType(name, params) =>
        val nameDep = name match {
          case LocalName(_) =>
            Set.empty[String]
          case RemoteName(module, _) =>
            Set(module)
        }
        params.map(getDepType).foldLeft(nameDep)(_ ++ _)
    }
}
