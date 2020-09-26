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

class Elaborate(val vars: Vars, val context: Context, val program: Ast.Program) {
  private val T = Types
  private val MT = METypes
  private val ST = STypes

  private val U = new Unify(vars)
  private val TU = new TypesUtil(vars)

  private type PEnv = Set[String]

  private case class Function(name: String, clauses: List[Ast.Clause])(val r: Doc.Range)

  private def dExpander(d: T.Depth): Expander =
    new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))

  private def freshTypeVar(d: T.Depth): T.Type =
    T.VarType(vars.tVar(T.Open(d)))

  private def freshRowTypeVar(d: T.Depth, kind: T.RtVarKind = T.empty): T.RowType =
    T.RowVarType(vars.rVar(T.RowOpen(d, kind)))
  private def freshRTypeVar(d: T.Depth)(kind: T.RtVarKind): T.RowTypeVar =
    vars.rVar(T.RowOpen(d, kind))

  def unify(pos: Doc.Range, t1: T.Type, t2: T.Type): Unit = {
    val required = TypePrinter2(vars, None).printType(t1)
    val found = TypePrinter2(vars, None).printType(t2)
    try {
      U.unify(t1, t2)
    } catch {
      case Circularity | RowCircularity =>
        throw new InfiniteTypeError(pos, required, found)
      case _: UnifyError =>
        throw new TypeMismatchError(pos, required, found)
    }
  }

  def elaborateFuns(funs: List[Ast.Fun]): (List[AnnAst.Fun], Env) = {
    val names = funs.map(_.name.stringId)
    val fMap = funs.map { f => f.name.stringId -> f }.toMap
    val sccNames = AstUtil.buildSCC(funs, program.module)
    val sccFuns = sccNames.map(_.map(fMap))
    val (sccFuns1, env) = elabSccFuns(sccFuns)
    val afMap = sccFuns1.flatten.map { f => f.name -> f }.toMap
    val afuns2 = names.map(afMap)
    (afuns2, env)
  }

  private def elabSccFuns(sccFuns: List[List[Ast.Fun]]): (List[List[AnnAst.Fun]], Env) = {
    var envAcc = context.env
    val sccFuns1 = for ((funs, d) <- sccFuns.zipWithIndex) yield {
      val funClauses = funs.map(f => Function(f.name.stringId, f.clauses)(r = f.r))
      val (funs1, env1) = elabFuns(funClauses, d, envAcc)
      envAcc = env1
      funs1
    }
    (sccFuns1, envAcc)
  }

  private def elabBody(body: Ast.Body, ty: T.Type, d: T.Depth, env: Env): AnnAst.Body = {
    val Ast.Body(defs, main) = body
    var envAcc: Env = env
    var d1 = d
    val defs1 = for (Ast.ValDef(valPat, valBody) <- defs) yield {
      d1 = d1 + 1
      val t = freshTypeVar(d1)
      val body1 = elab(valBody, t, d1, envAcc)
      val (pat1, env1, _) =
        elpat(valPat, t, d1, envAcc, Set.empty, gen = true)
      envAcc = env1
      AnnAst.ValDef(pat1, body1, envAcc, d1, t)
    }
    d1 = d1 + 1
    val Ast.ValDef(valPat, valBody) = main
    val body1 = elab(valBody, ty, d1, envAcc)
    val (pat1, env1, _) =
      elpat(valPat, ty, d1, envAcc, Set.empty, gen = true)
    val def1 = AnnAst.ValDef(pat1, body1, env1, d1, ty)
    AnnAst.Body(defs1, def1, ty)
  }

  private def elab(exp: Ast.Exp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp =
    exp match {
      case caseExp: Ast.CaseExp =>
        elabCaseExp(caseExp, ty, d, env)
      case ifExp: Ast.IfExp =>
        elabIfExp(ifExp, ty, d, env)
      case comprehension: Ast.Comprehension =>
        elabComprehension(comprehension, ty, d, env)
      case bComprehension: Ast.BComprehension =>
        elabBComprehension(bComprehension, ty, d, env)
      case shapeUpdateExp: Ast.ShapeUpdateExp =>
        elabShapeUpdateExp(shapeUpdateExp, ty, d, env)
      case structCreate: Ast.StructCreate =>
        elabStructCreate(structCreate, ty, d, env)
      case structUpdate: Ast.StructUpdate =>
        elabStructUpdate(structUpdate, ty, d, env)
      case structSelect: Ast.StructSelect =>
        elabStructSelect(structSelect, ty, d, env)
      case binOpExp: Ast.BinOpExp =>
        elabBinOpExp(binOpExp, ty, d, env)
      case uopExp: Ast.UOpExp =>
        elabUOpExp(uopExp, ty, d, env)
      case appExp: Ast.AppExp =>
        elabAppExp(appExp, ty, d, env)
      case shapeSelectExp: Ast.ShapeSelectExp =>
        elabShapeSelectExp(shapeSelectExp, ty, d, env)
      case boolExp: Ast.BoolExp =>
        elabBoolExp(boolExp, ty, d, env)
      case numberExp: Ast.NumberExp =>
        elabNumberExp(numberExp, ty, d, env)
      case charExp: Ast.CharExp =>
        elabCharExp(charExp, ty, d, env)
      case stringExp: Ast.StringExp =>
        elabStringExp(stringExp, ty, d, env)
      case varExp: Ast.VarExp =>
        elabVarExp(varExp, ty, d, env)
      case shapeCreateExp: Ast.ShapeCreateExp =>
        elabShapeCreateExp(shapeCreateExp, ty, d, env)
      case tupleExp: Ast.TupleExp =>
        elabTupleExp(tupleExp, ty, d, env)
      case fnExp: Ast.FnExp =>
        elabFnExp(fnExp, ty, d, env)
      case namedFnExp: Ast.NamedFnExp =>
        elabNamedFnExp(namedFnExp, ty, d, env)
      case listExp: Ast.NilExp =>
        elabListExp(listExp, ty, d, env)
      case consExp: Ast.ConsExp =>
        elabConsExp(consExp, ty, d, env)
      case enumConExp: Ast.EnumConExp =>
        elabEnumConExp(enumConExp, ty, d, env)
      case blockExp: Ast.BlockExpr =>
        elabBlockExp(blockExp, ty, d, env)
      case binExp: Ast.Bin =>
        elabBin(binExp, ty, d, env)
      case tryCatchExp: Ast.TryCatchExp =>
        elabTryCatchExp(tryCatchExp, ty, d, env)
      case tryOfCatchExp: Ast.TryOfCatchExp =>
        elabTryOfCatchExp(tryOfCatchExp, ty, d, env)
      case receiveExp: Ast.ReceiveExp =>
        elabReceiveExp(receiveExp, ty, d, env)
    }

  private def elpat(p: Ast.Pat, t: T.Type, d: T.Depth, env: Env, penv: PEnv, gen: Boolean): (AnnAst.Pat, Env, PEnv) = {
    val ts =
      if (gen)
        TU.generalize(d)(t)
      else
        // dummy type scheme
        ST.TypeSchema(0, List(), ST.PlainType(t))

    val (p1, env1, penv1) = elpat1(p, ts, d, env, penv, gen)

    // Attach type annotation to the pattern
    assert(p1.isInstanceOf[AnnAst.LiteralPat] || p1.typ == null)
    val p2 = p1 match {
      case p: AnnAst.WildPat            => p.copy()(typ = t, r = p.r)
      case p: AnnAst.VarPat             => p.copy()(typ = t, r = p.r)
      case p: AnnAst.AndPat             => p.copy()(typ = t, r = p.r)
      case p: AnnAst.LiteralPat         => p
      case p: AnnAst.TuplePat           => p.copy()(typ = t, r = p.r)
      case p: AnnAst.NilPat             => p.copy()(typ = t, r = p.r)
      case p: AnnAst.ShapePat           => p.copy()(typ = t, r = p.r)
      case p: AnnAst.StructPat          => p.copy()(typ = t, r = p.r)
      case p: AnnAst.ConsPat            => p.copy()(typ = t, r = p.r)
      case p: AnnAst.EnumConstructorPat => p.copy()(typ = t, r = p.r)
      case p: AnnAst.BinPat             => p.copy()(typ = t, r = p.r)
    }
    assert(p2.typ != null)

    (p2, env1, penv1)
  }

  private def elpat1(
      p: Ast.Pat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) =
    p match {
      case wildPat: Ast.WildPat =>
        elabWildPat(wildPat, ts, d, env, penv, gen)
      case varPat: Ast.VarPat =>
        elabVarPat(varPat, ts, d, env, penv, gen)
      case andPat: Ast.AndPat =>
        elabAndPat(andPat, ts, d, env, penv, gen)
      case tuplePat: Ast.TuplePat =>
        elabTuplePat(tuplePat, ts, d, env, penv, gen)
      case boolPat: Ast.BoolPat =>
        elabBoolPat(boolPat, ts, d, env, penv, gen)
      case charPat: Ast.CharPat =>
        elabCharPat(charPat, ts, d, env, penv, gen)
      case numberPat: Ast.NumberPat =>
        elabNumberPat(numberPat, ts, d, env, penv, gen)
      case stringPat: Ast.StringPat =>
        elabStringPat(stringPat, ts, d, env, penv, gen)
      case shapePat: Ast.ShapePat =>
        elabShapePat(shapePat, ts, d, env, penv, gen)
      case enumCtrPat: Ast.EnumCtrPat =>
        elabEnumCtrPat(enumCtrPat, ts, d, env, penv, gen)
      case eRecordPat: Ast.StructPat =>
        elabStructPat(eRecordPat, ts, d, env, penv, gen)
      case listPat: Ast.NilPat =>
        elabListPat(listPat, ts, d, env, penv, gen)
      case binPat: Ast.BinPat =>
        elabBinPat(binPat, ts, d, env, penv, gen)
      case consPat: Ast.ConsPat =>
        elabConsPat(consPat, ts, d, env, penv, gen)
    }

  private def elabBinOpExp(exp: Ast.BinOpExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.BinOpExp(op: Ast.BinOp, e1: Ast.Exp, e2: Ast.Exp) = exp

    val (e1Elaborated, e2Elaborated) =
      op match {
        case Ast.BoolConnOp(_) =>
          unify(exp.r, ty, MT.BoolType)
          (elab(e1, MT.BoolType, d, env), elab(e2, MT.BoolType, d, env))
        case Ast.Cmp(_) =>
          unify(exp.r, ty, MT.BoolType)
          val operandType = freshTypeVar(d)
          (elab(e1, operandType, d, env), elab(e2, operandType, d, env))
        case Ast.ListOp(_) =>
          val elemType = freshTypeVar(d)
          val listType = MT.ListType(elemType)
          val res1 = elab(e1, listType, d, env)
          val res2 = elab(e2, listType, d, env)
          unify(exp.r, ty, listType)
          (res1, res2)
        case Ast.Arith(_) =>
          unify(exp.r, ty, MT.IntType)
          (elab(e1, MT.IntType, d, env), elab(e2, MT.IntType, d, env))
      }

    AnnAst.BinOpExp(op, e1Elaborated, e2Elaborated)(typ = ty, r = exp.r)
  }

  private def elabUOpExp(exp: Ast.UOpExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.UOpExp(op: Ast.UOp, e1: Ast.Exp) = exp
    val e1Elaborated = op match {
      case Ast.UMinus | Ast.UPlus | Ast.BNot =>
        unify(exp.r, ty, MT.IntType)
        elab(e1, MT.IntType, d, env)
      case Ast.UNot =>
        unify(exp.r, ty, MT.BoolType)
        elab(e1, MT.BoolType, d, env)
    }
    AnnAst.UOpExp(op, e1Elaborated)(typ = ty, r = exp.r)
  }

  private def elabCaseExp(exp: Ast.CaseExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.CaseExp(selector, eRules) = exp

    val t = freshTypeVar(d)
    val selector1 = elab(selector, t, d, env)
    val resType = freshTypeVar(d)

    val branches = for (Ast.Rule(pat, guards, body) <- eRules) yield {
      val (pat1, env1, _) = elpat(pat, t, d, env, Set.empty, gen = true)
      val guards1 = elabGuards(guards, d, env1)
      val body1 = elabBody(body, resType, d + 1, env1)
      AnnAst.Branch(pat1, guards1, body1)
    }

    unify(exp.r, ty, resType)
    AnnAst.CaseExp(selector1, branches)(typ = ty, r = exp.r)
  }

  private def elabTryCatchExp(exp: Ast.TryCatchExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.TryCatchExp(tryBody, catchRules, after) = exp

    val resType = freshTypeVar(d)

    def elabCatchRules(rules: List[Ast.Rule]): List[AnnAst.Branch] =
      rules match {
        case Nil => Nil
        case Ast.Rule(pat, guards, body) :: rest =>
          pat match {
            case Ast.WildPat() | Ast.StructPat(_, _) =>
              val (pat1, env1, _) = elpat(pat, MT.ExceptionType, d, env, Set.empty, gen = true)
              val guards1 = elabGuards(guards, d, env1)
              val body1 = elabBody(body, resType, d + 1, env1)
              val branch1 = AnnAst.Branch(pat1, guards1, body1)
              branch1 :: elabCatchRules(rest)
            case _ =>
              throw new IllegalCatchPattern(pat.r)
          }
      }

    val tryBody1 = elabBody(tryBody, resType, d, env)
    val catchBranches = elabCatchRules(catchRules)
    val after1 = after.map(elabBody(_, freshTypeVar(d), d, env))

    unify(exp.r, ty, resType)

    AnnAst.TryCatchExp(tryBody1, catchBranches, after1)(typ = ty, r = exp.r)
  }

  private def elabTryOfCatchExp(exp: Ast.TryOfCatchExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.TryOfCatchExp(tryBody, tryRules, catchRules, after) = exp

    val resType = freshTypeVar(d)
    val tryBodyType = freshTypeVar(d)

    def elabCatchRules(rules: List[Ast.Rule]): List[AnnAst.Branch] =
      rules match {
        case Nil => Nil
        case Ast.Rule(pat, guards, body) :: rest =>
          pat match {
            case Ast.WildPat() | Ast.StructPat(_, _) =>
              val (pat1, env1, _) = elpat(pat, MT.ExceptionType, d, env, Set.empty, gen = true)
              val guards1 = elabGuards(guards, d, env1)
              val body1 = elabBody(body, resType, d + 1, env1)
              val branch1 = AnnAst.Branch(pat1, guards1, body1)
              branch1 :: elabCatchRules(rest)
            case _ =>
              throw new IllegalCatchPattern(pat.r)
          }
      }

    val tryBody1 = elabBody(tryBody, tryBodyType, d, env)

    val tryBranches = for (Ast.Rule(pat, guards, body) <- tryRules) yield {
      val (pat1, env1, _) = elpat(pat, tryBodyType, d, env, Set.empty, gen = true)
      val guards1 = elabGuards(guards, d, env1)
      val body1 = elabBody(body, resType, d + 1, env1)
      AnnAst.Branch(pat1, guards1, body1)
    }

    val catchBranches = elabCatchRules(catchRules)
    val after1 = after.map(elabBody(_, freshTypeVar(d), d, env))

    unify(exp.r, ty, resType)
    AnnAst.TryOfCatchExp(tryBody1, tryBranches, catchBranches, after1)(typ = ty, r = exp.r)
  }

  private def elabReceiveExp(exp: Ast.ReceiveExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.ReceiveExp(rules, after) = exp
    val resType = freshTypeVar(d)

    def elabReceiveRules(rules: List[Ast.Rule]): List[AnnAst.Branch] =
      rules match {
        case Nil => Nil
        case Ast.Rule(pat, guards, body) :: rest =>
          pat match {
            case Ast.WildPat() | Ast.StructPat(_, _) =>
              val (pat1, env1, _) = elpat(pat, MT.MessageType, d, env, Set.empty, gen = true)
              val guards1 = elabGuards(guards, d, env1)
              val body1 = elabBody(body, resType, d + 1, env1)
              val branch1 = AnnAst.Branch(pat1, guards1, body1)
              branch1 :: elabReceiveRules(rest)
            case _ =>
              throw new IllegalReceivePattern(pat.r)
          }
      }
    val receiveBranches = elabReceiveRules(rules)
    val after1 = after.map {
      case Ast.AfterBody(timeout, body) =>
        val timeout1 = elab(timeout, MT.IntType, d, env)
        val body1 = elabBody(body, resType, d, env)
        AnnAst.AfterBody(timeout1, body1)
    }

    unify(exp.r, ty, resType)

    AnnAst.ReceiveExp(receiveBranches, after1)(typ = ty, r = exp.r)
  }

  private def elabIfExp(exp: Ast.IfExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val ifClauses = exp.ifClauses

    val resType = freshTypeVar(d)
    val bodies = for (Ast.IfClause(guards, body) <- ifClauses) yield {
      for (guard <- guards) {
        for (guardExp <- guard.exprs) {
          elab(guardExp, MT.BoolType, d, env)
        }
      }
      elabBody(body, resType, d, env)
    }

    unify(exp.r, ty, resType)
    AnnAst.IfExp(bodies)(typ = ty, r = exp.r)
  }

  private def elabComprehension(exp: Ast.Comprehension, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.Comprehension(template, qualifiers) = exp
    var envAcc = env
    var depth = d

    val qualifiers1: List[AnnAst.Qualifier] = qualifiers.map {
      case Ast.Filter(exp) =>
        AnnAst.Filter(elab(exp, MT.BoolType, depth, envAcc))
      case Ast.Generator(pat, gExp) =>
        val elemType = freshTypeVar(depth)
        val gExp1 = elab(gExp, MT.ListType(elemType), depth, envAcc)
        val (pat1, env1, _) = elpat(pat, elemType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        AnnAst.Generator(pat1, gExp1)
      case Ast.BGenerator(pat, gExp) =>
        val gExp1 = elab(gExp, MT.BinaryType, depth, envAcc)
        val (pat1, env1, _) = elpat(pat, MT.BinaryType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        AnnAst.BGenerator(pat1, gExp1)
    }

    val elemType = freshTypeVar(depth)
    val resType = MT.ListType(elemType)
    val template1 = elab(template, elemType, depth, envAcc)

    unify(exp.r, ty, resType)

    AnnAst.Comprehension(template1, qualifiers1)(typ = ty, r = exp.r)
  }

  private def elabBComprehension(exp: Ast.BComprehension, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.BComprehension(template, qualifiers) = exp
    var envAcc = env
    var depth = d

    val qualifiers1: List[AnnAst.Qualifier] = qualifiers.map {
      case Ast.Filter(exp) =>
        AnnAst.Filter(elab(exp, MT.BoolType, depth, envAcc))
      case Ast.Generator(pat, gExp) =>
        val elemType = freshTypeVar(depth)
        val gExp1 = elab(gExp, MT.ListType(elemType), depth, envAcc)
        val (pat1, env1, _) = elpat(pat, elemType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        AnnAst.Generator(pat1, gExp1)
      case Ast.BGenerator(pat, gExp) =>
        val gExp1 = elab(gExp, MT.BinaryType, depth, envAcc)
        val (pat1, env1, _) = elpat(pat, MT.BinaryType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        AnnAst.BGenerator(pat1, gExp1)
    }

    val template1 = elab(template, MT.BinaryType, depth, envAcc)

    unify(exp.r, ty, MT.BinaryType)

    AnnAst.BComprehension(template1, qualifiers1)(typ = ty, r = exp.r)
  }

  private def elabShapeUpdateExp(exp: Ast.ShapeUpdateExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.ShapeUpdateExp(shape, delta) = exp
    checkUniqueFields(delta.r, delta.fields.map(_.label))

    val fields = delta.fields
    val (fieldTypes, fields1) = fields
      .map({ field =>
        val Ast.Field(label, value) = field
        val fieldType = freshTypeVar(d)
        val value1 = elab(value, fieldType, d, env)
        (T.Field(label, fieldType), AnnAst.Field(label, value1))
      })
      .unzip

    val rt = freshRowTypeVar(d, fieldTypes.map(_.label).toSet)

    val shapeType =
      MT.ShapeType(fieldTypes.foldRight(rt) { T.RowFieldType })

    val shape1 = elab(shape, shapeType, d, env)

    unify(exp.r, ty, shapeType)
    AnnAst.ShapeUpdateExp(shape1, fields1)(typ = ty, r = exp.r)
  }

  private def elabStructCreate(exp: Ast.StructCreate, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.StructCreate(name, fields) = exp
    val structDef = getStructDef(exp.r, name)
    val expander = dExpander(d)

    checkUniqueFields(exp.r, fields.map(_.label))
    checkStructFields(fields, structDef)
    checkStructInit(exp.r, fields, structDef)

    val fieldTypes = structDef.fields.map(f => f.label -> f.value).toMap
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      AnnAst.Field(field.label, elab(field.value, eFieldType, d, env))
    }

    val expType = structDef.kind match {
      case Ast.StrStruct => MT.StructType(name)
      case Ast.ExnStruct => MT.ExceptionType
      case Ast.MsgStruct => MT.MessageType
    }

    unify(exp.r, ty, expType)

    AnnAst.StructCreate(name, fields1)(typ = ty, r = exp.r)
  }

  private def elabStructUpdate(exp: Ast.StructUpdate, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.StructUpdate(struct, name, fields) = exp
    val structDef = getStructDef(exp.r, name)

    structDef.kind match {
      case Ast.StrStruct =>
      // OK
      case Ast.ExnStruct =>
        throw new UnconditionalExceptionUpdate(exp.r, name)
      case Ast.MsgStruct =>
        throw new UnconditionalMessageUpdate(exp.r, name)
    }

    val expander = dExpander(d)

    checkUniqueFields(exp.r, fields.map(_.label))
    checkStructFields(fields, structDef)

    val fieldTypes = structDef.fields.map(f => f.label -> f.value).toMap
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      AnnAst.Field(field.label, elab(field.value, eFieldType, d, env))
    }

    val structType = MT.StructType(name)
    val struct1 = elab(struct, structType, d, env)
    unify(exp.r, ty, structType)

    AnnAst.StructUpdate(struct1, name, fields1)(typ = ty, r = exp.r)
  }

  private def elabStructSelect(exp: Ast.StructSelect, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.StructSelect(struct, name, fieldName) = exp
    val structDef = getStructDef(exp.r, name)

    structDef.kind match {
      case Ast.StrStruct =>
      // OK
      case Ast.ExnStruct =>
        throw new UnconditionalExceptionSelect(exp.r, name)
      case Ast.MsgStruct =>
        throw new UnconditionalMessageSelect(exp.r, name)
    }

    val expander = dExpander(d)
    val fieldDef =
      structDef.fields.find(_.label == fieldName) match {
        case Some(f) => f
        case None    => throw new UnknownStructField(exp.r, name, fieldName)
      }

    val structType = MT.StructType(name)
    val struct1 = elab(struct, structType, d, env)

    val fieldType = expander.mkType(fieldDef.value, Map.empty)
    val eFieldType = expander.expandType(fieldType)

    unify(exp.r, ty, eFieldType)

    AnnAst.StructSelect(struct1, name, fieldName)(typ = ty, r = exp.r)
  }

  private def elabAppExp(exp: Ast.AppExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.AppExp(head, args) = exp

    val argTypes = args.map(_ => freshTypeVar(d))
    val resType = freshTypeVar(d)
    val exp1 = elab(head, MT.FunType(argTypes, resType), d, env)
    val args1 = (args zip argTypes).map { case (e, t) => elab(e, t, d, env) }

    unify(exp.r, ty, resType)
    AnnAst.AppExp(exp1, args1)(typ = ty, r = exp.r)
  }

  private def elabShapeSelectExp(exp: Ast.ShapeSelectExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.ShapeSelectExp(shape, label) = exp

    val fieldType = freshTypeVar(d)
    val base = freshRowTypeVar(d, T.single(label))
    val shapeType = MT.ShapeType(T.RowFieldType(T.Field(label, fieldType), base))
    val shape1 = elab(shape, shapeType, d, env)

    unify(exp.r, ty, fieldType)
    AnnAst.ShapeSelectExp(shape1, label)(typ = ty, r = exp.r)
  }

  private def elabBoolExp(exp: Ast.BoolExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.BoolExp(b) = exp

    unify(exp.r, ty, MT.BoolType)
    AnnAst.LiteralExp(Ast.BooleanVal(b))(typ = ty, r = exp.r)
  }

  private def elabNumberExp(exp: Ast.NumberExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.NumberExp(n) = exp

    unify(exp.r, ty, MT.IntType)
    AnnAst.LiteralExp(Ast.IntVal(n))(typ = ty, r = exp.r)
  }

  private def elabCharExp(exp: Ast.CharExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.CharExp(n) = exp

    unify(exp.r, ty, MT.CharType)
    AnnAst.LiteralExp(Ast.CharVal(n))(typ = ty, r = exp.r)
  }

  private def elabStringExp(exp: Ast.StringExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.StringExp(s) = exp

    unify(exp.r, ty, MT.StringType)
    AnnAst.LiteralExp(Ast.StringVal(s))(typ = ty, r = exp.r)
  }

  private def elabVarExp(exp: Ast.VarExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.VarExp(v) = exp

    val nv = normalizeVar(v)
    env.get(nv.stringId) match {
      case Some(ts) =>
        val t = TU.instantiate(d, ts)

        unify(exp.r, ty, t)

        AnnAst.VarExp(nv.stringId)(typ = t, r = exp.r)
      case None =>
        throw new UnboundVar(exp.r, nv.stringId)
    }
  }

  private def elabShapeCreateExp(exp: Ast.ShapeCreateExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.ShapeCreateExp(fields) = exp
    checkUniqueFields(exp.r, fields.map(_.label))

    // elaborating fields
    val (fieldTypes, fields1) = fields
      .map({ field =>
        val Ast.Field(label, value) = field
        val fieldType = freshTypeVar(d)
        val value1 = elab(value, fieldType, d, env)
        (T.Field(label, fieldType), AnnAst.Field(label, value1))
      })
      .unzip
    // elaborating ellipsis
    val baseType: T.RowType = T.RowEmptyType
    val shapeType = MT.ShapeType(fieldTypes.foldRight(baseType)(T.RowFieldType))

    unify(exp.r, ty, shapeType)
    AnnAst.ShapeCreateExp(fields1)(typ = ty, r = exp.r)
  }

  private def elabTupleExp(exp: Ast.TupleExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.TupleExp(elems) = exp

    val elemTypes = elems.map(_ => freshTypeVar(d))
    val elems1 = (elems zip elemTypes).map { case (e, t) => elab(e, t, d, env) }

    unify(exp.r, ty, MT.TupleType(elemTypes))
    AnnAst.TupleExp(elems1)(typ = ty, r = exp.r)
  }

  private def elabFnExp(exp: Ast.FnExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.FnExp(clauses) = exp

    val fnType = freshTypeVar(d)
    val clauses1 = elabClauses(clauses, fnType, d, env)

    unify(exp.r, ty, fnType)
    AnnAst.FnExp(clauses1)(typ = ty, r = exp.r)
  }

  private def elabClauses(clauses: List[Ast.Clause], ty: T.Type, d: T.Depth, env: Env): List[AnnAst.Clause] = {
    val arity = clauses.head.pats.length

    val patTypes = (1 to arity).map(_ => freshTypeVar(d)).toList
    val bodyType = freshTypeVar(d)
    val fnType = MT.FunType(patTypes, bodyType)
    U.unify(fnType, ty)

    for (Ast.Clause(pats, guards, body) <- clauses) yield {
      var envAcc = env
      var penvAcc: PEnv = Set.empty
      val pats1 = for { (pat, pt) <- pats.zip(patTypes) } yield {
        val (p1, env1, penv1) = elpat(pat, pt, d, envAcc, penvAcc, gen = false)
        envAcc = env1
        penvAcc = penv1
        p1
      }
      val guards1 = elabGuards(guards, d, envAcc)
      val body1 = elabBody(body, bodyType, d + 1, envAcc)
      AnnAst.Clause(pats1, guards1, body1)
    }
  }

  private def elabGuards(guards: List[Ast.Guard], d: T.Depth, env: Env): List[AnnAst.Guard] =
    for (guard <- guards) yield {
      AnnAst.Guard(expressions = guard.exprs.map(exp => elab(exp, MT.BoolType, d, env)))
    }

  private def elabNamedFnExp(exp: Ast.NamedFnExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.NamedFnExp(name, clauses) = exp
    val funClause = Function(name.stringId, clauses)(r = exp.r)
    val (List(fun1: AnnAst.Fun), env1) = elabFuns(List(funClause), d, env)
    val tScheme = env1(name.stringId)
    val t = TU.instantiate(d, tScheme)

    unify(exp.r, ty, t)
    AnnAst.NamedFnExp(name.stringId, fun1.clauses)(typ = fun1.typ, r = exp.r)
  }

  private def elabListExp(exp: Ast.NilExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.NilExp() = exp

    val elemType = freshTypeVar(d)

    unify(exp.r, ty, MT.ListType(elemType))
    AnnAst.NilExp()(typ = ty, r = exp.r)
  }

  private def elabBin(exp: Ast.Bin, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.Bin(elems) = exp
    val elems1 = elems.map(elabBinElement(_, d, env))
    unify(exp.r, ty, MT.BinaryType)
    AnnAst.BinExp(elems1)(typ = ty, r = exp.r)
  }

  private def elabBinElement(elem: Ast.BinElement, d: T.Depth, env: Env): AnnAst.BinElement = {
    val size1 = elem.size.map(elab(_, MT.IntType, d, env))
    val isStringLiteral = elem.expr match {
      case Ast.StringExp(_) => true
      case _                => false
    }
    val expType = elem.binElemType match {
      case Some(value) =>
        value match {
          case Ast.IntegerBinElemType | Ast.Utf8BinElemType | Ast.Utf16BinElemType | Ast.Utf32BinElemType =>
            if (isStringLiteral) MT.StringType else MT.IntType
          case Ast.FloatBinElemType =>
            MT.FloatType
          case Ast.BinaryBinElemType | Ast.BytesBinElemType =>
            MT.BinaryType
          case Ast.BitstringBinElemType | Ast.BitsBinElemType =>
            MT.BitstringType
        }
      case None =>
        if (isStringLiteral) MT.StringType else MT.IntType
    }
    val exp1 = elab(elem.expr, expType, d, env)
    AnnAst.BinElement(exp1, size1, elem.binElemType)
  }

  private def elabConsExp(exp: Ast.ConsExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.ConsExp(h, t) = exp

    val elemType = freshTypeVar(d)
    val listType = MT.ListType(elemType)
    val h1 = elab(h, elemType, d, env)
    val t1 = elab(t, listType, d, env)

    unify(exp.r, ty, listType)
    AnnAst.ConsExp(h1, t1)(typ = ty, r = exp.r)
  }

  private def elabEnumConExp(exp: Ast.EnumConExp, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val Ast.EnumConExp(eName, cName, args) = exp
    val nName = normalizeEnumName(eName)
    val expander = dExpander(d)

    val enumDef = context.enumDefs.find(_.name == nName.stringId) match {
      case Some(ed) => ed
      case None     => throw new UnknownEnum(exp.r, nName.stringId)
    }
    val sub: Map[String, T.Type] =
      enumDef.params.map(tv => tv.name -> freshTypeVar(d)).toMap

    val typeConParams = enumDef.params.map(p => sub(p.name))
    val namedType = MT.NamedType(enumDef.name, typeConParams)
    val eNamedType = expander.expandType(namedType)
    val arity = args.size

    val enumCon = enumDef.cons.find(con => con.name == cName && con.argTypes.size == arity) match {
      case Some(ec) => ec
      case None     => throw new UnknownEnumCon(exp.r, s"$cName/$arity")
    }

    val args1 = for ((arg, argType) <- args.zip(enumCon.argTypes)) yield {
      val contentType = expander.mkType(argType, sub)
      val eContentType = expander.expandType(contentType)
      elab(arg, eContentType, d, env)
    }

    unify(exp.r, ty, eNamedType)
    AnnAst.EnumConstructorExp(eName.stringId, cName, args1)(typ = ty, r = exp.r)
  }

  private def elabBlockExp(exp: Ast.BlockExpr, ty: T.Type, d: T.Depth, env: Env): AnnAst.Exp = {
    val body1 = elabBody(exp.body, ty, d, env)
    AnnAst.BlockExp(body1)(r = exp.r)
  }

  private def createTypeSchema(d: T.Depth)(fun: Function): ST.TypeSchema = {
    val expander = dExpander(d)
    def freshSType(): ST.Type = ST.PlainType(freshTypeVar(d))
    val sFunType = context.specs.find(_.name.stringId == fun.name) match {
      case Some(spec) =>
        val specFType = spec.funType
        val sVars = AstUtil.collectNamedTypeVars(specFType)
        val sub = sVars.map { v => v -> freshSType() }.toMap
        val sType = expander.mkSType(specFType, sub)
        expander.expandSType(sType)
      case None =>
        val arity = fun.clauses.head.pats.size
        ST.SFunType((1 to arity).map(_ => freshSType()).toList, freshSType())
    }
    ST.TypeSchema(0, Nil, sFunType)
  }

  private def elabFuns(funs: List[Function], d: T.Depth, env: Env): (List[AnnAst.Fun], Env) = {
    val fNames = funs.map(_.name)

    val funSchemas: List[ST.TypeSchema] = funs.map(createTypeSchema(d))
    val envWithFuns = (fNames zip funSchemas).foldLeft(env)(_ + _)
    val funTypes = funSchemas.map(TU.instantiate(d, _))

    val funs1: List[AnnAst.Fun] = (funs zip funTypes).map {
      case (fun, funType) =>
        val clauses1 = elabClauses(fun.clauses, funType, d, envWithFuns)
        AnnAst.Fun(fun.name, clauses1, funType)(fun.r)
    }

    // now the elaborated types are generalized back for future use
    // (outside of the bodies of these functions)
    val funSchemas1 = TU.generalize_*(d)(funTypes)

    // checking that provided specs are correct
    funs1.zip(funSchemas1).foreach {
      case (aFun, s) =>
        checkSpec(aFun.name, s, d)
    }

    // the resulting env contains generalized (polymorphic) types
    val env1 = (fNames zip funSchemas1).foldLeft(env)(_ + _)
    (funs1, env1)
  }

  private def elabWildPat(
      p: Ast.WildPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.WildPat() = p

    (AnnAst.WildPat()(typ = null, r = p.r), env, penv)
  }

  private def elabVarPat(
      p: Ast.VarPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.VarPat(v) = p

    if (penv(v)) {
      val t1 = TU.instantiate(d, env(v))
      val t2 = TU.instantiate(d, ts)
      unify(p.r, t1, t2)
      (AnnAst.VarPat(v)(typ = null, r = p.r), env, penv)
    } else {
      (AnnAst.VarPat(v)(typ = null, r = p.r), env + (v -> ts), penv + v)
    }
  }

  private def elabAndPat(
      p: Ast.AndPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.AndPat(p1, p2) = p
    val t = TU.instantiate(d, ts)

    val (pp1, env1, penv1) = elpat(p1, t, d, env, penv, gen)
    val (pp2, env2, penv2) = elpat(p2, t, d, env1, penv1, gen)

    (AnnAst.AndPat(pp1, pp2)(typ = null, r = p.r), env2, penv2)
  }

  private def elabTuplePat(
      p: Ast.TuplePat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.TuplePat(pats) = p
    val t = TU.instantiate(d, ts)

    val pat2Type: List[(Ast.Pat, T.Type)] = pats.map(p => (p, freshTypeVar(d)))

    unify(p.r, t, MT.TupleType(pat2Type.map(_._2)))

    var envAcc = env
    var penvAcc = penv
    val pats1 = for { (p, pt) <- pat2Type } yield {
      val (p1, env1, penv1) = elpat(p, pt, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      p1
    }
    (AnnAst.TuplePat(pats1)(null, r = p.r), envAcc, penvAcc)
  }

  private def elabBoolPat(
      p: Ast.BoolPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.BoolPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.r, t, MT.BoolType)
    (AnnAst.LiteralPat(Ast.BooleanVal(b))(typ = t, r = p.r), env, penv)
  }

  private def elabCharPat(
      p: Ast.CharPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.CharPat(c) = p
    val t = TU.instantiate(d, ts)
    unify(p.r, t, MT.CharType)
    (AnnAst.LiteralPat(Ast.CharVal(c))(typ = t, r = p.r), env, penv)
  }

  private def elabNumberPat(
      p: Ast.NumberPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.NumberPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.r, t, MT.IntType)
    (AnnAst.LiteralPat(Ast.IntVal(b))(typ = t, r = p.r), env, penv)
  }

  private def elabStringPat(
      p: Ast.StringPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.StringPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.r, t, MT.StringType)
    (AnnAst.LiteralPat(Ast.StringVal(b))(typ = t, r = p.r), env, penv)
  }

  private def elabListPat(
      p: Ast.NilPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.NilPat() = p
    val t = TU.instantiate(d, ts)
    val elemType = freshTypeVar(d)

    unify(p.r, t, MT.ListType(elemType))
    (AnnAst.NilPat()(typ = null, r = p.r), env, penv)
  }

  private def elabBinPat(
      p: Ast.BinPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.BinPat(elems) = p
    val t = TU.instantiate(d, ts)
    unify(p.r, t, MT.BinaryType)

    var envAcc = env
    var penvAcc = penv

    val elems1 = for { elem <- elems } yield {
      val (elem1, env1, penv1) = elabBinElementPat(elem, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      elem1
    }

    (AnnAst.BinPat(elems1)(typ = null, r = p.r), envAcc, penvAcc)
  }

  private def elabBinElementPat(
      elem: Ast.BinElementPat,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.BinElementPat, Env, PEnv) = {
    val size1 = elem.size.map(elab(_, MT.IntType, d, env))
    val isStringLiteral = elem.pat match {
      case Ast.StringPat(_) => true
      case _                => false
    }
    val expType = elem.binElemType match {
      case Some(value) =>
        value match {
          case Ast.IntegerBinElemType | Ast.Utf8BinElemType | Ast.Utf16BinElemType | Ast.Utf32BinElemType =>
            if (isStringLiteral) MT.StringType else MT.IntType
          case Ast.FloatBinElemType =>
            MT.FloatType
          case Ast.BinaryBinElemType | Ast.BytesBinElemType =>
            MT.BinaryType
          case Ast.BitstringBinElemType | Ast.BitsBinElemType =>
            MT.BitstringType
        }
      case None =>
        if (isStringLiteral) MT.StringType else MT.IntType
    }

    val (pat1, env1, penv1) = elpat(elem.pat, expType, d, env, penv, gen)
    (AnnAst.BinElementPat(pat1, size1, elem.binElemType), env1, penv1)
  }

  private def elabConsPat(
      p: Ast.ConsPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.ConsPat(hPat, tPat) = p
    val t = TU.instantiate(d, ts)
    val elemType = freshTypeVar(d)

    unify(p.r, t, MT.ListType(elemType))

    val (hPat1, env1, penv1) = elpat(hPat, elemType, d, env, penv, gen)
    val (tPat1, env2, penv2) = elpat(tPat, MT.ListType(elemType), d, env1, penv1, gen)
    (AnnAst.ConsPat(hPat1, tPat1)(typ = null, r = p.r), env2, penv2)
  }

  private def elabShapePat(
      p: Ast.ShapePat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.ShapePat(fieldPats) = p
    checkUniqueFields(p.r, fieldPats.map(_.label))

    val t = TU.instantiate(d, ts)

    val labelPatTypes =
      for { Ast.Field(lbl, pat) <- fieldPats } yield (lbl, pat, freshTypeVar(d))
    val baseType = freshRowTypeVar(d, labelPatTypes.map(_._1).toSet)

    val rowType = labelPatTypes.foldRight(baseType) {
      case ((label, _, fieldType), acc) => T.RowFieldType(T.Field(label, fieldType), acc)
    }
    unify(p.r, t, MT.ShapeType(rowType))

    var envAcc = env
    var penvAcc = penv
    val fields = for { (l, p, pt) <- labelPatTypes } yield {
      val (p1, env1, penv1) = elpat(p, pt, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      AnnAst.Field(l, p1)
    }

    (AnnAst.ShapePat(fields)(typ = null, r = p.r), envAcc, penvAcc)
  }

  private def elabEnumCtrPat(
      p: Ast.EnumCtrPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val expander = dExpander(d)
    val Ast.EnumCtrPat(eName, cName, pats) = p
    val nName = normalizeEnumName(eName)
    val t = TU.instantiate(d, ts)

    val enumDef = context.enumDefs.find(_.name == nName.stringId) match {
      case Some(definition) => definition
      case None             => throw new UnknownEnum(p.r, nName.stringId)
    }
    val sub = enumDef.params.map(tv => tv.name -> freshTypeVar(d)).toMap
    val typeConParams = enumDef.params.map(p => sub(p.name))
    val namedType = MT.NamedType(enumDef.name, typeConParams)
    val eNamedType = expander.expandType(namedType)
    unify(p.r, t, eNamedType)

    val arity = pats.size
    val enumCon = enumDef.cons.find(con => con.name == cName && con.argTypes.size == arity) match {
      case Some(ec) => ec
      case None     => throw new UnknownEnumCon(p.r, s"$cName/$arity")
    }
    var envAcc = env
    var penvAcc = penv
    val argPats1 = for ((argType, pat) <- enumCon.argTypes.zip(pats)) yield {
      val contentType = expander.mkType(argType, sub)
      val eContentType = expander.expandType(contentType)
      val (pat1, env1, penv1) = elpat(pat, eContentType, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      pat1
    }

    (AnnAst.EnumConstructorPat(nName.stringId, cName, argPats1)(typ = null, r = p.r), envAcc, penvAcc)
  }

  private def elabStructPat(
      p: Ast.StructPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (AnnAst.Pat, Env, PEnv) = {
    val Ast.StructPat(name, fields) = p
    val structDef = getStructDef(p.r, name)
    val expander = dExpander(d)

    checkUniqueFields(p.r, fields.map(_.label))
    checkStructFields(fields, structDef)

    val t = TU.instantiate(d, ts)

    val expType = structDef.kind match {
      case Ast.StrStruct =>
        MT.StructType(name)
      case Ast.ExnStruct =>
        MT.ExceptionType
      case Ast.MsgStruct =>
        MT.MessageType
    }

    unify(p.r, t, expType)

    val fieldTypes = structDef.fields.map(f => f.label -> f.value).toMap
    var envAcc = env
    var penvAcc = penv
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      val (pat1, env1, penv1) = elpat(field.value, eFieldType, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      AnnAst.Field(field.label, pat1)
    }

    (AnnAst.StructPat(name, fields1)(typ = null, r = p.r), envAcc, penvAcc)
  }

  // --- Some additional checks ---

  private def checkSpec(fName: String, elabSchemaType: ST.TypeSchema, d: Int): Unit = {
    val expander = dExpander(d)
    context.specs.find(_.name.stringId == fName).foreach { spec =>
      val specFType = spec.funType
      val sVars = AstUtil.collectNamedTypeVars(specFType)
      val sub = sVars.map { v => v -> freshTypeVar(d) }.toMap
      val specType = expander.mkType(specFType, sub)
      val eSpecType = expander.expandType(specType)
      val specScheme = TU.generalize(d)(eSpecType)

      val elabNormString =
        new TypePrinter2(vars, None).printScheme(elabSchemaType)
      val specNormString =
        new TypePrinter2(vars, None).printScheme(specScheme)

      if (specNormString != elabNormString) {
        throw new SpecError(spec.r, fName, specNormString, elabNormString)
      }
    }
  }

  private def checkUniqueFields(pos: Doc.Range, fields: List[String]): Unit = {
    val uniqueIds = fields.distinct
    if (fields != uniqueIds) {
      val duplicateIds = fields.groupBy(v => v).filter(_._2.size > 1).keys.toList.sorted
      throw new DuplicateFields(pos, duplicateIds)
    }
  }

  private def checkStructFields(usedFields: List[Ast.Field[_]], structDef: Ast.StructDef): Unit = {
    val structFields = structDef.fields.map(_.label).toSet
    for (f <- usedFields) {
      if (!structFields(f.label))
        throw new UnknownStructField(f.r, structDef.name, f.label)
    }
  }

  private def checkStructInit(pos: Doc.Range, fields: List[Ast.Field[_]], structDef: Ast.StructDef): Unit = {
    val initialized = fields.map(_.label).toSet
    for (f <- structDef.fields) {
      if (!initialized(f.label))
        throw new UnInitializedStructField(pos, structDef.name, f.label)
    }
  }

  private def normalizeVar(v: Ast.VarName): Ast.VarName =
    v match {
      case _: Ast.LocalVarName => v
      case local: Ast.LocalFunName =>
        program.imports.getOrElse(local, local)
      case rem: Ast.RemoteFunName =>
        if (rem.module == program.module) new Ast.LocalFunName(rem.name, rem.arity)
        else rem
    }

  private def normalizeEnumName(eName: Ast.Name): Ast.Name =
    eName match {
      case local: Ast.LocalName =>
        program.typeMap.getOrElse(local, local)
      case rem: Ast.RemoteName =>
        if (rem.module == program.module) Ast.LocalName(rem.name)
        else eName
    }

  private def getStructDef(pos: Doc.Range, name: String): Ast.StructDef =
    program.structDefs.find(_.name == name) match {
      case Some(structDef) => structDef
      case None            => throw new UnknownStruct(pos, name)
    }
}
