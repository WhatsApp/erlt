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
  private val S = Ast
  private val A = Absyn
  private val T = Types
  private val MT = METypes
  private val ST = STypes

  private val U = new Unify(vars)
  private val TU = new TypesUtil(vars)

  private type PEnv = Set[String]

  private case class Function(name: String, clauses: List[S.Clause])(val sourceLocation: Pos.P)

  private def freshTypeVar(d: T.Depth): T.Type =
    T.VarType(vars.tVar(T.Open(d)))

  private def freshRowTypeVar(d: T.Depth, kind: T.RtVarKind = T.empty): T.RowType =
    T.RowVarType(vars.rVar(T.RowOpen(d, kind)))
  private def freshRTypeVar(d: T.Depth)(kind: T.RtVarKind): T.RowTypeVar =
    vars.rVar(T.RowOpen(d, kind))

  def unify(pos: Pos.P, t1: T.Type, t2: T.Type): Unit = {
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

  def elaborateFuns(funs: List[S.Fun]): (List[Absyn.Fun], Env) = {
    val names = funs.map(_.name.stringId)
    val fMap = funs.map { f => f.name.stringId -> f }.toMap
    val sccNames = SyntaxUtil.buildSCC(funs, program.module)
    val sccFuns = sccNames.map(_.map(fMap))
    val (sccFuns1, env) = elabSccFuns(sccFuns)
    val afMap = sccFuns1.flatten.map { f => f.name -> f }.toMap
    val afuns2 = names.map(afMap)
    (afuns2, env)
  }

  private def elabSccFuns(sccFuns: List[List[S.Fun]]): (List[List[A.Fun]], Env) = {
    var envAcc = context.env
    val sccFuns1 = for ((funs, d) <- sccFuns.zipWithIndex) yield {
      val funClauses = funs.map(f => Function(f.name.stringId, f.clauses)(sourceLocation = f.p))
      val (funs1, env1) = elabFuns(funClauses, d, envAcc)
      envAcc = env1
      funs1
    }
    (sccFuns1, envAcc)
  }

  private def elabBody(body: S.Body, ty: T.Type, d: T.Depth, env: Env): A.Body = {
    val S.Body(defs, main) = body
    var envAcc: Env = env
    var d1 = d
    val defs1 = for (S.ValDef(valPat, valBody) <- defs) yield {
      d1 = d1 + 1
      val t = freshTypeVar(d1)
      val body1 = elab(valBody, t, d1, envAcc)
      val (pat1, env1, _) =
        elpat(valPat, t, d1, envAcc, Set.empty, gen = true)
      envAcc = env1
      A.ValDef(pat1, body1, envAcc, d1, t)
    }
    d1 = d1 + 1
    val S.ValDef(valPat, valBody) = main
    val body1 = elab(valBody, ty, d1, envAcc)
    val (pat1, env1, _) =
      elpat(valPat, ty, d1, envAcc, Set.empty, gen = true)
    val def1 = A.ValDef(pat1, body1, env1, d1, ty)
    A.Body(defs1, def1, ty)
  }

  private def elab(exp: S.Exp, ty: T.Type, d: T.Depth, env: Env): A.Exp =
    exp match {
      case caseExp: S.CaseExp =>
        elabCaseExp(caseExp, ty, d, env)
      case ifExp: S.IfExp =>
        elabIfExp(ifExp, ty, d, env)
      case comprehension: S.Comprehension =>
        elabComprehension(comprehension, ty, d, env)
      case bComprehension: S.BComprehension =>
        elabBComprehension(bComprehension, ty, d, env)
      case recordUpdateExp: S.RecordUpdateExp =>
        elabRecordUpdateExp(recordUpdateExp, ty, d, env)
      case eRecordCreate: S.ERecordCreate =>
        elabERecordCreate(eRecordCreate, ty, d, env)
      case eRecordUpdate: S.ERecordUpdate =>
        elabERecordUpdate(eRecordUpdate, ty, d, env)
      case eRecordIndex: S.ERecordIndex =>
        elabERecordIndex(eRecordIndex, ty, d, env)
      case eRecordSelect: S.ERecordSelect =>
        elabERecordSelect(eRecordSelect, ty, d, env)
      case binOpExp: S.BinOpExp =>
        elabBinOpExp(binOpExp, ty, d, env)
      case uopExp: S.UOpExp =>
        elabUOpExp(uopExp, ty, d, env)
      case appExp: S.AppExp =>
        elabAppExp(appExp, ty, d, env)
      case selExp: S.SelExp =>
        elabSelExp(selExp, ty, d, env)
      case boolExp: S.BoolExp =>
        elabBoolExp(boolExp, ty, d, env)
      case numberExp: S.NumberExp =>
        elabNumberExp(numberExp, ty, d, env)
      case charExp: S.CharExp =>
        elabCharExp(charExp, ty, d, env)
      case stringExp: S.StringExp =>
        elabStringExp(stringExp, ty, d, env)
      case varExp: S.VarExp =>
        elabVarExp(varExp, ty, d, env)
      case recordExp: S.RecordExp =>
        elabRecordExp(recordExp, ty, d, env)
      case tupleExp: S.TupleExp =>
        elabTupleExp(tupleExp, ty, d, env)
      case fnExp: S.FnExp =>
        elabFnExp(fnExp, ty, d, env)
      case namedFnExp: S.NamedFnExp =>
        elabNamedFnExp(namedFnExp, ty, d, env)
      case listExp: S.ListExp =>
        elabListExp(listExp, ty, d, env)
      case consExp: S.ConsExp =>
        elabConsExp(consExp, ty, d, env)
      case enumConExp: S.EnumConExp =>
        elabEnumConExp(enumConExp, ty, d, env)
      case blockExp: S.BlockExpr =>
        elabBlockExp(blockExp, ty, d, env)
      case binExp: S.Bin =>
        elabBin(binExp, ty, d, env)
      case tryCatchExp: S.TryCatchExp =>
        elabTryCatchExp(tryCatchExp, ty, d, env)
      case tryOfCatchExp: S.TryOfCatchExp =>
        elabTryOfCatchExp(tryOfCatchExp, ty, d, env)
    }

  private def elpat(p: S.Pat, t: T.Type, d: T.Depth, env: Env, penv: PEnv, gen: Boolean): (A.Pat, Env, PEnv) = {
    val ts =
      if (gen)
        TU.generalize(d)(t)
      else
        // dummy type scheme
        ST.TypeSchema(0, List(), ST.PlainType(t))

    val (p1, env1, penv1) = elpat1(p, ts, d, env, penv, gen)

    // Attach type annotation to the pattern
    assert(p1.isInstanceOf[A.LiteralPat] || p1.typ == null)
    val p2 = p1 match {
      case p: A.WildPat            => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.VarPat             => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.AndPat             => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.LiteralPat         => p
      case p: A.TuplePat           => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.ListPat            => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.RecordPat          => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.ERecordPat         => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.ConsPat            => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.EnumConstructorPat => p.copy()(typ = t, sourceLocation = p.sourceLocation)
      case p: A.BinPat             => p.copy()(typ = t, sourceLocation = p.sourceLocation)
    }
    assert(p2.typ != null)

    (p2, env1, penv1)
  }

  private def elpat1(p: S.Pat, ts: ST.TypeSchema, d: T.Depth, env: Env, penv: PEnv, gen: Boolean): (A.Pat, Env, PEnv) =
    p match {
      case wildPat: S.WildPat =>
        elabWildPat(wildPat, ts, d, env, penv, gen)
      case varPat: S.VarPat =>
        elabVarPat(varPat, ts, d, env, penv, gen)
      case andPat: S.AndPat =>
        elabAndPat(andPat, ts, d, env, penv, gen)
      case tuplePat: S.TuplePat =>
        elabTuplePat(tuplePat, ts, d, env, penv, gen)
      case boolPat: S.BoolPat =>
        elabBoolPat(boolPat, ts, d, env, penv, gen)
      case numberPat: S.NumberPat =>
        elabNumberPat(numberPat, ts, d, env, penv, gen)
      case stringPat: S.StringPat =>
        elabStringPat(stringPat, ts, d, env, penv, gen)
      case recordPat: S.RecordPat =>
        elabRecordPat(recordPat, ts, d, env, penv, gen)
      case enumCtrPat: S.EnumCtrPat =>
        elabEnumCtrPat(enumCtrPat, ts, d, env, penv, gen)
      case eRecordPat: S.ERecordPat =>
        elabERecordPat(eRecordPat, ts, d, env, penv, gen)
      case eRecordIndexPat: S.ERecordIndexPat =>
        elabERecordIndexPat(eRecordIndexPat, ts, d, env, penv, gen)
      case listPat: S.ListPat =>
        elabListPat(listPat, ts, d, env, penv, gen)
      case binPat: S.BinPat =>
        elabBinPat(binPat, ts, d, env, penv, gen)
      case consPat: S.ConsPat =>
        elabConsPat(consPat, ts, d, env, penv, gen)
    }

  private def elabBinOpExp(exp: S.BinOpExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.BinOpExp(op: S.BinOp, e1: S.Exp, e2: S.Exp) = exp

    val (e1Elaborated, e2Elaborated) =
      op match {
        case S.BoolConnOp(_) =>
          unify(exp.p, ty, MT.BoolType)
          (elab(e1, MT.BoolType, d, env), elab(e2, MT.BoolType, d, env))
        case S.Cmp(_) =>
          unify(exp.p, ty, MT.BoolType)
          val operandType = freshTypeVar(d)
          (elab(e1, operandType, d, env), elab(e2, operandType, d, env))
        case S.ListOp(_) =>
          val elemType = freshTypeVar(d)
          val listType = MT.ListType(elemType)
          val res1 = elab(e1, listType, d, env)
          val res2 = elab(e2, listType, d, env)
          unify(exp.p, ty, listType)
          (res1, res2)
        case S.Arith(_) =>
          unify(exp.p, ty, MT.IntType)
          (elab(e1, MT.IntType, d, env), elab(e2, MT.IntType, d, env))
      }

    A.BinOpExp(op, e1Elaborated, e2Elaborated)(typ = ty, sourceLocation = exp.p)
  }

  private def elabUOpExp(exp: S.UOpExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.UOpExp(op: S.UOp, e1: S.Exp) = exp
    val e1Elaborated = op match {
      case S.UMinus | S.UPlus | S.BNot =>
        unify(exp.p, ty, MT.IntType)
        elab(e1, MT.IntType, d, env)
      case S.UNot =>
        unify(exp.p, ty, MT.BoolType)
        elab(e1, MT.BoolType, d, env)
    }
    A.UOpExp(op, e1Elaborated)(typ = ty, sourceLocation = exp.p)
  }

  private def elabCaseExp(exp: S.CaseExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.CaseExp(selector, eRules) = exp

    val t = freshTypeVar(d)
    val selector1 = elab(selector, t, d, env)
    val resType = freshTypeVar(d)

    val branches = for (S.Rule(pat, guards, body) <- eRules) yield {
      val (pat1, env1, _) = elpat(pat, t, d, env, Set.empty, gen = true)
      val guards1 = elabGuards(guards, d, env1)
      val body1 = elabBody(body, resType, d + 1, env1)
      A.Branch(pat1, guards1, body1)
    }

    unify(exp.p, ty, resType)
    A.CaseExp(selector1, branches)(typ = ty, sourceLocation = exp.p)
  }

  private def elabTryCatchExp(exp: S.TryCatchExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.TryCatchExp(tryBody, catchRules, after) = exp

    val resType = freshTypeVar(d)

    def elabCatchRules(rules: List[S.Rule]): List[A.Branch] =
      rules match {
        case Nil => Nil
        case S.Rule(pat, guards, body) :: rest =>
          pat match {
            case S.WildPat() | S.ERecordPat(_, _) =>
              val (pat1, env1, _) = elpat(pat, MT.ExceptionType, d, env, Set.empty, gen = true)
              val guards1 = elabGuards(guards, d, env1)
              val body1 = elabBody(body, resType, d + 1, env1)
              val branch1 = A.Branch(pat1, guards1, body1)
              branch1 :: elabCatchRules(rest)
            case _ =>
              throw new IllegalCatchPattern(pat.p)
          }
      }

    val tryBody1 = elabBody(tryBody, resType, d, env)
    val catchBranches = elabCatchRules(catchRules)
    val after1 = after.map(elabBody(_, freshTypeVar(d), d, env))

    unify(exp.p, ty, resType)

    A.TryCatchExp(tryBody1, catchBranches, after1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabTryOfCatchExp(exp: S.TryOfCatchExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.TryOfCatchExp(tryBody, tryRules, catchRules, after) = exp

    val resType = freshTypeVar(d)
    val tryBodyType = freshTypeVar(d)

    def elabCatchRules(rules: List[S.Rule]): List[A.Branch] =
      rules match {
        case Nil => Nil
        case S.Rule(pat, guards, body) :: rest =>
          pat match {
            case S.WildPat() | S.ERecordPat(_, _) =>
              val (pat1, env1, _) = elpat(pat, MT.ExceptionType, d, env, Set.empty, gen = true)
              val guards1 = elabGuards(guards, d, env1)
              val body1 = elabBody(body, resType, d + 1, env1)
              val branch1 = A.Branch(pat1, guards1, body1)
              branch1 :: elabCatchRules(rest)
            case _ =>
              throw new IllegalCatchPattern(pat.p)
          }
      }

    val tryBody1 = elabBody(tryBody, tryBodyType, d, env)

    val tryBranches = for (S.Rule(pat, guards, body) <- tryRules) yield {
      val (pat1, env1, _) = elpat(pat, tryBodyType, d, env, Set.empty, gen = true)
      val guards1 = elabGuards(guards, d, env1)
      val body1 = elabBody(body, resType, d + 1, env1)
      A.Branch(pat1, guards1, body1)
    }

    val catchBranches = elabCatchRules(catchRules)
    val after1 = after.map(elabBody(_, freshTypeVar(d), d, env))

    unify(exp.p, ty, resType)
    A.TryOfCatchExp(tryBody1, tryBranches, catchBranches, after1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabIfExp(exp: S.IfExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val ifClauses = exp.ifClauses

    val resType = freshTypeVar(d)
    val bodies = for (S.IfClause(guards, body) <- ifClauses) yield {
      for (guard <- guards) {
        for (guardExp <- guard.exprs) {
          elab(guardExp, MT.BoolType, d, env)
        }
      }
      elabBody(body, resType, d, env)
    }

    unify(exp.p, ty, resType)
    A.IfExp(bodies)(typ = ty, sourceLocation = exp.p)
  }

  private def elabComprehension(exp: S.Comprehension, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.Comprehension(template, qualifiers) = exp
    var envAcc = env
    var depth = d

    val qualifiers1: List[A.Qualifier] = qualifiers.map {
      case S.Filter(exp) =>
        A.Filter(elab(exp, MT.BoolType, depth, envAcc))
      case S.Generator(pat, gExp) =>
        val elemType = freshTypeVar(depth)
        val gExp1 = elab(gExp, MT.ListType(elemType), depth, envAcc)
        val (pat1, env1, _) = elpat(pat, elemType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        A.Generator(pat1, gExp1)
      case S.BGenerator(pat, gExp) =>
        val gExp1 = elab(gExp, MT.BinaryType, depth, envAcc)
        val (pat1, env1, _) = elpat(pat, MT.BinaryType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        A.BGenerator(pat1, gExp1)
    }

    val elemType = freshTypeVar(depth)
    val resType = MT.ListType(elemType)
    val template1 = elab(template, elemType, depth, envAcc)

    unify(exp.p, ty, resType)

    A.Comprehension(template1, qualifiers1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabBComprehension(exp: S.BComprehension, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.BComprehension(template, qualifiers) = exp
    var envAcc = env
    var depth = d

    val qualifiers1: List[A.Qualifier] = qualifiers.map {
      case S.Filter(exp) =>
        A.Filter(elab(exp, MT.BoolType, depth, envAcc))
      case S.Generator(pat, gExp) =>
        val elemType = freshTypeVar(depth)
        val gExp1 = elab(gExp, MT.ListType(elemType), depth, envAcc)
        val (pat1, env1, _) = elpat(pat, elemType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        A.Generator(pat1, gExp1)
      case S.BGenerator(pat, gExp) =>
        val gExp1 = elab(gExp, MT.BinaryType, depth, envAcc)
        val (pat1, env1, _) = elpat(pat, MT.BinaryType, d, envAcc, Set.empty, gen = true)
        envAcc = env1
        depth += 1
        A.BGenerator(pat1, gExp1)
    }

    val template1 = elab(template, MT.BinaryType, depth, envAcc)

    unify(exp.p, ty, MT.BinaryType)

    A.BComprehension(template1, qualifiers1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabRecordUpdateExp(exp: S.RecordUpdateExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.RecordUpdateExp(rec, delta) = exp
    checkUniqueFields(delta.p, delta.fields.map(_.label))

    val fields = delta.fields
    val (fieldTypes, fields1) = fields
      .map({ field =>
        val S.Field(label, value) = field
        val fieldType = freshTypeVar(d)
        val value1 = elab(value, fieldType, d, env)
        (T.Field(label, fieldType), A.Field(label, value1))
      })
      .unzip

    val rt = freshRowTypeVar(d, fieldTypes.map(_.label).toSet)

    val recType =
      MT.RecordType(fieldTypes.foldRight(rt) { T.RowFieldType })

    val rec1 = elab(rec, recType, d, env)

    unify(exp.p, ty, recType)
    A.RecordUpdateExp(rec1, fields1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabERecordCreate(exp: S.ERecordCreate, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ERecordCreate(name, fields) = exp
    val eRec = getERecord(exp.p, name)
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))

    checkUniqueFields(exp.p, fields.map(_.label))
    checkRecordFields(fields, eRec)
    checkRecordInit(exp.p, fields, eRec)

    val fieldTypes = eRec.fields.map(f => f.label -> f.value).toMap
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      A.Field(field.label, elab(field.value, eFieldType, d, env))
    }

    val expType = eRec.kind match {
      case Ast.ErlangRecord    => MT.ERecordType(name)
      case Ast.ExceptionRecord => MT.ExceptionType
      case Ast.MessageRecord   => MT.MessageType
    }

    unify(exp.p, ty, expType)

    A.ERecordCreate(name, fields1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabERecordUpdate(exp: S.ERecordUpdate, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ERecordUpdate(rec, name, fields) = exp
    val eRec = getERecord(exp.p, name)

    eRec.kind match {
      case Ast.ErlangRecord =>
      // OK
      case Ast.ExceptionRecord =>
        throw new UnconditionalExceptionUpdate(exp.p, name)
      case Ast.MessageRecord =>
        throw new UnconditionalMessageUpdate(exp.p, name)
    }

    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))

    checkUniqueFields(exp.p, fields.map(_.label))
    checkRecordFields(fields, eRec)

    val fieldTypes = eRec.fields.map(f => f.label -> f.value).toMap
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      A.Field(field.label, elab(field.value, eFieldType, d, env))
    }

    val eRecordType = MT.ERecordType(name)
    val rec1 = elab(rec, eRecordType, d, env)
    unify(exp.p, ty, eRecordType)

    A.ERecordUpdate(rec1, name, fields1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabERecordIndex(exp: S.ERecordIndex, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ERecordIndex(recName, fieldName) = exp
    val eRec = getERecord(exp.p, recName)
    val knownField = eRec.fields.exists(_.label == fieldName)
    if (!knownField) {
      throw new UnknownERecordField(exp.p, recName, fieldName)
    }
    unify(exp.p, ty, MT.IntType)
    A.ERecordIndex(recName, fieldName)(typ = MT.IntType, sourceLocation = exp.p)
  }

  private def elabERecordSelect(exp: S.ERecordSelect, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ERecordSelect(rec, recName, fieldName) = exp
    val eRec = getERecord(exp.p, recName)

    eRec.kind match {
      case Ast.ErlangRecord =>
      // OK
      case Ast.ExceptionRecord =>
        throw new UnconditionalExceptionSelect(exp.p, recName)
      case Ast.MessageRecord =>
        throw new UnconditionalMessageSelect(exp.p, recName)
    }

    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))
    val fieldDef =
      eRec.fields.find(_.label == fieldName) match {
        case Some(f) => f
        case None    => throw new UnknownERecordField(exp.p, recName, fieldName)
      }

    val eRecordType = MT.ERecordType(recName)
    val rec1 = elab(rec, eRecordType, d, env)

    val fieldType = expander.mkType(fieldDef.value, Map.empty)
    val eFieldType = expander.expandType(fieldType)

    unify(exp.p, ty, eFieldType)

    A.ERecordSelect(rec1, recName, fieldName)(typ = ty, sourceLocation = exp.p)
  }

  private def elabAppExp(exp: S.AppExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.AppExp(head, args) = exp

    val argTypes = args.map(_ => freshTypeVar(d))
    val resType = freshTypeVar(d)
    val exp1 = elab(head, MT.FunType(argTypes, resType), d, env)
    val args1 = (args zip argTypes).map { case (e, t) => elab(e, t, d, env) }

    unify(exp.p, ty, resType)
    A.AppExp(exp1, args1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabSelExp(exp: S.SelExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.SelExp(record, label) = exp

    val fieldType = freshTypeVar(d)
    val base = freshRowTypeVar(d, T.single(label))
    val recordType = MT.RecordType(T.RowFieldType(T.Field(label, fieldType), base))
    val record1 = elab(record, recordType, d, env)

    unify(exp.p, ty, fieldType)
    A.RecordSelectionExp(record1, label)(typ = ty, sourceLocation = exp.p)
  }

  private def elabBoolExp(exp: S.BoolExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.BoolExp(b) = exp

    unify(exp.p, ty, MT.BoolType)
    A.LiteralExp(Values.BooleanValue(b))(sourceLocation = exp.p)
  }

  private def elabNumberExp(exp: S.NumberExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.NumberExp(n) = exp

    unify(exp.p, ty, MT.IntType)
    A.LiteralExp(Values.IntegerValue(n))(sourceLocation = exp.p)
  }

  private def elabCharExp(exp: S.CharExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.CharExp(n) = exp

    unify(exp.p, ty, MT.CharType)
    A.LiteralExp(Values.CharValue(n))(sourceLocation = exp.p)
  }

  private def elabStringExp(exp: S.StringExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.StringExp(s) = exp

    unify(exp.p, ty, MT.StringType)
    A.LiteralExp(Values.StringValue(s))(sourceLocation = exp.p)
  }

  private def elabVarExp(exp: S.VarExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.VarExp(v) = exp

    val nv = normalizeVar(v)
    env.get(nv.stringId) match {
      case Some(ts) =>
        val t = TU.instantiate(d, ts)

        unify(exp.p, ty, t)

        A.VarExp(nv.stringId)(typ = t, sourceLocation = exp.p)
      case None =>
        throw new UnboundVar(exp.p, nv.stringId)
    }
  }

  private def elabRecordExp(exp: S.RecordExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.RecordExp(fields) = exp
    checkUniqueFields(exp.p, fields.map(_.label))

    // elaborating fields
    val (fieldTypes, fields1) = fields
      .map({ field =>
        val S.Field(label, value) = field
        val fieldType = freshTypeVar(d)
        val value1 = elab(value, fieldType, d, env)
        (T.Field(label, fieldType), A.Field(label, value1))
      })
      .unzip
    // elaborating ellipsis
    val baseType: T.RowType = T.RowEmptyType
    val recordType = MT.RecordType(fieldTypes.foldRight(baseType)(T.RowFieldType))

    unify(exp.p, ty, recordType)
    A.RecordExp(fields1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabTupleExp(exp: S.TupleExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.TupleExp(elems) = exp

    val elemTypes = elems.map(_ => freshTypeVar(d))
    val elems1 = (elems zip elemTypes).map { case (e, t) => elab(e, t, d, env) }

    unify(exp.p, ty, MT.TupleType(elemTypes))
    A.TupleExp(elems1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabFnExp(exp: S.FnExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.FnExp(clauses) = exp

    val fnType = freshTypeVar(d)
    val clauses1 = elabClauses(clauses, fnType, d, env)

    unify(exp.p, ty, fnType)
    A.FnExp(clauses1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabClauses(clauses: List[S.Clause], ty: T.Type, d: T.Depth, env: Env): List[A.Clause] = {
    val arity = clauses.head.pats.length

    val patTypes = (1 to arity).map(_ => freshTypeVar(d)).toList
    val bodyType = freshTypeVar(d)
    val fnType = MT.FunType(patTypes, bodyType)
    U.unify(fnType, ty)

    for (S.Clause(pats, guards, body) <- clauses) yield {
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
      A.Clause(pats1, guards1, body1)
    }
  }

  private def elabGuards(guards: List[S.Guard], d: T.Depth, env: Env): List[A.Guard] =
    for (guard <- guards) yield {
      A.Guard(expressions = guard.exprs.map(exp => elab(exp, MT.BoolType, d, env)))
    }

  private def elabNamedFnExp(exp: S.NamedFnExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.NamedFnExp(name, clauses) = exp
    val funClause = Function(name.stringId, clauses)(sourceLocation = exp.p)
    val (List(fun1: A.Fun), env1) = elabFuns(List(funClause), d, env)
    val tScheme = env1(name.stringId)
    val t = TU.instantiate(d, tScheme)

    unify(exp.p, ty, t)
    A.NamedFnExp(name.stringId, fun1.clauses)(typ = fun1.typ, sourceLocation = exp.p)
  }

  private def elabListExp(exp: S.ListExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ListExp(elems) = exp

    val elemType = freshTypeVar(d)
    val elems1 = elems.map(elab(_, elemType, d, env))

    unify(exp.p, ty, MT.ListType(elemType))
    A.ListExp(elems1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabBin(exp: S.Bin, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.Bin(elems) = exp
    val elems1 = elems.map(elabBinElement(_, d, env))
    unify(exp.p, ty, MT.BinaryType)
    A.BinExp(elems1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabBinElement(elem: S.BinElement, d: T.Depth, env: Env): A.BinElement = {
    val size1 = elem.size.map(elab(_, MT.IntType, d, env))
    val isStringLiteral = elem.expr match {
      case S.StringExp(_) => true
      case _              => false
    }
    val expType = elem.binElemType match {
      case Some(value) =>
        value match {
          case S.IntegerBinElemType | S.Utf8BinElemType | S.Utf16BinElemType | S.Utf32BinElemType =>
            if (isStringLiteral) MT.StringType else MT.IntType
          case S.FloatBinElemType =>
            MT.FloatType
          case S.BinaryBinElemType | S.BytesBinElemType =>
            MT.BinaryType
          case S.BitstringBinElemType | S.BitsBinElemType =>
            MT.BitstringType
        }
      case None =>
        if (isStringLiteral) MT.StringType else MT.IntType
    }
    val exp1 = elab(elem.expr, expType, d, env)
    A.BinElement(exp1, size1, elem.binElemType)
  }

  private def elabConsExp(exp: S.ConsExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.ConsExp(h, t) = exp

    val elemType = freshTypeVar(d)
    val listType = MT.ListType(elemType)
    val h1 = elab(h, elemType, d, env)
    val t1 = elab(t, listType, d, env)

    unify(exp.p, ty, listType)
    A.ConsExp(h1, t1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabEnumConExp(exp: S.EnumConExp, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val S.EnumConExp(eName, cName, args) = exp
    val nName = normalizeEnumName(eName)
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))

    val enumDef = context.enumDefs.find(_.name == nName.stringId) match {
      case Some(ed) => ed
      case None     => throw new UnknownEnum(exp.p, nName.stringId)
    }
    val sub: Map[String, T.Type] =
      enumDef.params.map(tv => tv.name -> freshTypeVar(d)).toMap

    val typeConParams = enumDef.params.map(p => sub(p.name))
    val namedType = MT.NamedType(enumDef.name, typeConParams)
    val eNamedType = expander.expandType(namedType)
    val arity = args.size

    val enumCon = enumDef.cons.find(con => con.name == cName && con.argTypes.size == arity) match {
      case Some(ec) => ec
      case None     => throw new UnknownEnumCon(exp.p, s"$cName/$arity")
    }

    val args1 = for ((arg, argType) <- args.zip(enumCon.argTypes)) yield {
      val contentType = expander.mkType(argType, sub)
      val eContentType = expander.expandType(contentType)
      elab(arg, eContentType, d, env)
    }

    unify(exp.p, ty, eNamedType)
    A.EnumConstructorExp(eName.stringId, cName, args1)(typ = ty, sourceLocation = exp.p)
  }

  private def elabBlockExp(exp: S.BlockExpr, ty: T.Type, d: T.Depth, env: Env): A.Exp = {
    val body1 = elabBody(exp.body, ty, d, env)
    A.BlockExp(body1)(sourceLocation = exp.p)
  }

  private def createTypeSchema(d: T.Depth)(fun: Function): ST.TypeSchema = {
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))
    def freshSType(): ST.Type = ST.PlainType(freshTypeVar(d))
    val sFunType = context.specs.find(_.name.stringId == fun.name) match {
      case Some(spec) =>
        val specFType = spec.funType
        val sVars = SyntaxUtil.collectNamedTypeVars(specFType)
        val sub = sVars.map { v => v -> freshSType() }.toMap
        val sType = expander.mkSType(specFType, sub)
        expander.expandSType(sType)
      case None =>
        val arity = fun.clauses.head.pats.size
        ST.SFunType((1 to arity).map(_ => freshSType()).toList, freshSType())
    }
    ST.TypeSchema(0, Nil, sFunType)
  }

  private def elabFuns(funs: List[Function], d: T.Depth, env: Env): (List[A.Fun], Env) = {
    val fNames = funs.map(_.name)

    val funSchemas: List[ST.TypeSchema] = funs.map(createTypeSchema(d))
    val envWithFuns = (fNames zip funSchemas).foldLeft(env)(_ + _)
    val funTypes = funSchemas.map(TU.instantiate(d, _))

    val funs1: List[A.Fun] = (funs zip funTypes).map {
      case (fun, funType) =>
        val clauses1 = elabClauses(fun.clauses, funType, d, envWithFuns)
        A.Fun(fun.name, clauses1, funType)(fun.sourceLocation)
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
      p: S.WildPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.WildPat() = p

    (A.WildPat()(typ = null, sourceLocation = p.p), env, penv)
  }

  private def elabVarPat(
      p: S.VarPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.VarPat(v) = p

    if (penv(v)) {
      val t1 = TU.instantiate(d, env(v))
      val t2 = TU.instantiate(d, ts)
      unify(p.p, t1, t2)
      (A.VarPat(v)(typ = null, sourceLocation = p.p), env, penv)
    } else {
      (A.VarPat(v)(typ = null, sourceLocation = p.p), env + (v -> ts), penv + v)
    }
  }

  private def elabAndPat(
      p: S.AndPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.AndPat(p1, p2) = p
    val t = TU.instantiate(d, ts)

    val (pp1, env1, penv1) = elpat(p1, t, d, env, penv, gen)
    val (pp2, env2, penv2) = elpat(p2, t, d, env1, penv1, gen)

    (A.AndPat(pp1, pp2)(typ = null, sourceLocation = p.p), env2, penv2)
  }

  private def elabTuplePat(
      p: S.TuplePat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.TuplePat(pats) = p
    val t = TU.instantiate(d, ts)

    val pat2Type: List[(S.Pat, T.Type)] = pats.map(p => (p, freshTypeVar(d)))

    unify(p.p, t, MT.TupleType(pat2Type.map(_._2)))

    var envAcc = env
    var penvAcc = penv
    val pats1 = for { (p, pt) <- pat2Type } yield {
      val (p1, env1, penv1) = elpat(p, pt, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      p1
    }
    (A.TuplePat(pats1)(null, sourceLocation = p.p), envAcc, penvAcc)
  }

  private def elabBoolPat(
      p: S.BoolPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.BoolPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.p, t, MT.BoolType)
    (A.LiteralPat(Values.BooleanValue(b))(sourceLocation = p.p), env, penv)
  }

  private def elabNumberPat(
      p: S.NumberPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.NumberPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.p, t, MT.IntType)
    (A.LiteralPat(Values.IntegerValue(b))(sourceLocation = p.p), env, penv)
  }

  private def elabStringPat(
      p: S.StringPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.StringPat(b) = p
    val t = TU.instantiate(d, ts)
    unify(p.p, t, MT.StringType)
    (A.LiteralPat(Values.StringValue(b))(sourceLocation = p.p), env, penv)
  }

  private def elabERecordIndexPat(
      p: S.ERecordIndexPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.ERecordIndexPat(recName, fieldName) = p

    val eRec = getERecord(p.p, recName)
    val knownField = eRec.fields.exists(_.label == fieldName)
    if (!knownField) {
      throw new UnknownERecordField(p.p, recName, fieldName)
    }

    val t = TU.instantiate(d, ts)
    unify(p.p, t, MT.IntType)
    (A.LiteralPat(Values.RecordIndexValue(recName, fieldName))(sourceLocation = p.p), env, penv)
  }

  private def elabListPat(
      p: S.ListPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.ListPat(pats) = p
    val t = TU.instantiate(d, ts)
    val elemType = freshTypeVar(d)

    unify(p.p, t, MT.ListType(elemType))

    var envAcc = env
    var penvAcc = penv
    val pats1 = for { pat <- pats } yield {
      val (pat1, env1, penv1) = elpat(pat, elemType, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      pat1
    }

    (A.ListPat(pats1)(typ = null, sourceLocation = p.p), envAcc, penvAcc)
  }

  private def elabBinPat(
      p: S.BinPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.BinPat(elems) = p
    val t = TU.instantiate(d, ts)
    unify(p.p, t, MT.BinaryType)

    var envAcc = env
    var penvAcc = penv

    val elems1 = for { elem <- elems } yield {
      val (elem1, env1, penv1) = elabBinElementPat(elem, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      elem1
    }

    (A.BinPat(elems1)(typ = null, sourceLocation = p.p), envAcc, penvAcc)
  }

  private def elabBinElementPat(
      elem: S.BinElementPat,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.BinElementPat, Env, PEnv) = {
    val size1 = elem.size.map(elab(_, MT.IntType, d, env))
    val isStringLiteral = elem.pat match {
      case S.StringPat(_) => true
      case _              => false
    }
    val expType = elem.binElemType match {
      case Some(value) =>
        value match {
          case S.IntegerBinElemType | S.Utf8BinElemType | S.Utf16BinElemType | S.Utf32BinElemType =>
            if (isStringLiteral) MT.StringType else MT.IntType
          case S.FloatBinElemType =>
            MT.FloatType
          case S.BinaryBinElemType | S.BytesBinElemType =>
            MT.BinaryType
          case S.BitstringBinElemType | S.BitsBinElemType =>
            MT.BitstringType
        }
      case None =>
        if (isStringLiteral) MT.StringType else MT.IntType
    }

    val (pat1, env1, penv1) = elpat(elem.pat, expType, d, env, penv, gen)
    (A.BinElementPat(pat1, size1, elem.binElemType), env1, penv1)
  }

  private def elabConsPat(
      p: S.ConsPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.ConsPat(hPat, tPat) = p
    val t = TU.instantiate(d, ts)
    val elemType = freshTypeVar(d)

    unify(p.p, t, MT.ListType(elemType))

    val (hPat1, env1, penv1) = elpat(hPat, elemType, d, env, penv, gen)
    val (tPat1, env2, penv2) = elpat(tPat, MT.ListType(elemType), d, env1, penv1, gen)
    (A.ConsPat(hPat1, tPat1)(typ = null, sourceLocation = p.p), env2, penv2)
  }

  private def elabRecordPat(
      p: S.RecordPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.RecordPat(fieldPats, open) = p
    checkUniqueFields(p.p, fieldPats.map(_.label))

    val t = TU.instantiate(d, ts)

    val labelPatTypes =
      for { S.Field(lbl, pat) <- fieldPats } yield (lbl, pat, freshTypeVar(d))
    val baseType =
      if (open)
        freshRowTypeVar(d, labelPatTypes.map(_._1).toSet)
      else
        T.RowEmptyType

    val rowType = labelPatTypes.foldRight(baseType) {
      case ((label, _, fieldType), acc) => T.RowFieldType(T.Field(label, fieldType), acc)
    }
    unify(p.p, t, MT.RecordType(rowType))

    var envAcc = env
    var penvAcc = penv
    val fields = for { (l, p, pt) <- labelPatTypes } yield {
      val (p1, env1, penv1) = elpat(p, pt, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      A.Field(l, p1)
    }

    (A.RecordPat(fields, open)(typ = null, sourceLocation = p.p), envAcc, penvAcc)
  }

  private def elabEnumCtrPat(
      p: S.EnumCtrPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))
    val S.EnumCtrPat(eName, cName, pats) = p
    val nName = normalizeEnumName(eName)
    val t = TU.instantiate(d, ts)

    val enumDef = context.enumDefs.find(_.name == nName.stringId) match {
      case Some(definition) => definition
      case None             => throw new UnknownEnum(p.p, nName.stringId)
    }
    val sub = enumDef.params.map(tv => tv.name -> freshTypeVar(d)).toMap
    val typeConParams = enumDef.params.map(p => sub(p.name))
    val namedType = MT.NamedType(enumDef.name, typeConParams)
    val eNamedType = expander.expandType(namedType)
    unify(p.p, t, eNamedType)

    val arity = pats.size
    val enumCon = enumDef.cons.find(con => con.name == cName && con.argTypes.size == arity) match {
      case Some(ec) => ec
      case None     => throw new UnknownEnumCon(p.p, s"$cName/$arity")
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

    (A.EnumConstructorPat(nName.stringId, cName, argPats1)(typ = null, sourceLocation = p.p), envAcc, penvAcc)
  }

  private def elabERecordPat(
      p: S.ERecordPat,
      ts: ST.TypeSchema,
      d: T.Depth,
      env: Env,
      penv: PEnv,
      gen: Boolean,
  ): (A.Pat, Env, PEnv) = {
    val S.ERecordPat(recName, fields) = p
    val eRec = getERecord(p.p, recName)
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))

    checkUniqueFields(p.p, fields.map(_.label))
    checkRecordFields(fields, eRec)

    val t = TU.instantiate(d, ts)

    val expType = eRec.kind match {
      case S.ErlangRecord =>
        MT.ERecordType(recName)
      case S.ExceptionRecord =>
        MT.ExceptionType
      case S.MessageRecord =>
        MT.MessageType
    }

    unify(p.p, t, expType)

    val fieldTypes = eRec.fields.map(f => f.label -> f.value).toMap
    var envAcc = env
    var penvAcc = penv
    val fields1 = for (field <- fields) yield {
      val fieldType = expander.mkType(fieldTypes(field.label), Map.empty)
      val eFieldType = expander.expandType(fieldType)
      val (pat1, env1, penv1) = elpat(field.value, eFieldType, d, envAcc, penvAcc, gen)
      envAcc = env1
      penvAcc = penv1
      A.Field(field.label, pat1)
    }

    (A.ERecordPat(recName, fields1)(typ = null, sourceLocation = p.p), envAcc, penvAcc)
  }

  // --- Some additional checks ---

  private def checkSpec(fName: String, elabSchemaType: ST.TypeSchema, d: Int): Unit = {
    val expander = new Expander(context.aliases, () => freshTypeVar(d), freshRTypeVar(d))
    context.specs.find(_.name.stringId == fName).foreach { spec =>
      val specFType = spec.funType
      val sVars = SyntaxUtil.collectNamedTypeVars(specFType)
      val sub = sVars.map { v => v -> freshTypeVar(d) }.toMap
      val specType = expander.mkType(specFType, sub)
      val eSpecType = expander.expandType(specType)
      val specScheme = TU.generalize(d)(eSpecType)

      val elabNormString =
        new TypePrinter2(vars, None).printScheme(elabSchemaType)
      val specNormString =
        new TypePrinter2(vars, None).printScheme(specScheme)

      if (specNormString != elabNormString) {
        throw new SpecError(spec.p, fName, specNormString, elabNormString)
      }
    }
  }

  private def checkUniqueFields(pos: Pos.P, fields: List[String]): Unit = {
    val uniqueIds = fields.distinct
    if (fields != uniqueIds) {
      val duplicateIds = fields.groupBy(v => v).filter(_._2.size > 1).keys.toList.sorted
      throw new DuplicateFields(pos, duplicateIds)
    }
  }

  private def checkRecordFields(usedFields: List[S.Field[_]], recDef: S.ErlangRecordDef): Unit = {
    val recFields = recDef.fields.map(_.label).toSet
    for (f <- usedFields) {
      if (!recFields(f.label))
        throw new UnknownERecordField(f.p, recDef.name, f.label)
    }
  }

  private def checkRecordInit(pos: Pos.P, fields: List[S.Field[_]], recDef: S.ErlangRecordDef): Unit = {
    val initialized = fields.map(_.label).toSet
    for (f <- recDef.fields) {
      if (!initialized(f.label))
        throw new UnInitializedERecordField(pos, recDef.name, f.label)
    }
  }

  private def normalizeVar(v: S.VarName): S.VarName =
    v match {
      case _: S.LocalVarName => v
      case local: S.LocalFunName =>
        program.imports.getOrElse(local, local)
      case rem: S.RemoteFunName =>
        if (rem.module == program.module) new S.LocalFunName(rem.name, rem.arity)
        else rem
    }

  private def normalizeEnumName(eName: S.Name): S.Name =
    eName match {
      case local: S.LocalName =>
        program.typeMap.getOrElse(local, local)
      case rem: S.RemoteName =>
        if (rem.module == program.module) S.LocalName(rem.name)
        else eName
    }

  private def getERecord(pos: Pos.P, name: String): S.ErlangRecordDef =
    program.erlangRecordDefs.find(_.name == name) match {
      case Some(recDef) => recDef
      case None         => throw new UnknownRecord(pos, name)
    }
}
