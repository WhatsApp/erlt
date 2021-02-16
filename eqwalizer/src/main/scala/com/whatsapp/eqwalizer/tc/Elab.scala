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

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.{BinarySpecifiers, RecursiveFuns, Id, Vars}
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

final class Elab(module: String, check: Check) {
  private val elabPat = new ElabPat(module)
  private val elabGuard = new ElabGuard(module)
  private val recursiveFuns = new RecursiveFuns(module)

  // TODO: keep this cache in a singleton object somewhere
  var toBaseReturnTy = Map[FoonType, Type]()

  def elabBlock(exprs: List[Expr], env: Env): (Type, Env) = {
    var (elabType, envAcc) = elabExpr(exprs.head, env)
    for (expr <- exprs.tail) {
      val (t1, env1) = elabExpr(expr, envAcc)
      elabType = t1
      envAcc = env1
    }
    (elabType, envAcc)
  }

  private def elabClause(clause: Clause, env0: Env, exportedVars: Set[String]): (Type, Env) = {
    val allScopeVars = Vars.clauseVars(clause)
    val env1 = Util.enterScope(env0, allScopeVars)
    val argTypes = List.fill(clause.pats.size)(AnyType)
    val env2 = elabGuard.elabGuards(clause.guards, env1)
    val (_, env3) = elabPat.elabPats(clause.pats, argTypes, env2)
    val (eType, env4) = elabBlock(clause.body, env3)
    (eType, Util.exitScope(env0, env4, exportedVars))
  }

  def elabExpr(expr: Expr, env: Env): (Type, Env) =
    expr match {
      case Var(v) =>
        (env(v), env)
      case AtomLit(a) =>
        (AtomLitType(a), env)
      case NumberLit() =>
        (NumberType, env)
      case Tuple(elems) =>
        var envAcc = env
        val elemTypes = elems.map { elem =>
          val (eType, env1) = elabExpr(elem, envAcc)
          envAcc = env1
          eType
        }
        (TupleType(elemTypes), envAcc)
      case NilLit() =>
        (NilType, env)
      case Cons(head, tail) =>
        val (headT, env1) = elabExpr(head, env)
        val (tailT, env2) = elabExpr(tail, env1)
        if (!Subtype.subType(tailT, ListType(AnyType))) {
          throw TypeMismatch(expr.l, tail, expected = ListType(AnyType), got = tailT)
        } else {
          val resType = Approx.asListType(tailT) match {
            case Some(ListType(t)) => ListType(Subtype.join(headT, t))
            case None              => headT
          }
          (resType, env2)
        }
      case LocalCall(id, args) =>
        Util.getFunType(module, id) match {
          case Some(ft: FoonType) =>
            elabFoonCall(expr, Some(id), ft, args, env)
          case Some(FunType(argTys, resTy)) =>
            val env1 = check.checkExprs(args, argTys, env)
            (resTy, env1)
          case None =>
            throw UnboundVar(expr.l, id.toString)
        }
      case RemoteCall(fqn, args) =>
        Util.getFunType(fqn) match {
          case Some(ft: FoonType) =>
            elabFoonCall(expr, Some(Id(fqn.module, fqn.arity)), ft, args, env)
          case Some(FunType(argTys, resTy)) =>
            val env1 = check.checkExprs(args, argTys, env)
            (resTy, env1)
          case None =>
            throw UnboundVar(expr.l, fqn.toString)
        }
      case FunCall(expr, args) =>
        val (exprTy, env1) = elabExpr(expr, env)
        exprTy match {
          case ft: FoonType => elabFoonCall(expr, None, ft, args, env)
          case FunType(fArgTys, fResTy) =>
            val env2 = check.checkExprs(args, fArgTys, env1)
            (fResTy, env2)
          case _ =>
            val funTy = FunType(args.map(_ => AnyType), AnyType)
            throw TypeMismatch(expr.l, expr, expected = funTy, got = exprTy)
        }

      case LocalFun(id) =>
        Util.getFunType(module, id) match {
          case Some(ft) =>
            (ft, env)
          case None =>
            throw UnboundVar(expr.l, id.toString)
        }
      case RemoteFun(fqn) =>
        Util.getFunType(fqn) match {
          case Some(ft) =>
            (ft, env)
          case None =>
            throw UnboundVar(expr.l, fqn.toString)
        }
      case Block(block) =>
        elabBlock(block, env)
      case Case(sel, clauses) =>
        val (_, env1) = elabExpr(sel, env)
        val effVars = Vars.clausesVars(clauses)
        val (ts, envs) = clauses.map(elabClause(_, env1, effVars)).unzip
        (UnionType(ts), Approx.joinEnvs(envs))
      case If(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        val (ts, envs) = clauses.map(elabClause(_, env, effVars)).unzip
        (UnionType(ts), Approx.joinEnvs(envs))
      case Match(mPat, mExp) =>
        val (ty, env1) = elabExpr(mExp, env)
        elabPat.elabPat(mPat, ty, env1)
      case UnOp(op, arg) =>
        op match {
          case "not" =>
            val env1 = check.checkExpr(arg, booleanType, env)
            (booleanType, env1)
          case "bnot" | "+" | "-" =>
            val env1 = check.checkExpr(arg, NumberType, env)
            (NumberType, env1)
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException()
          // $COVERAGE-ON$
        }
      case BinOp(op, arg1, arg2) =>
        op match {
          case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
            val env1 = check.checkExpr(arg1, NumberType, env)
            val env2 = check.checkExpr(arg2, NumberType, env1)
            (NumberType, env2)
          case "or" | "and" | "xor" =>
            val env1 = check.checkExpr(arg1, booleanType, env)
            val env2 = check.checkExpr(arg2, booleanType, env1)
            (booleanType, env2)
          case "andalso" | "orelse" =>
            val env1 = check.checkExpr(arg1, booleanType, env)
            val (t2, env2) = elabExpr(arg2, env1)
            (Subtype.join(booleanType, t2), env2)
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException()
          // $COVERAGE-ON$
        }
      case Binary(elems) =>
        var envAcc = env
        for { elem <- elems } {
          val (_, env1) = elabBinaryElem(elem, envAcc)
          envAcc = env1
        }
        (BinaryType, envAcc)
      case Catch(cExpr) =>
        val (_, env1) = elabExpr(cExpr, env)
        (AnyType, env1)
      case TryCatchExpr(tryBody, catchClauses, afterBody) =>
        val (tryT, _) = elabBlock(tryBody, env)
        val (catchTs, _) = catchClauses.map(elabClause(_, env, Set.empty)).unzip
        val env1 = afterBody match {
          case Some(block) => elabBlock(block, env)._2
          case None        => env
        }
        (UnionType(tryT :: catchTs), env1)
      case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
        val (_, tryEnv) = elabBlock(tryBody, env)
        val (tryTs, _) = tryClauses.map(elabClause(_, tryEnv, Set.empty)).unzip
        val (catchTs, _) = catchClauses.map(elabClause(_, env, Set.empty)).unzip
        val env1 = afterBody match {
          case Some(block) => elabBlock(block, env)._2
          case None        => env
        }
        (UnionType(tryTs ::: catchTs), env1)
      case Receive(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        val (ts, envs) = clauses.map(elabClause(_, env, effVars)).unzip
        (UnionType(ts), Approx.joinEnvs(envs))
      case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
        val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
        val (ts, envs) = clauses.map(elabClause(_, env, effVars)).unzip
        val env1 = check.checkExpr(timeout, integerType, env)
        val (timeoutT, timeoutEnv) = elabBlock(timeoutBlock, env1)
        (UnionType(timeoutT :: ts), Approx.joinEnvs(timeoutEnv :: envs))
      case LComprehension(template, qualifiers) =>
        var envAcc = env
        qualifiers.foreach {
          case LGenerate(gPat, gExpr) =>
            val (gT, gEnv) = elabExpr(gExpr, envAcc)
            if (!Subtype.subType(gT, ListType(AnyType)))
              throw TypeMismatch(expr.l, gExpr, expected = ListType(AnyType), got = gT)
            val Some(ListType(gElemT)) = Approx.asListType(gT)
            val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
            envAcc = pEnv
          case BGenerate(gPat, gExpr) =>
            envAcc = check.checkExpr(gExpr, BinaryType, envAcc)
            val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
            envAcc = pEnv
          case Filter(fExpr) =>
            envAcc = check.checkExpr(fExpr, booleanType, envAcc)
        }
        val (tType, _) = elabExpr(template, envAcc)
        (ListType(tType), env)
      case BComprehension(template, qualifiers) =>
        var envAcc = env
        qualifiers.foreach {
          case LGenerate(gPat, gExpr) =>
            val (gT, gEnv) = elabExpr(gExpr, envAcc)
            if (!Subtype.subType(gT, ListType(AnyType)))
              throw TypeMismatch(expr.l, expr, expected = ListType(AnyType), got = gT)
            val Some(ListType(gElemT)) = Approx.asListType(gT)
            val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
            envAcc = pEnv
          case BGenerate(gPat, gExpr) =>
            envAcc = check.checkExpr(gExpr, BinaryType, envAcc)
            val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
            envAcc = pEnv
          case Filter(fExpr) =>
            envAcc = check.checkExpr(fExpr, booleanType, envAcc)
        }
        check.checkExpr(template, BinaryType, envAcc)
        (BinaryType, env)
      case rCreate: RecordCreate =>
        val recType = RecordType(rCreate.recName)
        val env1 = check.checkRecordCreate(rCreate, env)
        (recType, env1)
      case rUpdate: RecordUpdate =>
        val recType = RecordType(rUpdate.recName)
        val env1 = check.checkRecordUpdate(rUpdate, env)
        (recType, env1)
      case RecordSelect(recExpr, recName, fieldName) =>
        val recDecl = Util.getRecord(module, recName).get
        val field = recDecl.fields.find(_.name == fieldName).get
        val fieldT = field.tp
        val env1 = check.checkExpr(recExpr, RecordType(recName), env)
        (fieldT, env1)
      case RecordIndex(_, _) =>
        (integerType, env)
      case MapCreate(kvs) =>
        val isShape = kvs.forall(_._1.isInstanceOf[AtomLit])
        var envAcc = env
        if (isShape) {
          val props = kvs.collect { case (AtomLit(key), value) =>
            val (valT, env1) = elabExpr(value, envAcc)
            envAcc = env1
            ReqProp(key, valT)
          }
          (ShapeMap(props), envAcc)
        } else {
          val (keyTs, valTs) = kvs.map { case (key, value) =>
            val (keyT, env1) = elabExpr(key, envAcc)
            val (valT, env2) = elabExpr(value, env1)
            envAcc = env2
            (keyT, valT)
          }.unzip
          val domain = keyTs.reduce(Subtype.join)
          val codomain = valTs.reduce(Subtype.join)
          (DictMap(domain, codomain), envAcc)
        }
      case GenMapUpdate(map, kvs) =>
        val (mapT, env1) = elabExpr(map, env)
        val anyMap = DictMap(AnyType, AnyType)
        if (!Subtype.subType(mapT, anyMap)) {
          throw TypeMismatch(map.l, map, expected = anyMap, got = mapT)
        }
        var envAcc = env1
        var resT = mapT
        for ((key, value) <- kvs) {
          val (keyT, env2) = elabExpr(key, envAcc)
          val (valT, env3) = elabExpr(value, env2)
          envAcc = env3
          resT = Approx.adjustMapType(resT, keyT, valT)
        }
        (resT, envAcc)
      case ReqMapUpdate(map, kvs) =>
        val (mapT, env1) = elabExpr(map, env)
        val anyMap = DictMap(AnyType, AnyType)
        if (!Subtype.subType(mapT, anyMap))
          // it would be more understandable error first
          throw TypeMismatch(map.l, map, expected = anyMap, got = mapT)

        var envAcc = env1
        var resT = mapT
        for ((key, value) <- kvs) {
          if (!Approx.isShapeWithKey(mapT, key))
            throw UndefinedKey(expr.l, map, key, mapT)
          val (valT, env2) = elabExpr(value, envAcc)
          envAcc = env2
          resT = Approx.adjustMapType(resT, AtomLitType(key), valT)
        }
        (resT, envAcc)
      case fun: Fun => (FoonType(fun.clauses, module, env), env)

    }

  def elabFoonCall(expr: Expr, idOpt: Option[Id], ft: FoonType, args: List[Expr], env: Env): (Type, Env) = {
    val (argTys, argEnvs) = args.map(elabExpr(_, env)).unzip
    val funBody = ft.clauses.head.body.head

    def tooRecursive: TypeError =
      idOpt match {
        case Some(id) => NeedsSpec(funBody.l, id)
        case None => UncheckableFun(expr.l, expr)
      }

    def elabFoonClause(clause: Clause, argTys: List[Type], env0: Env): Type = {
      if (clause.pats.sizeCompare(argTys) != 0) {
        throw ArityMismatch(expr.l, expr, expected = clause.pats.size, got = argTys.size)
      }
      val allScopeVars = Vars.clauseVars(clause)
      val env1 = Util.enterScope(env0, allScopeVars)
      val env2 = elabGuard.elabGuards(clause.guards, env1)
      val (_, env3) = elabPat.elabPats(clause.pats, argTys, env2)
      elabBlock(clause.body, env3)._1
    }
    def doFoonClauses(ft1: FoonType): (Type, Env) = {
      val tys = ft1.clauses.map(elabFoonClause(_, argTys, ft1.env))
      val ty = tys.reduce(Subtype.join)
      (ty, Approx.joinEnvs(argEnvs))
    }

      toBaseReturnTy.get(ft) match {
        case Some(ty) =>
          (ty, Approx.joinEnvs(argEnvs))
        case None =>
            recursiveFuns.baseFoon(ft) match {
            case Some(basedFt) =>
              val (baseTy, _) = doFoonClauses(basedFt)
              toBaseReturnTy += ft -> baseTy
              val res = doFoonClauses(ft)
              toBaseReturnTy -= ft
              res
            case None => throw tooRecursive
          }
      }
  }

  def elabBinaryElem(elem: BinaryElem, env: Env): (Type, Env) = {
    val env1 = elem.size match {
      case Some(s) => check.checkExpr(s, integerType, env)
      case None    => env
    }
    val isStringLiteral = false
    val expType = BinarySpecifiers.expType(elem.specifier, isStringLiteral)
    val env2 = check.checkExpr(elem.expr, expType, env1)
    (expType, env2)
  }
}
