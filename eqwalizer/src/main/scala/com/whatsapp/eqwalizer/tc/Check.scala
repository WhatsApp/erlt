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
import com.whatsapp.eqwalizer.ast.Forms.{FunDecl, FunSpec}
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.Vars
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

final case class Check(module: String) {
  val elab = new Elab(module)

  def checkFun(f: FunDecl, spec: FunSpec): Unit = {
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    f.clauses.map(checkClause(_, argTys, resTy, Map.empty, Set.empty))
  }

  private def checkBlock(block: List[Expr], resTy: Type, env: Env): Env = {
    var envAcc = env
    for (expr <- block.init) {
      val (_, env1) = elab.elabExpr(expr, envAcc)
      envAcc = env1
    }
    checkExpr(block.last, resTy, envAcc)
  }

  private def checkClause(
      clause: Clause,
      argTys: List[Type],
      resTy: Type,
      env0: Env,
      exportedVars: Set[String],
  ): Env = {
    val allScopeVars = Vars.clauseVars(clause)
    val env1 = Util.enterScope(env0, allScopeVars)
    val env2 = ElabGuard.elabGuards(clause.guards, env1)
    val (_, env3) = ElabPat.elabPats(clause.pats, argTys, env2)
    val env4 = checkBlock(clause.body, resTy, env3)
    Util.exitScope(env0, env4, exportedVars)
  }

  def checkExprs(exprs: List[Expr], tys: List[Type], env: Env): Env = {
    var envAcc = env
    for ((e, t) <- exprs.zip(tys)) {
      envAcc = checkExpr(e, t, envAcc)
    }
    envAcc
  }

  def checkExpr(expr: Expr, resTy: Type, env: Env): Env =
    if (Subtype.subType(AnyType, resTy)) elab.elabExpr(expr, env)._2
    else
      expr match {
        case Var(v) =>
          val vt = env(v)
          if (Subtype.subType(vt, resTy)) env
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = vt)
        case AtomLit(a) =>
          val litType = AtomLitType(a)
          if (Subtype.subType(litType, resTy)) env
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = litType)
        case NumberLit() =>
          val litType = NumberType
          if (Subtype.subType(litType, resTy)) env
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = litType)
        case Tuple(elems) =>
          var envAcc = env
          val elemTypes = elems.map { elem =>
            val (elemType, env1) = elab.elabExpr(elem, envAcc)
            envAcc = env1
            elemType
          }
          val tupleType = TupleType(elemTypes)
          if (Subtype.subType(tupleType, resTy)) envAcc
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = tupleType)
        case NilLit() =>
          val litType = NilType
          if (Subtype.subType(litType, resTy)) env
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = litType)
        case Cons(head, tail) =>
          val (headType, env1) = elab.elabExpr(head, env)
          val typeList1 = ListType(headType)
          if (!Subtype.subType(typeList1, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = typeList1)
          checkExpr(tail, resTy, env1)
        case LocalCall(id, args) =>
          Util.getFunType(module, id) match {
            case Some(FunType(argTys, fResTy)) =>
              val env1 = checkExprs(args, argTys, env)
              if (Subtype.subType(fResTy, resTy)) env1
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = fResTy)
            case None =>
              throw UnboundVar(expr.l, id.toString)
          }
        case RemoteCall(fqn, args) =>
          Util.getFunType(fqn) match {
            case Some(FunType(argTys, fResTy)) =>
              val env1 = checkExprs(args, argTys, env)
              if (Subtype.subType(fResTy, resTy)) env1
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = fResTy)
            case None =>
              throw UnboundVar(expr.l, fqn.toString)
          }
        case LocalFun(id) =>
          Util.getFunType(module, id) match {
            case Some(ft) =>
              if (Subtype.subType(ft, resTy)) env
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = ft)
            case None =>
              throw UnboundVar(expr.l, id.toString)
          }
        case RemoteFun(fqn) =>
          Util.getFunType(fqn) match {
            case Some(ft) =>
              if (Subtype.subType(ft, resTy)) env
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = ft)
            case None =>
              throw UnboundVar(expr.l, fqn.toString)
          }
        case Block(block) =>
          checkBlock(block, resTy, env)
        case Case(sel, clauses) =>
          val (selType, env1) = elab.elabExpr(sel, env)
          val effVars = Vars.clausesVars(clauses)
          val envs2 = clauses.map(checkClause(_, List(selType), resTy, env1, effVars))
          Approx.joinEnvs(envs2)
        case If(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          val envs1 = clauses.map(checkClause(_, List.empty, resTy, env, effVars))
          Approx.joinEnvs(envs1)
        case Match(mPat, mExp) =>
          val (mType, env1) = elab.elabExpr(mExp, env)
          val (t2, env2) = ElabPat.elabPat(mPat, mType, env1)
          if (Subtype.subType(t2, resTy)) env2
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = t2)
        case UnOp(op, arg) =>
          op match {
            case "not" =>
              if (!Subtype.subType(booleanType, resTy)) {
                throw TypeMismatch(expr.l, expr, expected = resTy, got = booleanType)
              }
              checkExpr(arg, booleanType, env)
            case "bnot" | "+" | "-" =>
              if (!Subtype.subType(NumberType, resTy)) {
                throw TypeMismatch(expr.l, expr, expected = resTy, got = NumberType)
              }
              checkExpr(arg, NumberType, env)
            // $COVERAGE-OFF$
            case _ => throw new IllegalStateException()
            // $COVERAGE-ON$
          }
        case BinOp(op, arg1, arg2) =>
          op match {
            case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
              if (!Subtype.subType(NumberType, resTy)) {
                throw TypeMismatch(expr.l, expr, expected = resTy, got = NumberType)
              }
              val env1 = checkExpr(arg1, NumberType, env)
              val env2 = checkExpr(arg2, NumberType, env1)
              env2
            case "or" | "and" | "xor" =>
              if (!Subtype.subType(booleanType, resTy)) {
                throw TypeMismatch(expr.l, expr, expected = resTy, got = booleanType)
              }
              val env1 = checkExpr(arg1, booleanType, env)
              val env2 = checkExpr(arg2, booleanType, env1)
              env2
            case "andalso" | "orelse" =>
              val env1 = checkExpr(arg1, booleanType, env)
              val (t2, env2) = elab.elabExpr(arg2, env1)
              val elabType = Subtype.join(booleanType, t2)
              if (!Subtype.subType(elabType, resTy)) {
                throw TypeMismatch(expr.l, expr, expected = resTy, got = elabType)
              }
              env2
            // $COVERAGE-OFF$
            case _ => throw new IllegalStateException()
            // $COVERAGE-ON$
          }
        case Binary(elems) =>
          if (!Subtype.subType(BinaryType, resTy)) {
            throw TypeMismatch(expr.l, expr, expected = resTy, got = BinaryType)
          }
          var envAcc = env
          for { elem <- elems } {
            val (_, env1) = elab.elabBinaryElem(elem, envAcc)
            envAcc = env1
          }
          envAcc
        case Catch(_) =>
          throw TypeMismatch(expr.l, expr, expected = resTy, got = AnyType)
        case TryCatchExpr(tryBody, catchClauses, afterBody) =>
          checkBlock(tryBody, resTy, env)
          catchClauses.map(checkClause(_, List(AnyType), resTy, env, Set.empty))
          afterBody match {
            case Some(block) => elab.elabBlock(block, env)._2
            case None        => env
          }
        case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
          val (tryBodyT, tryEnv) = elab.elabBlock(tryBody, env)
          tryClauses.map(checkClause(_, List(tryBodyT), resTy, tryEnv, Set.empty))
          catchClauses.map(checkClause(_, List(AnyType), resTy, env, Set.empty))
          afterBody match {
            case Some(block) => elab.elabBlock(block, env)._2
            case None        => env
          }
        case Receive(clauses) =>
          val effVars = Vars.clausesVars(clauses)
          val envs1 = clauses.map(checkClause(_, List(AnyType), resTy, env, effVars))
          Approx.joinEnvs(envs1)
        case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
          val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
          val envs1 = clauses.map(checkClause(_, List(AnyType), resTy, env, effVars))
          val tEnv1 = checkExpr(timeout, integerType, env)
          val tEnv2 = checkBlock(timeoutBlock, resTy, tEnv1)
          val tEnv3 = Util.exitScope(env, tEnv2, effVars)
          Approx.joinEnvs(tEnv3 :: envs1)
        case LComprehension(template, qualifiers) =>
          var envAcc = env
          qualifiers.foreach {
            case LGenerate(gPat, gExpr) =>
              val (gT, gEnv) = elab.elabExpr(gExpr, envAcc)
              if (!Subtype.subType(gT, ListType(AnyType)))
                throw TypeMismatch(expr.l, gExpr, expected = ListType(AnyType), got = gT)
              val Some(ListType(gElemT)) = Approx.asListType(gT)
              val (_, pEnv) = ElabPat.elabPat(gPat, gElemT, gEnv)
              envAcc = pEnv
            case BGenerate(gPat, gExpr) =>
              envAcc = checkExpr(gExpr, BinaryType, envAcc)
              val (_, pEnv) = ElabPat.elabPat(gPat, BinaryType, envAcc)
              envAcc = pEnv
            case Filter(fExpr) =>
              envAcc = checkExpr(fExpr, booleanType, envAcc)
          }
          val (tType, _) = elab.elabExpr(template, envAcc)
          val elabType = ListType(tType)
          if (!Subtype.subType(elabType, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = elabType)
          env
        case BComprehension(template, qualifiers) =>
          if (!Subtype.subType(BinaryType, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = BinaryType)
          var envAcc = env
          qualifiers.foreach {
            case LGenerate(gPat, gExpr) =>
              val (gT, gEnv) = elab.elabExpr(gExpr, envAcc)
              if (!Subtype.subType(gT, ListType(AnyType)))
                throw TypeMismatch(expr.l, gExpr, expected = ListType(AnyType), got = gT)
              val Some(ListType(gElemT)) = Approx.asListType(gT)
              val (_, pEnv) = ElabPat.elabPat(gPat, gElemT, gEnv)
              envAcc = pEnv
            case BGenerate(gPat, gExpr) =>
              envAcc = checkExpr(gExpr, BinaryType, envAcc)
              val (_, pEnv) = ElabPat.elabPat(gPat, BinaryType, envAcc)
              envAcc = pEnv
            case Filter(fExpr) =>
              envAcc = checkExpr(fExpr, booleanType, envAcc)
          }
          checkExpr(template, BinaryType, envAcc)
          env
        case rCreate: RecordCreate =>
          val recType = RecordType(rCreate.recName)
          if (!Subtype.subType(recType, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = recType)
          else
            checkRecordCreate(rCreate, env)
        case rUpdate: RecordUpdate =>
          val recType = RecordType(rUpdate.recName)
          if (!Subtype.subType(recType, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = recType)
          else
            checkRecordUpdate(rUpdate, env)
        case RecordSelect(recExpr, recName, fieldName) =>
          val recDecl = Util.getRecord(module, recName).get
          val field = recDecl.fields.find(_.name == fieldName).get
          val fieldT = field.tp
          if (!Subtype.subType(fieldT, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = fieldT)
          else
            checkExpr(recExpr, RecordType(recName), env)
        case RecordIndex(_, _) =>
          val indT = integerType
          if (!Subtype.subType(indT, resTy))
            throw TypeMismatch(expr.l, expr, expected = resTy, got = indT)
          else
            env
      }

  def checkRecordCreate(rCreate: RecordCreate, env: Env): Env = {
    val RecordCreate(recName, fields) = rCreate
    val recDecl = Util.getRecord(module, recName).get
    val fieldDecls = recDecl.fields.map(f => f.name -> f).toMap
    val undefinedFields = fieldDecls.keySet -- fields.map(_.name)
    for (uField <- undefinedFields) {
      val fieldDecl = fieldDecls(uField)
      if (fieldDecl.defaultValue.isEmpty && !Subtype.subType(undefined, fieldDecl.tp)) {
        throw UndefinedField(rCreate.l, recName, uField)
      }
    }
    var envAcc = env
    for (field <- fields) {
      val fieldDecl = fieldDecls(field.name)
      envAcc = checkExpr(field.value, fieldDecl.tp, envAcc)
    }
    envAcc
  }

  def checkRecordUpdate(rUpdate: RecordUpdate, env: Env): Env = {
    val RecordUpdate(recExpr, recName, fields) = rUpdate
    var envAcc = checkExpr(recExpr, RecordType(recName), env)
    val recDecl = Util.getRecord(module, recName).get
    val fieldDecls = recDecl.fields.map(f => f.name -> f).toMap
    for (field <- fields) {
      val fieldDecl = fieldDecls(field.name)
      envAcc = checkExpr(field.value, fieldDecl.tp, envAcc)
    }
    envAcc
  }
}
