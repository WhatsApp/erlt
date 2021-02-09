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

package com.whatsapp.coralizer.tc

import com.whatsapp.coralizer.ast.Forms.FunSpec
import com.whatsapp.coralizer.ast.Types._
import com.whatsapp.coralizer.ast.{Id, Vars}
import com.whatsapp.coralizer.tc.TcDiagnostics._
import erlang.CErl._

final case class Check(module: String) {
  val elab = new Elab(module, this)

  def checkSpeccedFun(f: CFun, spec: FunSpec, env: Env): Unit = {
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    val env1 = env ++ (f.vars.map(_.name) zip argTys).toMap
    try {
      checkExpr(f.body, resTy, env1)
    } catch {
      case te: TypeError if te.line == 0 =>
        val line = f.line
        throw te match {
          case te: TypeMismatch    => te.copy(line = line)
          case te: UnboundVar      => te.copy(line = line)
          case te: UnboundId       => te.copy(line = line)
          case te: UnboundRemoteId => te.copy(line = line)
        }
    }
  }

  private def checkClause(
      clause: CClause,
      argTys: List[Type],
      resTy: Type,
      env0: Env
  ): Env = {
    val env1 = Util.initClauseEnv(env0, Vars.clauseVars(clause))
    val env2 = ElabGuard(elab).elabGuard(clause.guard, env1)
    val (_, env3) = ElabPat.elabPats(clause.pats, argTys, env2)
    checkExpr(clause.body, resTy, env3)
  }

  def checkFunBody(body: CErl, resTy: Type, env: Env): Env = {
    checkExpr(body, resTy, env)
  }

  def checkExprs(exprs: List[CErl], tys: List[Type], env: Env): Env = {
    var envAcc = env
    for ((e, t) <- exprs.zip(tys)) {
      envAcc = checkExpr(e, t, envAcc)
    }
    envAcc
  }

  def checkExpr(expr: CErl, resTy: Type, env: Env): Env = {
    expr match {
      case CCase(_, sel, clauses) =>
        val (argTys, env2) = elab.elabExpr(sel, env) match {
          case (CValuesType(tys), e) => (tys, e)
          case (ty, e)               => (List(ty), e)
        }
        if (clauses.isEmpty) env
        else {
          val envs = clauses.map(checkClause(_, argTys, resTy, env2))
          Approx.combineEnvs(env2, Subtype.join, envs)
        }
      case CLetRec(_, defs, body) =>
        for ((cvar, fun) <- defs) {
          cvar match {
            case CVar(_, VarNameAtomInt(id)) =>
              val FunType(_, retTy) = BuiltIn.letRecSpecialFunToType(id)
              checkFunBody(fun.body, retTy, env)
            case _ => sys.error(s"unexpected fun in letrec $cvar")
          }
        }
        checkExpr(body, resTy, env)
      case CSeq(_, arg, body) =>
        val env1 = checkExpr(arg, AnyType, env)
        checkExpr(body, resTy, env1)
      case CTry(_, arg, bodyVars, body, evars, handler) =>
        val (argTy, env1) = elab.elabExpr(arg, env)
        val (_, env2) = ElabPat.elabPats(bodyVars, List(argTy), env1)
        val bodyEnv = checkExpr(body, resTy, env2)
        val handlerEnv = checkTryHandler(evars, handler, resTy, env1)
        // TODO: consider including env for `after` call as well. It's in a fun defined in a letrec
        Approx.combineEnvs(env, Subtype.join, bodyEnv :: handlerEnv :: Nil)
      case _ =>
        val (actualTy, env1) = elab.elabExpr(expr, env)
        if (!Subtype.subType(actualTy, resTy)) {
          throw TypeMismatch(expr.line, expr, resTy, actualTy)
        }
        env1
    }
  }

  private def checkTryHandler(
      evars: List[CVar],
      body: CErl,
      resTy: Type,
      env: Env
  ): Env = {
    val handlerTypes = evars.map(_.name) zip (evars.size match {
      case 3 =>
        /* Class:Msg:stack */
        BuiltIn.catchMatchWithStackTypes
      case _ =>
        ElabPat.elabPats(evars, List(BuiltIn.catchMatchNoStackType), env)._1
    })
    checkExpr(body, resTy, env ++ handlerTypes)
  }
}
