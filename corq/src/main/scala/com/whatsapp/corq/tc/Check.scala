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

package com.whatsapp.corq.tc

import com.whatsapp.corq.ast.Forms.FunSpec
import com.whatsapp.corq.ast.Types._
import com.whatsapp.corq.ast.{Id, Vars}
import com.whatsapp.corq.tc.TcDiagnostics._
import erlang.CErl
import erlang.CErl._
import erlang.Data._

final case class Check(module: String) {
  val elab = new Elab(module, this)

  def checkSpeccedFun(id: Id, f: CFun, spec: FunSpec, env: Env): Unit = {
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    val env1 = env ++ (f.vars.map(_.name) zip argTys).toMap
    try {
      checkExpr(f.body, resTy, env1)
    } catch {
      case te: TypeError =>
        throw te match {
          case te: TypeMismatch    => te.copy(expr = f.body)
          case te: UnboundVar      => te.copy(line = f.anno.line)
          case te: UnboundId       => te.copy(line = f.anno.line)
          case te: UnboundRemoteId => te.copy(line = f.anno.line)
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
  //  @tailrec
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

      case CCons(_, hd, tl) =>
        val (headType, env1) = elab.elabExpr(hd, env)
        val listType1 = ListType(headType)
        if (!Subtype.subType(listType1, resTy))
          throw TypeMismatch(expr, expected = resTy, got = listType1)
        checkExpr(tl, resTy, env1)

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
      case CFun(_, vars, body) => {
        resTy match {
          case FunType(expArgTys, expFunResTy) => {
            if (expArgTys.lengthCompare(vars) != 0)
              throw TypeMismatch(
                expr,
                resTy,
                FunType(vars.map(_ => AnyType), AnyType)
              )
            checkFunBody(body, expFunResTy, env ++ (vars.map(_.name).zip(expArgTys).toMap))
          }
          case other => throw TypeMismatch(expr, resTy, other)
        }
      }
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
          throw TypeMismatch(expr, resTy, actualTy)
        }
        env1
      // $COVERAGE-OFF$
      case other =>
        throw new IllegalStateException(s"internal error: unexpected $other")
      // $COVERAGE-ON$
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

//  @tailrec
//      case CCase(_, sel, clauses) =>
//        val (selType, env1) = elab.elabExpr(sel, env)
//        clauses.map(checkClause(_, List(selType), resTy, env1))
//      case ctry @ CTry(_, arg, bodyVars, body, handlerVars, handler) =>
//        val (bodyTy, handlerTy) = elab.elabTryBranches(ctry, env)
//        if (!Subtype.subType(handlerTy, resTy))
//          throw TypeMismatch(handler, resTy, handlerTy)
//        if (!Subtype.subType(bodyTy, resTy))
//          throw TypeMismatch(body, resTy, bodyTy)
  // These are OK to do in elab mode

}
