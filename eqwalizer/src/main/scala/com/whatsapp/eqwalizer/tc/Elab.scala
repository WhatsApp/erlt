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
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

import scala.annotation.tailrec

class Elab(module: String) {
  @tailrec
  private def elabBlock(exprs: List[Expr], env: Env): (Type, Env) =
    exprs match {
      case Nil =>
        throw new IllegalStateException()
      case expr :: Nil =>
        elabExpr(expr, env)
      case expr :: rest =>
        val (_, env1) = elabExpr(expr, env)
        elabBlock(rest, env1)
    }

  private def elabClause(clause: Clause, env: Env): (Type, Env) = {
    val argTypes = List.fill(clause.pats.size)(AnyType)
    val (_, env1) = ElabPat.elabPats(clause.pats, argTypes, env)
    elabBlock(clause.body, env1)
  }

  def elabExpr(expr: Expr, env: Env): (Type, Env) =
    expr match {
      case Var(v) =>
        env.get(v) match {
          case Some(vt) =>
            (vt, env)
          case None =>
            throw UnboundVar(expr.l, v)
        }
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
      case LocalCall(id, args) =>
        Util.getFunType(module, id) match {
          case Some(FunType(argTys, resTy)) =>
            val env1 = Check(module).checkExprs(args, argTys, env)
            (resTy, env1)
          case None =>
            throw UnboundVar(expr.l, id.toString)
        }
      case RemoteCall(fqn, args) =>
        Util.getFunType(fqn) match {
          case Some(FunType(argTys, resTy)) =>
            val env1 = Check(module).checkExprs(args, argTys, env)
            (resTy, env1)
          case None =>
            throw UnboundVar(expr.l, fqn.toString)
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
        val (ts, envs) = clauses.map(elabClause(_, env1)).unzip
        (UnionType(ts), Approx.joinEnvs(env1, envs))
      case Match(mPat, mExp) =>
        val (ty, env1) = elabExpr(mExp, env)
        ElabPat.elabPat(mPat, ty, env1)
    }
}
