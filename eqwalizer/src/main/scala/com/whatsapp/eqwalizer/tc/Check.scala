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
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

import scala.annotation.tailrec

case class Check(module: String) {
  val elab = new Elab(module)

  def checkFun(f: FunDecl, spec: FunSpec): Unit = {
    if (spec.types.size > 1) throw IntersectionType(f.line)
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    checkClauses(Map.empty, argTys, resTy, f.clauses)
  }

  @tailrec
  private def checkBlock(block: List[Expr], resTy: Type, env: Env): Env =
    if (block.size == 1)
      checkExpr(block.head, resTy, env)
    else {
      val (_, env1) = elab.elabExpr(block.head, env)
      checkBlock(block.tail, resTy, env1)
    }

  private def checkClause(clause: Clause, argTys: List[Type], resTy: Type, env: Env): Env = {
    val (_, env1) = ElabPat.elabPats(clause.pats, argTys, env)
    checkBlock(clause.body, resTy, env1)
  }

  private def checkClauses(env: Env, argTys: List[Type], resTy: Type, clauses: List[Clause]): Env = {
    val envs = clauses.map(checkClause(_, argTys, resTy, env))
    Approx.joinEnvs(env, envs)
  }

  def checkExprs(exprs: List[Expr], tys: List[Type], env: Env): Env = {
    var envAcc = env
    for ((e, t) <- exprs.zip(tys)) {
      envAcc = checkExpr(e, t, envAcc)
    }
    envAcc
  }

  private def checkExpr(expr: Expr, resTy: Type, env: Env): Env = {
    if (resTy == AnyType) elab.elabExpr(expr, env)._2
    else
      expr match {
        case Var(v) =>
          env.get(v) match {
            case Some(vt) =>
              if (Subtype.subType(vt, resTy)) env
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = vt)
            case None =>
              throw UnboundVar(expr.l, v)
          }
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
        case LocalCall(id, args) =>
          Util.getFunType(module, id) match {
            case Some(FunType(argTys, fResTy)) =>
              val env1 = Check(module).checkExprs(args, argTys, env)
              if (Subtype.subType(fResTy, resTy)) env1
              else throw TypeMismatch(expr.l, expr, expected = resTy, got = fResTy)
            case None =>
              throw UnboundVar(expr.l, id.toString)
          }
        case RemoteCall(fqn, args) =>
          Util.getFunType(fqn) match {
            case Some(FunType(argTys, fResTy)) =>
              val env1 = Check(module).checkExprs(args, argTys, env)
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
          checkClauses(env1, List(selType), resTy, clauses)
        case Match(mPat, mExp) =>
          val (mType, env1) = elab.elabExpr(mExp, env)
          val (t2, env2) = ElabPat.elabPat(mPat, mType, env1)
          if (Subtype.subType(t2, resTy)) env2
          else throw TypeMismatch(expr.l, expr, expected = resTy, got = t2)
      }

  }
}
