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
import com.whatsapp.eqwalizer.ast.{BinarySpecifiers, Vars}
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

import scala.annotation.tailrec

final class Elab(module: String) {
  @tailrec
  def elabBlock(exprs: List[Expr], env: Env): (Type, Env) =
    exprs match {
      case expr :: Nil =>
        elabExpr(expr, env)
      case expr :: rest =>
        val (_, env1) = elabExpr(expr, env)
        elabBlock(rest, env1)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }

  private def elabClause(clause: Clause, env: Env): (Type, Env) = {
    val env1 = Util.initClauseEnv(env, Vars.clauseVars(clause))
    val argTypes = List.fill(clause.pats.size)(AnyType)
    val env2 = ElabGuard.elabGuards(clause.guards, env1)
    val (_, env3) = ElabPat.elabPats(clause.pats, argTypes, env2)
    elabBlock(clause.body, env3)
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
      case NilLit() =>
        (NilType, env)
      case Cons(head, tail) =>
        val (headT, env1) = elabExpr(head, env)
        val (tailT, env2) = elabExpr(tail, env1)
        (UnionType(List(ListType(headT), tailT)), env2)
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
      case If(clauses) =>
        val (ts, envs) = clauses.map(elabClause(_, env)).unzip
        (UnionType(ts), Approx.joinEnvs(envs.head, envs.tail))
      case Match(mPat, mExp) =>
        val (ty, env1) = elabExpr(mExp, env)
        ElabPat.elabPat(mPat, ty, env1)
      case UnOp(op, arg) =>
        op match {
          case "not" =>
            val env1 = Check(module).checkExpr(arg, booleanType, env)
            (booleanType, env1)
          case "bnot" | "+" | "-" =>
            val env1 = Check(module).checkExpr(arg, NumberType, env)
            (NumberType, env1)
          // $COVERAGE-OFF$
          case _ => throw new IllegalStateException()
          // $COVERAGE-ON$
        }
      case BinOp(op, arg1, arg2) =>
        op match {
          case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
            val env1 = Check(module).checkExpr(arg1, NumberType, env)
            val env2 = Check(module).checkExpr(arg2, NumberType, env1)
            (NumberType, env2)
          case "or" | "and" | "xor" =>
            val env1 = Check(module).checkExpr(arg1, booleanType, env)
            val env2 = Check(module).checkExpr(arg2, booleanType, env1)
            (booleanType, env2)
          case "andalso" | "orelse" =>
            val env1 = Check(module).checkExpr(arg1, booleanType, env)
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
        val (catchTs, _) = catchClauses.map(elabClause(_, env)).unzip
        val env1 = afterBody match {
          case Some(block) => elabBlock(block, env)._2
          case None        => env
        }
        (UnionType(tryT :: catchTs), env1)
      case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
        val (_, tryEnv) = elabBlock(tryBody, env)
        val (tryTs, _) = tryClauses.map(elabClause(_, tryEnv)).unzip
        val (catchTs, _) = catchClauses.map(elabClause(_, env)).unzip
        val env1 = afterBody match {
          case Some(block) => elabBlock(block, env)._2
          case None        => env
        }
        (UnionType(tryTs ::: catchTs), env1)
      case Receive(clauses) =>
        val (ts, envs) = clauses.map(elabClause(_, env)).unzip
        (UnionType(ts), Approx.joinEnvsAll(envs))
      case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
        val (ts, envs) = clauses.map(elabClause(_, env)).unzip
        val env1 = Check(module).checkExpr(timeout, integerType, env)
        val (timeoutT, timeoutEnv) = elabBlock(timeoutBlock, env1)
        (UnionType(timeoutT :: ts), Approx.joinEnvsAll(timeoutEnv :: envs))
    }

  def elabBinaryElem(elem: BinaryElem, env: Env): (Type, Env) = {
    val env1 = elem.size match {
      case Some(s) => Check(module).checkExpr(s, integerType, env)
      case None    => env
    }
    val isStringLiteral = false
    val expType = BinarySpecifiers.expType(elem.specifier, isStringLiteral)
    val env2 = Check(module).checkExpr(elem.expr, expType, env1)
    (expType, env2)
  }
}
