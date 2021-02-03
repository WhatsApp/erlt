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

import scala.annotation.tailrec

final case class Check(module: String) {
  val elab = new Elab(module, this)

  def checkSpeccedFun(id: Id, f: CFun, spec: FunSpec, env: Env): Unit = {
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    val env1 = env ++ (f.vars zip argTys).toMap
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

  def checkFunBody(body: CErl, resTy: Type, env: Env): Unit = {
    checkExpr(body, resTy, env)
  }

  def checkExprs(exprs: List[CErl], tys: List[Type], env: Env): Unit = {
    for ((e, t) <- exprs.zip(tys)) {
      checkExpr(e, t, env)
    }
  }

//  @tailrec
  def checkExpr(expr: CErl, resTy: Type, env: Env): Unit = {
    expr match {
      case ccase @ CCase(_, _, clauses) =>
        val tys = elab.elabCaseBranches(ccase, env)
        clauses zip tys foreach {
          case (clause, ty) =>
            if (!Subtype.subType(ty, resTy))
              throw TypeMismatch(clause, resTy, ty)
        }
      case ctry @ CTry(_, _, _, body, _, handler) =>
        val (bodyTy, handlerTy) = elab.elabTryBranches(ctry, env)
        if (!Subtype.subType(handlerTy, resTy))
          throw TypeMismatch(handler, resTy, handlerTy)
        if (!Subtype.subType(bodyTy, resTy))
          throw TypeMismatch(body, resTy, bodyTy)
      case CFun(_, vars, body) => {
        resTy match {
          case FunType(expArgTys, expFunResTy) => {
            if (expArgTys.lengthCompare(vars) != 0)
              throw TypeMismatch(
                expr,
                resTy,
                FunType(vars.map(_ => AnyType), AnyType)
              )
            checkFunBody(body, expFunResTy, env ++ vars.zip(expArgTys).toMap)
          }
          case other => throw TypeMismatch(expr, resTy, other)
        }
      }
      case _: CApply | _: CBinary | _: CBitstr | _: CCall | _: CLiteral |
          _: CLet | _: CLetRec | _: CPrimOp | _: CTry | _: CTuple | _: CVar |
          _: CCons | _: CSeq | _: CCase | _: CCatch =>
        val actualTy = elab.elabExpr(expr, env)
        if (!Subtype.subType(actualTy, resTy)) {
          throw TypeMismatch(expr, resTy, actualTy)
        }
      // $COVERAGE-OFF$
      case other =>
        throw new IllegalStateException(s"internal error: unexpected $other")
      // $COVERAGE-ON$
    }
  }

}
