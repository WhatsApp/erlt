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

import com.whatsapp.corq.ast.Forms.{FunDecl, FunSpec}
import com.whatsapp.corq.ast.Types._
import com.whatsapp.corq.ast.{Id, Vars}
import com.whatsapp.corq.tc.TcDiagnostics._
import erlang.CErl._

import scala.annotation.tailrec

final case class Check(module: String) {
  val elab = new Elab(module)

  def checkFun(id: Id, f: CFun, spec: FunSpec, env: Env): Unit = {
    val constrainedFunType = spec.types.head
    val FunType(argTys, resTy) = constrainedFunType.ty
    val env = f.vars.zip(argTys).toMap
    checkExpr(f.body, resTy, env)
  }

//  @tailrec
  def checkExpr(expr: CErl, resTy: Type, env: Env): Env = {
//    if (Subtype.subType(AnyType, resTy)) elab.elabExpr(expr, env)._2
//    else
    expr match {
      case CApply(_, op, args) =>
        env
      case CBinary(_, segments) =>
        env
      case CBitstr(_, value, size, unit, typ, flags) =>
        env
      case CCall(_, module, name, args) =>
        env
      case CCase(_, arg, clauses) =>
        env
      case CCons(_, hd, tl) =>
        env
      case CLet(_, vars, arg, body) =>
        env
      case CLetRec(_, defs, body) =>
        env
      case CLiteral(_, value) =>
        env
      case CMap(_, arg, es, isPat) =>
        env
      case CMapPair(_, op, key, cVal) =>
        env
      case CPrimOp(_, name, args) =>
        env
      case CSeq(_, arg, body) =>
        env
      case CTry(_, arg, vars, body, evars, handler) =>
        env
      case CTuple(_, es) =>
        env
      case CValues(_, es) =>
        env
    }
  }

}
