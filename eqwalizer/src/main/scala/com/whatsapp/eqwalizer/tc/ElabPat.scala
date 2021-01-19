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

import com.whatsapp.eqwalizer.ast.Pats._
import com.whatsapp.eqwalizer.ast.Types._

object ElabPat {
  def elabPats(pats: List[Pat], tys: List[Type], env: Env): (List[Type], Env) = {
    var envAcc = env
    val patTypes = (pats zip tys).map { case (pat, ty) =>
      val (patType, env1) = elabPat(pat, ty, envAcc)
      envAcc = env1
      patType
    }
    (patTypes, envAcc)
  }

  def elabPat(pat: Pat, t: Type, env: Env): (Type, Env) =
    pat match {
      case PatWild() =>
        (t, env)
      case PatVar(v) =>
        val patType = env.get(v) match {
          case Some(vt) => Subtype.meet(vt, t)
          case None     => t
        }
        (patType, env + (v -> patType))
      case PatAtom(s) =>
        val patType = Subtype.meet(AtomLitType(s), t)
        (patType, env)
      case PatNumber() =>
        val patType = Subtype.meet(NumberType, t)
        (patType, env)
      case PatTuple(elems) =>
        Approx.asTupleType(t, elems.size) match {
          case None =>
            var envAcc = env
            elems.foreach { elem =>
              val (_, env1) = elabPat(elem, NoneType, envAcc)
              envAcc = env1
            }
            (NoneType, envAcc)
          case Some(TupleType(elemTypes)) =>
            var envAcc = env
            val patTypes = elems.zip(elemTypes).map { case (elem, elemT) =>
              val (patType, env1) = elabPat(elem, elemT, envAcc)
              envAcc = env1
              patType
            }
            (TupleType(patTypes), envAcc)
        }
      case PatNil() =>
        val patType = Subtype.meet(NilType, t)
        (patType, env)
      case PatCons(hPat, tPat) =>
        Approx.asListType(t) match {
          case None =>
            val (_, env1) = elabPat(hPat, NoneType, env)
            val (_, env2) = elabPat(tPat, NoneType, env1)
            (NoneType, env2)
          case Some(ListType(elemType)) =>
            val (hType, env1) = elabPat(hPat, elemType, env)
            val (tType, env2) = elabPat(tPat, ListType(elemType), env1)
            Approx.asListType(tType) match {
              case None =>
                (NoneType, env2)
              case Some(refinedT) =>
                (ListType(UnionType(List(hType, refinedT))), env2)
            }
        }
      case PatMatch(p1, p2) =>
        val (t1, env1) = elabPat(p1, t, env)
        elabPat(p2, t1, env1)
      case unOp: PatUnOp =>
        elabUnOp(unOp, t, env)
      case binOp: PatBinOp =>
        elabBinOp(binOp, t, env)
    }

  def elabUnOp(pat: PatUnOp, t: Type, env: Env): (Type, Env) = {
    val PatUnOp(op, arg) = pat
    op match {
      case "+" | "-" | "bnot" =>
        val (_, env1) = elabPat(arg, NumberType, env)
        (NumberType, env1)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }

  def elabBinOp(binOp: PatBinOp, t: Type, env: Env): (Type, Env) = {
    val PatBinOp(op, arg1, arg2) = binOp
    op match {
      case "/" | "*" | "-" | "+" | "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
        val (_, env1) = elabPat(arg1, NumberType, env)
        val (_, env2) = elabPat(arg2, NumberType, env1)
        (NumberType, env2)
      // $COVERAGE-OFF$
      case _ => throw new IllegalStateException()
      // $COVERAGE-ON$
    }
  }
}
