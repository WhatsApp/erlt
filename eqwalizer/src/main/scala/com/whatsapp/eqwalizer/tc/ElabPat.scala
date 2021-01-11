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
      case PatMatch(p1, p2) =>
        val (t1, env1) = elabPat(p1, t, env)
        elabPat(p2, t1, env1)
    }
}
