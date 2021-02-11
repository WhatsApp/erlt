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

import com.whatsapp.coralizer.ast.BinarySpecifiers
import com.whatsapp.coralizer.ast.Pats._
import com.whatsapp.coralizer.ast.Types._
import erlang.CErl._
import erlang.Data._

object ElabPat {
  def elabPats(
      pats: List[CErl],
      tys: List[Type],
      env: Env
  ): (List[Type], Env) = {
    var envAcc = env
    val patTypes = (pats zip tys).map {
      case (pat, ty) =>
        val (patType, env1) = elabPat(pat, ty, envAcc)
        envAcc = env1
        patType
    }
    (patTypes, envAcc)
  }
  private def elabPatData(data: EObject, t: Type, env: Env): Type =
    data match {
      case EAtom(atom) =>
        AtomLitType(atom)
      case EDouble(_) =>
        NumberType
      case EString(_) =>
        // note: we hit this code path even for things that
        // are not strings in the surface syntax such as
        // `case X of [1, 2]`.
        ListType(NumberType)
      case EList(elems) =>
        Approx.asListType(t) match {
          case None =>
            NoneType
          case Some(ListType(elemType)) =>
            val elemTys = elems map (elabPatData(_, elemType, env))
            ListType(UnionType(elemTys))
        }
      case ELong(_) =>
        NumberType
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $data")
      // $COVERAGE-ON$
    }
  def elabPat(pat: CErl, t: Type, env: Env): (Type, Env) =
    pat match {
      case CAlias(_, v, pat) =>
        val (patTy, env1) = elabPat(pat, t, env)
        (t, env1 + (v.name -> patTy))

      case CBinary(_, elems) =>
        val patType = Subtype.meet(BinaryType, t)
        var envAcc = env
        for { elem <- elems } {
          envAcc = elabBinaryElem(elem, envAcc)
        }
        (patType, envAcc)
      case CCons(_, hd, tl) =>
        Approx.asListType(t) match {
          case None =>
            val (_, env1) = elabPat(hd, NoneType, env)
            val (_, env2) = elabPat(tl, NoneType, env1)
            (NoneType, env2)
          case Some(ListType(elemType)) =>
            val (hType, env1) = elabPat(hd, elemType, env)
            val (tType, env2) = elabPat(tl, ListType(elemType), env1)
            Approx.asListType(tType) match {
              case None =>
                (NoneType, env2)
              case Some(refinedT) =>
                (ListType(Subtype.meet(hType, refinedT)), env2)
            }
        }
      case CLiteral(_, data) =>
        (elabPatData(data, t, env), env)
      case CTuple(_, elems) =>
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
            val patTypes = elems.zip(elemTypes).map {
              case (elem, elemT) =>
                val (patType, env1) = elabPat(elem, elemT, envAcc)
                envAcc = env1
                patType
            }
            (TupleType(patTypes), envAcc)
        }
      case cv: CVar =>
        val patType = env.get(cv.name) match {
          case Some(vt) => Subtype.meet(vt, t)
          case None     => t
        }
        (patType, env + (cv.name -> patType))
      // $COVERAGE-OFF$
      case x => sys.error(s"unexpected $x")
      // $COVERAGE-ON$
    }
  private def elabBinaryElem(elem: CBitstr, env: Env): Env = env
}
