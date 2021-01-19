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

import com.whatsapp.eqwalizer.ast.Types._

object Subtype {
  def subType(t1: Type, t2: Type): Boolean = {
    (t1, t2) match {
      case (_, _) if t1 == t2   => true
      case (_, AnyType)         => true
      case (NoneType, _)        => true
      case (UnionType(tys1), _) => tys1.forall(subType(_, t2))
      case (_, UnionType(tys2)) => tys2.exists(subType(t1, _))

      case (AtomLitType(_), AtomType) => true

      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(subType)
      case (NilType, ListType(_)) =>
        true
      case (ListType(et1), ListType(et2)) =>
        subType(et1, et2)
      case (FunType(args1, res1), FunType(args2, res2)) if args1.size == args2.size =>
        subType(res1, res2) && args2.lazyZip(args1).forall(subType)

      case _ =>
        false
    }
  }

  def join(t1: Type, t2: Type): Type =
    if (subType(t1, t2)) t2
    else if (subType(t2, t1)) t1
    else UnionType(List(t1, t2))

  def meet(t1: Type, t2: Type): Type =
    if (subType(t1, t2)) t1
    else if (subType(t2, t1)) t2
    else
      (t1, t2) match {
        case (UnionType(ty1s), _) => UnionType(ty1s.map(meet(_, t2)))
        case (_, UnionType(ty2s)) => UnionType(ty2s.map(meet(t1, _)))

        case (TupleType(elems1), TupleType(elems2)) if elems1.size == elems2.size =>
          TupleType(elems1.lazyZip(elems2).map(meet))
        case (ListType(et1), ListType(et2)) =>
          ListType(meet(et1, et2))
        case (FunType(args1, res1), FunType(args2, res2)) if args1.size == args2.size =>
          FunType(args1.lazyZip(args2).map(join), meet(res1, res2))

        case (_, _) =>
          NoneType
      }
}
