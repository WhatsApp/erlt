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
      case (FunType(args1, res1), FunType(args2, res2)) if args1.size == args2.size =>
        subType(res1, res2) && args2.lazyZip(args1).forall(subType)

      case _ =>
        false
    }
  }
}
