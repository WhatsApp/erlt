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

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

object Subst {
  def subst(s: Map[String, Type], t: Type): Type =
    t match {
      case FunType(args, resType) =>
        FunType(args.map(subst(s, _)), subst(s, resType))
      case LocalType(id, params) =>
        LocalType(id, params.map(subst(s, _)))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(subst(s, _)))
      case TupleType(params) =>
        TupleType(params.map(subst(s, _)))
      case ListType(elemT) =>
        ListType(subst(s, elemT))
      case UnionType(params) =>
        UnionType(params.map(subst(s, _)))
      case VarType(v) =>
        s.getOrElse(v, t)
      case _: BuiltinType | _: AtomLitType | NilType | BinaryType | _: RecordType =>
        t
    }
}
