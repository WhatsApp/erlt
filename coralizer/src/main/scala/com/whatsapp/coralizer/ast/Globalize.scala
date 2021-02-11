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

package com.whatsapp.coralizer.ast

import com.whatsapp.coralizer.ast.Forms._
import com.whatsapp.coralizer.ast.Types._

object Globalize {
  def globalize(module: String, t: Type): Type =
    t match {
      case FunType(args, resType) =>
        FunType(args.map(globalize(module, _)), globalize(module, resType))
      case LocalType(Id(n, arity), params) =>
        RemoteType(RemoteId(module, n, arity), params.map(globalize(module, _)))
      case RemoteType(id, params) =>
        RemoteType(id, params.map(globalize(module, _)))
      case TupleType(params) =>
        TupleType(params.map(globalize(module, _)))
      case ListType(et) =>
        ListType(globalize(module, et))
      case UnionType(params) =>
        UnionType(params.map(globalize(module, _)))
      case _: VarType | _: BuiltinType | _: AtomLitType | NilType |
          BinaryType =>
        t
      // $COVERAGE-OFF$
      case _ => sys.error(s"unexpected $t")
      // $COVERAGE-ON$
    }

  def globalizeSpec(module: String, spec: FunSpec): FunSpec = {
    val types = spec.types.map {
      case ConstrainedFunType(FunType(args, res), constraints) =>
        ConstrainedFunType(
          FunType(args.map(globalize(module, _)), globalize(module, res)),
          constraints.map {
            case Constraint(v, tp) => Constraint(v, globalize(module, tp))
          }
        )
    }
    spec.copy(types = types)(spec.line)
  }

  def globalizeTypeDecl(module: String, decl: TypeDecl): TypeDecl =
    decl.copy(body = globalize(module, decl.body))(decl.line)
}