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

import com.whatsapp.corq.ast.{DB, Id, RemoteId}
import com.whatsapp.corq.ast.Types.{AnyType, FunType}

object Util {
  def getFunType(module: String, id: Id): Option[FunType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(module)
      hostModule = moduleStub.imports.getOrElse(id, module)
      ft <- getFunType(RemoteId(hostModule, id.name, id.arity))
    } yield ft

  def getFunType(fqn: RemoteId): Option[FunType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(fqn.module)
      spec <- moduleStub.specs.get(Id(fqn.name, fqn.arity)) if spec.types.size == 1
    } yield spec.types.head.ty

  def initClauseEnv(env: Env, clauseVars: Set[String]): Env = {
    env
//    var envAcc = env
//    for {
//      v <- clauseVars if !env.contains(v)
//    } envAcc = envAcc.updated(v, AnyType)
//    envAcc
//  }
    }
}
