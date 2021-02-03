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
import com.whatsapp.corq.ast.Types.{AnyType, FunType, NoneType}
import erlang.CErl._
import erlang.Data.ELong

object Util {
  def getApplyType(module: String, id: Id): Option[FunType] = {
    BuiltIn.letRecSpecialFunToType
      .get(id)
      .orElse(for {
        moduleStub <- DB.getExpandedModuleStub(module)
        hostModule = moduleStub.imports.getOrElse(id, module)
        ft <- getCallType(RemoteId(hostModule, id.name, id.arity))
      } yield ft)
  }

  def getCallType(fqn: RemoteId): Option[FunType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(fqn.module)
      spec <- moduleStub.specs.get(Id(fqn.name, fqn.arity))
      if spec.types.size == 1
    } yield spec.types.head.ty

  def initClauseEnv(env: Env, clauseVars: Set[CVar]): Env = {
    var envAcc = env
    for {
      v <- clauseVars if !env.contains(v)
    } envAcc = envAcc + (v -> AnyType)
    envAcc
  }
}
