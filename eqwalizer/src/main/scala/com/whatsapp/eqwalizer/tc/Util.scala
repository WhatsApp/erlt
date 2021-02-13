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

import com.whatsapp.eqwalizer.ast.Forms.RecDecl
import com.whatsapp.eqwalizer.ast.{DB, Id, RemoteId}
import com.whatsapp.eqwalizer.ast.Types.{AnyType, FunishType}

object Util {
  def getFunType(module: String, id: Id): Option[FunishType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(module)
      hostModule = moduleStub.imports.getOrElse(id, module)
      ft <- getFunType(RemoteId(hostModule, id.name, id.arity))
    } yield ft

  def getRecord(module: String, name: String): Option[RecDecl] =
    for {
      moduleStub <- DB.getExpandedModuleStub(module)
      recRecl <- moduleStub.records.get(name)
    } yield recRecl

  def getFunType(fqn: RemoteId): Option[FunishType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(fqn.module)
      spec <- moduleStub.specs.get(Id(fqn.name, fqn.arity)) if spec.types.size == 1
    } yield spec.types.head.ty

  def enterScope(env0: Env, scopeVars: Set[String]): Env = {
    var env = env0
    for {
      v <- scopeVars if !env0.contains(v)
    } env = env.updated(v, AnyType)
    env
  }

  def exitScope(env0: Env, env1: Env, scopeVars: Set[String]): Env = {
    val allVars = env0.keySet ++ scopeVars
    env1.view.filterKeys(allVars).toMap
  }
}
