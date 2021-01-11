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

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Forms._

package object ast {
  case class Id(name: String, arity: Int) {
    override def toString: String = s"$name/$arity"
  }
  case class RemoteId(module: String, name: String, arity: Int) {
    override def toString: String = s"$module:$name/$arity"
  }

  case class App(
      name: String,
      ebinDir: String,
      modules: List[String],
  )

  case class ModuleStub(
      module: String,
      exports: Set[Id],
      imports: Map[Id, String],
      exportTypes: Set[Id],
      specs: Map[Id, FunSpec],
      types: Map[Id, TypeDecl],
      skippedSpecs: Map[Id, SkippedFunSpec],
      skippedTypes: Map[Id, SkippedTypeDecl],
  )
}
