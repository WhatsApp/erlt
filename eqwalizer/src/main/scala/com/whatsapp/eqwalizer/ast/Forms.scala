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

import com.whatsapp.eqwalizer.ast.Exprs.Clause
import com.whatsapp.eqwalizer.ast.Types.{ConstrainedFunType, Type}

object Forms {
  sealed trait Form { val line: Int }
  case class Module(name: String)(val line: Int) extends Form
  case class Export(funs: List[Id])(val line: Int) extends Form
  case class Import(module: String, funs: List[Id])(val line: Int) extends Form
  case class ExportType(types: List[Id])(val line: Int) extends Form
  case class TypeDecl(id: Id, params: List[String], body: Type)(val line: Int) extends Form
  case class FunSpec(id: Id, types: List[ConstrainedFunType])(val line: Int) extends Form
  case class FunDecl(id: Id, clauses: List[Clause])(val line: Int) extends Form
  case class File(file: String, start: Int)(val line: Int) extends Form
}
