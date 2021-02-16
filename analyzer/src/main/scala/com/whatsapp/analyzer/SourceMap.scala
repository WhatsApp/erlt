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

package com.whatsapp.analyzer

import erlang.forms.AbstractForm._

object SourceMap {
  var maps: Map[String, SourceMap] =
    Map.empty

  def getOrFind(module: String): SourceMap =
    get(module).getOrElse {
      // populates SourceMap as a side-effect
      BeamDb.getModuleApi(module).get
      get(module).get
    }

  def get(module: String): Option[SourceMap] =
    maps.get(module)

  def put(module: String, forms: List[AbstractForm]): Unit = {
    maps += module -> buildMap(module, forms)
  }

  def buildMap(module: String, forms: List[AbstractForm]): SourceMap = {
    var curFile: String =
      null
    var records: Map[String, String] =
      Map.empty
    var types: Map[(String, Int), String] =
      Map.empty
    var sourceFile: Option[String] = None
    for (form <- forms)
      form match {
        case AF_File(file) =>
          if (!sourceFile.isDefined)
            sourceFile = Some(file)
          curFile = file
        case AF_RecordDecl(name, _) =>
          records += name -> curFile
        case AF_TypeDecl(_, typeName, _, params) =>
          types += (typeName, params.size) -> curFile
        case _ =>
      }
    SourceMap(module, sourceFile.get, records, types)
  }
}

case class SourceMap(
    module: String,
    sourceFile: String,
    // recordName -> file
    records: Map[String, String],
    // (name, arity) -> file
    types: Map[(String, Int), String],
)
