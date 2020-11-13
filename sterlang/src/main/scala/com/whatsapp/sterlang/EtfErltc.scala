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

package com.whatsapp.sterlang

import java.nio.file.Paths

import com.whatsapp.sterlang.Etf.{ETerm, readEtf}
import com.whatsapp.sterlang.forms.FormsConvert

object EtfErltc extends EtfApi {
  def programFromFile(path: String): Ast.Program = {
    val etf = etfFromFileErlt(path)
    val forms = FormsConvert.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  def moduleApiFromFile(path: String): ModuleApi = {
    val etf = etfFromFileErlt(path)
    val forms = FormsConvert.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    val program = Ast.RawProgram(elems).program

    ModuleApi(
      enumDefs = program.enumDefs,
      structDefs = program.structDefs,
      aliases = program.typeAliases,
      specs = program.specs,
      opaques = List.empty,
    )
  }

  private def etfFromFileErlt(file: String): ETerm =
    readEtf(Paths.get(file))
}
