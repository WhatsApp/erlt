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

package com.whatsapp.sterlang.dev

import java.nio.file.{Files, Paths}

import sys.process._
import com.whatsapp.sterlang.dev.forms.FormsConvertDev
import com.whatsapp.sterlang.Etf.{ETerm, readEtf}
import com.whatsapp.sterlang.{Ast, Convert, ModuleApi}

object EtfDev {
  def programFromFile(path: String): Ast.Program = {
    val etf = etfFromFileDev(path)
    val forms = FormsConvertDev.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  private def etfFromFileDev(path: String): ETerm = {
    val etfPath =
      if (path.endsWith(".etf")) {
        Paths.get(path)
      } else {
        val tmp = Files.createTempFile("sterlang", ".etf")
        s"./parser -ifile $path -ofile $tmp".!!
        tmp
      }
    readEtf(etfPath)
  }

  def programFromString(text: String): Ast.Program = {
    val etf = etfFromString(text)
    val forms = FormsConvertDev.fromEtf(etf)
    val elems = forms.flatMap(Convert.convertForm)
    Ast.RawProgram(elems).program
  }

  private def etfFromString(text: String): ETerm = {
    val tmpErlT = Files.createTempFile("sterlang", ".erlt")
    Files.write(tmpErlT, text.getBytes)
    val tmpEtf = Files.createTempFile("sterlang", ".etf")
    s"./parser -ifile $tmpErlT -ofile $tmpEtf".!!
    readEtf(tmpEtf)
  }

  def moduleApiFromFile(path: String): ModuleApi = {
    val etf = etfFromFileDev(path)
    val forms = FormsConvertDev.fromEtf(etf)
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
}
