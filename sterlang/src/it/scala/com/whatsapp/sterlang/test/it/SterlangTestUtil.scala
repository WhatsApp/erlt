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

package com.whatsapp.sterlang.test.it

import java.io.File

import com.whatsapp.sterlang._

object SterlangTestUtil {
  import java.io.{BufferedWriter, FileWriter, StringWriter}

  val generateOut = false

  def processFile(path: String, mode: TypePrinter2.Mode, tmpExt: String, outExt: String): Unit = {
    val file = new File(path)
    val rawProgram = Main.loadProgram(file, false)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Main.loadContext(file, program, vars).extend(program)
    new AstChecks(context).check(program)
    val (annDefs, env) = new Elaborate(vars, context, program).elaborateFuns(program.funs)

    val sw = new StringWriter
    val printer = TypePrinter2(vars, Some(sw))
    mode match {
      case TypePrinter2.TypeSchemes =>
        printer.printFunsTypeSchemes(annDefs, env)
      case TypePrinter2.Types =>
        printer.printFuns(annDefs)
    }

    {
      val w2 = new BufferedWriter(new FileWriter(path + "." + tmpExt))
      w2.write(sw.toString)
      w2.close()
    }

    if (generateOut) {
      val w = new BufferedWriter(new FileWriter(path + "." + outExt))
      w.write(sw.toString)
      w.close()
    }
  }

  def processIllTyped(path: String): Boolean = {
    val file = new File(path)
    val rawProgram = Main.loadProgram(file, false)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = Main.loadContext(file, program, vars).extend(program)
      new AstChecks(context).check(program)
      new Elaborate(vars, context, program).elaborateFuns(program.funs)
      false
    } catch {
      case _:PositionedError => true
    }
  }
}