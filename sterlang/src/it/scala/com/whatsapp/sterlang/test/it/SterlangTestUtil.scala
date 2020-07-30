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
import com.whatsapp.sterlang.patterns.PatternChecker

object SterlangTestUtil {
  import java.io.{BufferedWriter, FileWriter, StringWriter}

  val generateOut = false

  def processFile(path: String, mode: TypePrinter2.Mode, tmpExt: String, outExt: String): Unit = {
    val (rawProgram) = Main.loadProgram(path)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Main.loadContext(path, program, vars).extend(program)
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

    // Check pattern matching
    // TODO: apply to all files when ready.
    if (new File(path).getParent == "examples/pattern")
      new PatternChecker(context).check(annDefs)
  }

  def processIllTyped(path: String): Boolean = {
    val rawProgram = Main.loadProgram(path)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = Main.loadContext(path, program, vars).extend(program)
      new AstChecks(context).check(program)
      val (annotatedFunctions, _) = new Elaborate(vars, context, program).elaborateFuns(program.funs)
      new PatternChecker(context).check(annotatedFunctions)
      false
    } catch {
      case _: PositionedError => true
    }
  }

  def processIllSyntax(path: String): Boolean = {
    try {
      Main.loadProgram(path)
      false
    } catch {
      case _: UnsupportedSyntaxError => true
    }
  }
}
