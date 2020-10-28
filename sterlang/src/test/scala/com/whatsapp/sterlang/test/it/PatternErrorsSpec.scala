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
import java.nio.file.{Files, Paths}

import com.whatsapp.sterlang._

class PatternErrorsSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/pattern_error/src")

  private def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!

      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt")).map(_.getName).map(_.dropRight(5)).sorted

      moduleNames.foreach { p =>
        val erltPath = s"$iDirPath/$p.erlt"
        val etfPath = s"$oDirPath/$p.etf"
        it(erltPath) {
          processIllPatterns(erltPath, etfPath)
        }
      }
    }
  }

  private def processIllPatterns(erltPath: String, etfPath: String): Unit = {
    val rawProgram = Driver.loadProgram(etfPath, Driver.Dev)
    val program = AstUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Driver.loadContext(etfPath, program, vars, Driver.Dev).extend(program)
    new AstChecks(context).check(program)
    val (annotatedFunctions, _) = new Elaborate(vars, context, program).elaborate()
    val warnings = new PatternChecker(new TypesUtil(vars), context, program).warnings(annotatedFunctions)
    assert(warnings.nonEmpty)

    val actualErr = warnings.map(Driver.errorString(erltPath, fileContent(erltPath), _)).mkString("\n")
    if (generateOut) {
      val expPath = Paths.get(erltPath + ".warn.exp")
      Files.write(expPath, actualErr.getBytes)
    }

    val tmpPath = Paths.get(erltPath + "_err")
    Files.write(tmpPath, actualErr.getBytes)
    val expectedErr = fileContent(erltPath + ".warn.exp")
    assert(expectedErr === actualErr)
    Files.delete(tmpPath)
  }

  private def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }
}
