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

class TypeErrorsSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/neg")
  testDir("examples/err")
  testDir("examples/err2")

  def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!

      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl")).map(_.getName).map(_.dropRight(4)).sorted

      moduleNames.foreach { p =>
        val erlPath = s"$iDirPath/$p.erl"
        val etfPath = s"$oDirPath/$p.etf"

        if (erlPath.endsWith("core.erl")) {
          ignore(erlPath) {}
        } else {
          it(erlPath) {
            processIllTyped(erlPath, etfPath)
          }
        }
      }
    }
  }

  private def processIllTyped(erlPath: String, etfPath: String): Unit = {
    val rawProgram = Main.loadProgram(etfPath)
    val program = AstUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = Main.loadContext(etfPath, program, vars).extend(program)
      new AstChecks(context).check(program)
      new Elaborate(vars, context, program).elaborateFuns(program.funs)
      fail(s"$erlPath should not type-check")
    } catch {
      case error: RangedError =>
        val actualErr = Main.errorString(erlPath, fileContent(erlPath), error)
        if (generateOut) {
          val expPath = Paths.get(erlPath + ".err.exp")
          Files.write(expPath, actualErr.getBytes)
        }

        val tmpPath = Paths.get(erlPath + "_err")
        Files.write(tmpPath, actualErr.getBytes)
        val expectedErr = fileContent(erlPath + ".err.exp")
        assert(expectedErr === actualErr)
        Files.delete(tmpPath)
    }
  }

  private def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }
}
