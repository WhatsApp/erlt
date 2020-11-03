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
import java.nio.file.{Files, Path, Paths}

import com.whatsapp.sterlang._

class TypeErrorsSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = true
  val mode: Driver.Mode = Driver.Erlt

  testDir("examples/neg/src")
  testDir("examples/err/src")
  testDir("examples/err2/src")

  def testDir(srcDir: String): Unit = {
    import sys.process._
    describe(srcDir) {
      val buildDir =
        Files.createTempDirectory("sterlang-test")
      val modules =
        new File(srcDir)
          .listFiles()
          .filter(f => f.isFile && f.getPath.endsWith(".erlt"))
          .map(_.getName)
          .map(_.dropRight(5))
          .sorted
      val moduleArgs =
        modules.map(_ ++ ".erlt").mkString(" ")

      mode match {
        case Driver.Dev =>
          s"./parser -idir $srcDir -odir $buildDir".!!
        case Driver.Erlt =>
          s"./erltc --build compile --src-dir $srcDir --build-dir $buildDir -o $buildDir $moduleArgs".!!
      }

      modules.foreach { module =>
        val erltPath = s"$srcDir/$module.erlt"

        if (erltPath.endsWith("core.erlt")) {
          ignore(erltPath) {}
        } else {
          it(erltPath) {
            processIllTyped(module, srcDir, buildDir, mode)
          }
        }
      }
    }
  }

  private def processIllTyped(module: String, sourceDir: String, buildDir: Path, mode: Driver.Mode): Unit = {
    val erltPath = s"$sourceDir/$module.erlt"
    val mainFile = s"$buildDir/$module.etf"
    val rawProgram = Driver.loadProgram(mainFile, mode)
    val program = AstUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = Driver.loadContext(mainFile, program, vars, mode).extend(program)
      val astChecks = new AstChecks(context)
      astChecks.check(program)
      new Elaborate(vars, context, program).elaborate()
      if (module.contains("_unspeced")) {
        astChecks.checkPublicSpecs(program)
      }
      fail(s"$mainFile should not type-check")
    } catch {
      case error: RangedError =>
        val actualErr = Driver.errorString(erltPath, fileContent(erltPath), error)
        if (generateOut) {
          val expPath = Paths.get(erltPath + ".err.exp")
          Files.write(expPath, actualErr.getBytes)
        }

        val tmpPath = Paths.get(erltPath + "_err")
        Files.write(tmpPath, actualErr.getBytes)
        val expectedErr = fileContent(erltPath + ".err.exp")
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
