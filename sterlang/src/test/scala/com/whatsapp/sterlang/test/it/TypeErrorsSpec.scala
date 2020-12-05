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

import java.nio.file.{Files, Path, Paths}
import scala.util.Using

import com.whatsapp.sterlang._
import com.whatsapp.sterlang.dev.DriverDev

class TypeErrorsSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/neg/src", erltc = true)
  testDir("examples/err/src", erltc = true)
  testDir("examples/err2/src", erltc = true)
  testDir("examples/err3/src", erltc = false)
  testDir("examples/err_lint/src", erltc = false)

  def testDir(srcDir: String, erltc: Boolean): Unit = {
    import sys.process._
    describe(srcDir) {
      val buildDirDev =
        Files.createTempDirectory("sterlang-test")
      val buildDirErltc =
        Files.createTempDirectory("sterlang-test")
      val modules =
        Paths
          .get(srcDir)
          .toFile
          .listFiles()
          .filter(f => f.isFile && f.getPath.endsWith(".erlt"))
          .map(_.getName)
          .map(_.dropRight(5))
          .sorted
      val moduleArgs =
        modules.map(_ ++ ".erlt").mkString(" ")

      val parserYrlCmd = s"./parser -idir $srcDir -odir $buildDirDev"

      val parserYrlLog = Files.createTempFile("parser.yrl", null).toFile
      val parserYrlSuccess = Using.resource(ProcessLogger(parserYrlLog)) { log => parserYrlCmd.!(log) == 0 }

      if (parserYrlSuccess) run(DriverDev, modules, srcDir, buildDirDev)
      else it(s"$srcDir - parser.yrl") { fail(s"$parserYrlCmd failed, log: $parserYrlLog") }

      if (erltc) {
        val erltcCmd =
          s"./erltc --build compile --src-dir $srcDir --build-dir $buildDirErltc -o $buildDirErltc --skip-type-checking $moduleArgs"
        val erltcLog = Files.createTempFile("erltc", null).toFile
        val erltcSuccess = Using.resource(ProcessLogger(erltcLog)) { log => erltcCmd.!(log) == 0 }
        if (erltcSuccess) run(DriverErltc, modules, srcDir, buildDirErltc)
        else it(s"$srcDir - erltc") { fail(s"$erltcCmd failed, log: $erltcLog") }
      }
    }
  }

  private def run(driver: DriverApi, modules: Array[String], srcDir: String, buildDir: Path): Unit =
    modules.foreach { module =>
      val erltPath = s"$srcDir/$module.erlt"
      if (erltPath.endsWith("core.erlt")) ignore(s"$erltPath ${driver.getClass}") {}
      else {
        val erltPath = s"$srcDir/$module.erlt"
        val mainFile = s"$buildDir/$module.etf"
        if (Files.exists(Paths.get(mainFile)))
          it(s"$erltPath ${driver.getClass.getSimpleName}") {
            processIllTyped(driver, module, srcDir, buildDir)
          }
        else ignore(s"$erltPath ${driver.getClass}") {}
      }
    }

  private def processIllTyped(driver: DriverApi, module: String, sourceDir: String, buildDir: Path): Unit = {
    val erltPath = s"$sourceDir/$module.erlt"
    val mainFile = s"$buildDir/$module.etf"
    val rawProgram = driver.loadProgram(mainFile)
    val program = AstUtil.normalizeTypes(rawProgram)
    try {
      val vars = new Vars()
      val context = driver.loadContext(mainFile, program, vars).extend(program)
      val astChecks = new AstChecks(context)
      astChecks.check(program)
      new Elaborate(vars, context, program).elaborate()
      fail(s"$mainFile should not type-check")
    } catch {
      case error: RangeError =>
        val actualErr = Util.rangeErrorString(erltPath, fileContent(erltPath), error)
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
