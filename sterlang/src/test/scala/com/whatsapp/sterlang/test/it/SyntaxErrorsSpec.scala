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
import com.whatsapp.sterlang._
import com.whatsapp.sterlang.dev.DriverDev

import scala.util.Using

class SyntaxErrorsSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  testDir(srcDir = "examples/err_syntax/src", erltc = false)
  testDir(srcDir = "examples/sterlang_lint/src", erltc = true)

  def testDir(srcDir: String, erltc: Boolean): Unit = {
    import sys.process._
    describe(srcDir) {

      val buildDirDev = Files.createTempDirectory("sterlang")
      val buildDirErltc = Files.createTempDirectory("erlc")
      s"./parser -idir $srcDir -odir $buildDirDev".!!

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

      val erlCompatModules = modules.filterNot(_.endsWith("_erlt"))
      val erlcTmpDir = Files.createTempDirectory("erlc-in")
      erlCompatModules.foreach { m =>
        Files.copy(Paths.get(s"$srcDir/$m.erlt"), erlcTmpDir.resolve(s"$m.erl"))
      }
      val erlcInputs = erlCompatModules.map(m => s"$erlcTmpDir/$m.erl").mkString(" ")
      s"erlc -o $erlcTmpDir $erlcInputs".!!

      run(DriverDev, modules, srcDir, buildDirDev)

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
    modules.foreach { p =>
      val erltPath = s"$srcDir/$p.erlt"
      val etfPath = s"$buildDir/$p.etf"
      it(s"$erltPath - ${driver.getClass.getName}") {
        processIllSyntax(driver, erltPath, etfPath)
      }
    }

  def processIllSyntax(driver: DriverApi, erltPath: String, etfPath: String): Unit = {
    try {
      driver.loadProgram(etfPath)
      fail(s"$erltPath should generate an UnsupportedSyntaxError or a ParseError")
    } catch {
      case error: UnsupportedSyntaxError =>
        val errMsg = Util.rangeErrorString(erltPath, fileContent(erltPath), error)
        checkMsg(erltPath, errMsg)
      case error: ParseError =>
        val errMsg = Util.posErrorString(erltPath, fileContent(erltPath), error)
        checkMsg(erltPath, errMsg)
    }

    if (driver == DriverErltc) {
      val response = DriverErltc.process(etfPath)
      Etf.toJava(response)
    }
  }

  private def checkMsg(erltPath: String, actualErrMsg: String): Unit = {
    if (generateOut) {
      val expPath = Paths.get(erltPath + ".err.exp")
      Files.write(expPath, actualErrMsg.getBytes)
    }
    val tmpPath = Paths.get(erltPath + "_err")
    Files.write(tmpPath, actualErrMsg.getBytes)
    val expectedErr = fileContent(erltPath + ".err.exp")
    assert(expectedErr === actualErrMsg)
    Files.delete(tmpPath)
  }

  private def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }
}
