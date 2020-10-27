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

class SyntaxErrorsSpec extends org.scalatest.funspec.AnyFunSpec {
  val generateOut = false

  testDir("examples/err_syntax/src")

  def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!

      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt")).map(_.getName).map(_.dropRight(5)).sorted

      val erlCompatModules = moduleNames.filterNot(_.endsWith("_erlt"))
      val erlcTmpDir = Files.createTempDirectory("erlc-in")
      erlCompatModules.foreach { m =>
        Files.copy(Paths.get(s"$iDirPath/$m.erlt"), erlcTmpDir.resolve(s"$m.erl"))
      }
      val erlcInputs = erlCompatModules.map(m => s"$erlcTmpDir/$m.erl").mkString(" ")
      val cmd = s"erlc -o $erlcTmpDir $erlcInputs"
      s"erlc -o $erlcTmpDir $erlcInputs".!!

      moduleNames.foreach { p =>
        val erltPath = s"$iDirPath/$p.erlt"
        val etfPath = s"$oDirPath/$p.etf"
        it(erltPath) {
          processIllSyntax(erltPath, etfPath)
        }
      }
    }
  }

  def processIllSyntax(erltPath: String, etfPath: String): Unit = {
    try {
      Driver.loadProgram(etfPath)
      fail(s"$erltPath should generate an UnsupportedSyntaxError or a ParseError")
    } catch {
      case error: UnsupportedSyntaxError =>
        val errMsg = Driver.errorString(erltPath, fileContent(erltPath), error)
        checkMsg(erltPath, errMsg)
      case error: ParseError =>
        val errMsg = Driver.parseErrorString(erltPath, fileContent(erltPath), error)
        checkMsg(erltPath, errMsg)
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
