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

  testDir("examples/err-syntax")

  def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!

      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl")).map(_.getName).map(_.dropRight(4)).sorted

      val erlCompatModules = moduleNames.filterNot(_.endsWith("_erlt"))
      val erlcOutDir = Files.createTempDirectory("erlc-out")
      val erlcInputs = erlCompatModules.map(m => s"$iDirPath/$m.erl").mkString(" ")
      s"erlc -o $erlcOutDir $erlcInputs".!!

      moduleNames.foreach { p =>
        val erlPath = s"$iDirPath/$p.erl"
        val etfPath = s"$oDirPath/$p.etf"
        it(erlPath) {
          processIllSyntax(erlPath, etfPath)
        }
      }
    }
  }

  def processIllSyntax(erlPath: String, etfPath: String): Unit = {
    try {
      Main.loadProgram(etfPath)
      fail(s"$erlPath should generate an UnsupportedSyntaxError")
    } catch {
      case error: UnsupportedSyntaxError =>
        val errMsg = Main.errorString(erlPath, fileContent(erlPath), error)
        checkMsg(erlPath, errMsg)
      case error: ParseError =>
        val errMsg = Main.parseErrorString(erlPath, fileContent(erlPath), error)
        checkMsg(erlPath, errMsg)
    }
  }

  private def checkMsg(erlPath: String, actualErrMsg: String): Unit = {
    if (generateOut) {
      val expPath = Paths.get(erlPath + ".err.exp")
      Files.write(expPath, actualErrMsg.getBytes)
    }
    val tmpPath = Paths.get(erlPath + "_err")
    Files.write(tmpPath, actualErrMsg.getBytes)
    val expectedErr = fileContent(erlPath + ".err.exp")
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
