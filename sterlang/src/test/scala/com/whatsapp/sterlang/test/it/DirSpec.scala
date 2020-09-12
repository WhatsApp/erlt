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

import java.nio.file.Files

import com.whatsapp.sterlang.TypePrinter2.{TypeSchemes, Types}

abstract class DirSpec extends org.scalatest.funspec.AnyFunSpec {
  import java.io.File

  def testDir(iDirPath: String): Unit = {
    import sys.process._
    describe(iDirPath) {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!

      val file = new File(iDirPath)
      val moduleNames =
        file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl")).map(_.getName).map(_.dropRight(4))

      moduleNames.foreach { p =>
        val erlPath = s"$iDirPath/$p.erl"
        val etfPath = s"$oDirPath/$p.etf"
        it(erlPath) {
          testFile(erlPath, etfPath)
          testFileVerbose(erlPath, etfPath)
        }
      }
    }
  }

  def testFile(erlPath: String, etfPath: String): Unit = {
    SterlangTestUtil.processFile(erlPath, etfPath, TypeSchemes, "_ty", "ty")

    val myOutput = fileContent(erlPath + "._ty")
    val expectedOut = fileContent(erlPath + ".ty")
    assert(myOutput == expectedOut)

    new File(erlPath + "._ty").delete()
  }

  def testFileVerbose(f: String, etfPath: String): Unit = {
    SterlangTestUtil.processFile(f, etfPath, Types, "_vt", "vt")

    val myOutput = fileContent(f + "._vt")
    val expectedOut = fileContent(f + ".vt")
    assert(myOutput == expectedOut)

    new File(f + "._vt").delete()
  }

  def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }
}
