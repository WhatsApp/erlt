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

import com.whatsapp.sterlang.TypePrinter2.{TypeSchemes, Types}

abstract class DirSpec extends org.scalatest.funspec.AnyFunSpec {
  import java.io.File

  def testDir(dir: String): Unit = {
    describe(dir) {
      val file = new File(dir)
      val files = file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl"))
      val paths: Array[String] = files.map(_.getPath).sorted
      paths.foreach { p =>
        it(p) {
          testFile(p)
          testFileVerbose(p)
        }
      }
    }
  }

  def testFile(f: String): Unit = {
    SterlangTestUtil.processFile(f, TypeSchemes, "_ty", "ty")

    val myOutput = fileContent(f + "._ty")
    val expectedOut = fileContent(f + ".ty")
    assert(myOutput == expectedOut)

    new File(f + "._ty").delete()
  }

  def testFileVerbose(f: String): Unit = {
    SterlangTestUtil.processFile(f, Types, "_vt", "vt")

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
