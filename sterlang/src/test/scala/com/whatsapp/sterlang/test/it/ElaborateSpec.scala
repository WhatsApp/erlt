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

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files

import com.whatsapp.sterlang._

class ElaborateSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/pos/src")
  testDir("examples/elm_core/src")
  testDir("examples/dev/src")
  testDir("examples/pattern/src")
  testDir("examples/sterlang/src")
  smokeTestFile("examples/elm_core/src", "basics")
  smokeTestDir("examples/dir/src")

  private def smokeTestDir(iDirPath: String): Unit = {
    it(s"smoke test: $iDirPath") {
      Driver.main(Array(iDirPath))
    }
  }

  private def smokeTestFile(iDirPath: String, module: String): Unit = {
    import sys.process._
    it(s"smoke test: $iDirPath/$module.erlt") {
      Driver.main(Array(s"$iDirPath/$module.erlt"))
      Driver.main(Array(s"$iDirPath/$module.erlt", "--check-patterns"))
    }
  }

  def testDir(iDirPath: String): Unit = {
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
          testFile(erltPath, etfPath)
          testFileVerbose(erltPath, etfPath)
        }
      }
    }
  }

  def testFile(erltPath: String, etfPath: String): Unit = {
    processFile(erltPath, etfPath, verbose = false, "_ty", "ty")

    val myOutput = fileContent(erltPath + "._ty")
    val expectedOut = fileContent(erltPath + ".ty")
    assert(myOutput == expectedOut)

    new File(erltPath + "._ty").delete()
  }

  def testFileVerbose(erltPath: String, etfPath: String): Unit = {
    processFile(erltPath, etfPath, verbose = true, "_vt", "vt")

    val myOutput = fileContent(erltPath + "._vt")
    val expectedOut = fileContent(erltPath + ".vt")
    assert(myOutput == expectedOut)

    new File(erltPath + "._vt").delete()
  }

  def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }

  def processFile(erltPath: String, etfPath: String, verbose: Boolean, tmpExt: String, outExt: String): Unit = {
    val rawProgram = Driver.loadProgram(etfPath, Driver.Dev)
    val program = AstUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Driver.loadContext(etfPath, program, vars, Driver.Dev).extend(program)
    new AstChecks(context).check(program)
    val (annDefs, env) = new Elaborate(vars, context, program).elaborate()

    val lines =
      if (verbose) Render(vars).varTypes(annDefs) else Render(vars).specs(annDefs, env)
    val output =
      lines.mkString("", "\n", "\n")

    {
      val w2 = new BufferedWriter(new FileWriter(erltPath + "." + tmpExt))
      w2.write(output)
      w2.close()
    }

    if (generateOut) {
      val w = new BufferedWriter(new FileWriter(erltPath + "." + outExt))
      w.write(output)
      w.close()
    }

    // Check pattern matching
    // TODO: apply to all files when ready.
    if (new File(erltPath).getParent == "examples/pattern/src") {
      val patternWarnings = new PatternChecker(new TypesUtil(vars), context, program).warnings(annDefs)
      assert(patternWarnings.isEmpty)
    }
  }
}
