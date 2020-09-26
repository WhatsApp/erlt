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

import java.io.{BufferedWriter, File, FileWriter, StringWriter}
import java.nio.file.Files

import com.whatsapp.sterlang._

class ElaborateSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/pos")
  testDir("examples/elm-core")
  testDir("examples/dev")
  testDir("examples/pattern")
  smokeTestFile("examples/elm-core", "basics")
  smokeTestDir("examples/dir")

  private def smokeTestDir(iDirPath: String): Unit = {
    it(s"smoke test: $iDirPath") {
      Main.main(Array(iDirPath))
    }
  }

  private def smokeTestFile(iDirPath: String, module: String): Unit = {
    import sys.process._
    it(s"smoke test: $iDirPath/$module.erl") {
      val oDirPath = Files.createTempDirectory("sterlang")
      s"./parser -idir $iDirPath -odir $oDirPath".!!
      Main.main(Array(s"$iDirPath/$module.erl"))
      Main.main(Array(s"$iDirPath/$module.erl", s"$oDirPath/$module.etf"))
      Main.main(Array(s"$iDirPath/$module.erl", s"$oDirPath/$module.etf", "--check-patterns"))
    }
  }

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
        it(erlPath) {
          testFile(erlPath, etfPath)
          testFileVerbose(erlPath, etfPath)
        }
      }
    }
  }

  def testFile(erlPath: String, etfPath: String): Unit = {
    processFile(erlPath, etfPath, TypePrinter2.TypeSchemes, "_ty", "ty")

    val myOutput = fileContent(erlPath + "._ty")
    val expectedOut = fileContent(erlPath + ".ty")
    assert(myOutput == expectedOut)

    new File(erlPath + "._ty").delete()
  }

  def testFileVerbose(erlPath: String, etfPath: String): Unit = {
    processFile(erlPath, etfPath, TypePrinter2.Types, "_vt", "vt")

    val myOutput = fileContent(erlPath + "._vt")
    val expectedOut = fileContent(erlPath + ".vt")
    assert(myOutput == expectedOut)

    new File(erlPath + "._vt").delete()
  }

  def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }

  def processFile(erlPath: String, etfPath: String, mode: TypePrinter2.Mode, tmpExt: String, outExt: String): Unit = {
    val rawProgram = Main.loadProgram(etfPath)
    val program = AstUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = Main.loadContext(etfPath, program, vars).extend(program)
    new AstChecks(context).check(program)
    val (annDefs, env) = new Elaborate(vars, context, program).elaborateFuns(program.funs)

    val sw = new StringWriter
    val printer = TypePrinter2(vars, Some(sw))
    mode match {
      case TypePrinter2.TypeSchemes =>
        printer.printFunsTypeSchemes(annDefs, env)
      case TypePrinter2.Types =>
        printer.printFuns(annDefs)
    }

    {
      val w2 = new BufferedWriter(new FileWriter(erlPath + "." + tmpExt))
      w2.write(sw.toString)
      w2.close()
    }

    if (generateOut) {
      val w = new BufferedWriter(new FileWriter(erlPath + "." + outExt))
      w.write(sw.toString)
      w.close()
    }

    // Check pattern matching
    // TODO: apply to all files when ready.
    if (new File(erlPath).getParent == "examples/pattern")
      new PatternChecker(new TypesUtil(vars), context, program).check(annDefs)
  }
}
