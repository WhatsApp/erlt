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
import java.nio.file.{Files, Path, Paths}
import com.whatsapp.sterlang._
import com.whatsapp.sterlang.dev.DriverDev

import scala.util.Using

class ElaborateSpec extends org.scalatest.funspec.AnyFunSpec {

  val generateOut = false

  testDir("examples/pos/src", erltc = true)
  testDir("examples/elm_core/src", erltc = true)
  testDir("examples/dev/src", erltc = true)
  testDir("examples/pattern/src", erltc = true)
  testDir("examples/sterlang-dev/src", erltc = false)

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
        else it(s"$srcDir - erltc") {
          fail(s"$erltcCmd failed, log: $erltcLog")
        }
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
            testFile(driver, erltPath, mainFile)
            testFileVerbose(driver, erltPath, mainFile)
          }
        else ignore(s"$erltPath ${driver.getClass}") {}
      }
    }


  private def testFile(driver: DriverApi, erltPath: String, etfPath: String): Unit = {
    processFile(driver, erltPath, etfPath, verbose = false, "_ty", "ty")

    val myOutput = fileContent(erltPath + "._ty")
    val expectedOut = fileContent(erltPath + ".ty")
    assert(myOutput == expectedOut)

    new File(erltPath + "._ty").delete()
  }

  private def testFileVerbose(driver: DriverApi, erltPath: String, etfPath: String): Unit = {
    processFile(driver, erltPath, etfPath, verbose = true, "_vt", "vt")

    val myOutput = fileContent(erltPath + "._vt")
    val expectedOut = fileContent(erltPath + ".vt")
    assert(myOutput == expectedOut)

    new File(erltPath + "._vt").delete()
  }

  private def fileContent(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    val content = source.mkString
    source.close()
    content
  }

  private def processFile(driver: DriverApi, erltPath: String, etfPath: String, verbose: Boolean, tmpExt: String, outExt: String): Unit = {
    val rawProgram = driver.loadProgram(etfPath)
    val program = AstUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val context = driver.loadContext(etfPath, program, vars).extend(program)
    new AstChecks(context).check(program)
    val (annDefs, env) = new Elaborate(vars, context, program).elaborate()

    val output =
      if (verbose) {
        val infos = Render(vars).varTypes(annDefs, includeTopLevelFuns = true)
        val infoStrings = infos.map { info => "val " + info.varName + ": " + info.typeRepr }
        infoStrings.mkString("", "\n", "\n")
      } else {
        Render(vars).specs(annDefs, env).mkString("", "\n", "\n")
      }

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

    if (!verbose && driver == DriverErltc) {
      val response = DriverErltc.process(etfPath)
      Etf.toJava(response)
    }
  }
}
