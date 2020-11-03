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

package com.whatsapp.sterlang

import java.io.File
import java.nio.file.{Files, Paths}

import scala.collection.mutable

object ErltcDriver extends Driver {

  def main(args: Array[String]): Unit = {
    val options = args.toList.filter(_.startsWith("-")).toSet
    val files = args.filter(a => !a.startsWith("-"))
    process(options, files.toList)
  }

  private def process(options: Set[String], files: List[String]): Unit = {
    files match {
      case List(p) =>
        val file = new File(p)
        assert(file.exists())
        if (file.isDirectory) {
          val files: Array[File] = file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erlt"))
          files.sortBy(_.getPath).foreach(f => processFile(options, f.getPath, None))
        } else {
          processFile(options, p, None)
        }
      case List(erltFile, etfFile) =>
        processFile(options, erltFile, Some(etfFile))
    }
  }

  private def processFile(options: Set[String], erltFile: String, etfFile: Option[String]): Unit = {
    lazy val text = new String(Files.readAllBytes(Paths.get(erltFile)))
    val mainFile = etfFile.getOrElse(erltFile)
    val rawProgram =
      try {
        loadProgram(mainFile)
      } catch {
        // $COVERAGE-OFF$ interactive
        case error: ParseError =>
          displayParseError(erltFile, text, error)
          sys.exit(2)
        case error: RangedError =>
          displayError(erltFile, text, error)
          Console.err.println("DEBUG INFO:")
          error.printStackTrace(Console.err)
          sys.exit(2)
        // $COVERAGE-ON$
      }
    val program = AstUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val depContext = loadContext(mainFile, program, vars)
    val context = depContext.extend(program)

    try {
      val astChecks = new AstChecks(context)
      astChecks.check(program)
      val elaborate = new Elaborate(vars, context, program)
      val (annFuns, env) = elaborate.elaborate()

      // Check patterns and print warnings, if any.
      if (options.contains("--check-patterns")) {
        val warnings = new PatternChecker(new TypesUtil(vars), context, program).warnings(annFuns)
        // $COVERAGE-OFF$ interactive
        warnings.foreach(displayError(erltFile, text, _))
        // $COVERAGE-ON$
      }

      // checking them in the very end - since it is possible to present inferred types here
      astChecks.checkPublicSpecs(program)
    } catch {
      // $COVERAGE-OFF$ interactive
      case error: RangedError =>
        displayError(erltFile, text, error)
        sys.exit(2)
      // $COVERAGE-ON$
    }
  }

  private def freshTypeVar(vars: Vars): Types.Type =
    Types.VarType(vars.tVar(Types.Open(0)))
  private def freshRTypeVar(vars: Vars)(kind: Types.RtVarKind): Types.RowTypeVar =
    vars.rVar(Types.RowOpen(0, kind))

  def loadContext(mainFile: String, program: Ast.Program, vars: Vars): Context = {
    val ext = ".defs.etf"

    val dir = Paths.get(mainFile).getParent
    val TU = new TypesUtil(vars)
    var loaded = Set.empty[String]
    val queue = mutable.Queue.empty[String]
    AstUtil.getDeps(program).foreach(queue.enqueue)
    var api = List.empty[ModuleApi]

    while (queue.nonEmpty) {
      val module = queue.dequeue()
      val file = dir.resolve(module + ext).toFile
      if (file.exists()) {
        val rawProgram = loadProgram(file.getPath)
        val program = AstUtil.normalizeTypes(rawProgram)
        api = AstUtil.moduleApi(module, program) :: api
        val moduleDeps = AstUtil.getDeps(program)
        loaded += module
        moduleDeps.filterNot(loaded).foreach(queue.enqueue)
      } else {
        Console.println(s"Warning: cannot load module $module")
      }
    }

    val enumDefs = api.flatMap(_.enumDefs)
    val structDefs = api.flatMap(_.structDefs)
    val specs = api.flatMap(_.specs)
    val aliases = nativeAliases ++ api.flatMap(_.aliases)
    val expander = new Expander(aliases, () => freshTypeVar(vars), freshRTypeVar(vars))
    val opaques = nativeOpaques ++ api.flatMap(_.opaques)
    val env = specs.map { spec =>
      val name = spec.name.stringId
      val funType = spec.funType
      val sVars = AstUtil.collectNamedTypeVars(funType)
      val sub = sVars.map { v => v -> freshTypeVar(vars) }.toMap
      val eSub: Expander.Sub =
        sub.view.mapValues(Left(_)).toMap
      val specType = expander.mkType(funType, eSub)
      val expSpecType = expander.expandType(specType)
      val scheme = TU.generalize(0)(expSpecType)
      name -> scheme
    }.toMap
    Context(enumDefs, structDefs, aliases, opaques, env)
  }

  def loadProgram(file: String): Ast.Program =
    etf.programFromFileErlt(file)

  // $COVERAGE-OFF$ interactive
  private def displayError(inputPath: String, inputContent: String, error: RangedError): Unit =
    Console.println(errorString(inputPath, inputContent, error))

  private def displayParseError(inputPath: String, inputContent: String, error: ParseError): Unit =
    Console.println(parseErrorString(inputPath, inputContent, error))
  // $COVERAGE-ON$ interactive

  def errorString(inputPath: String, inputContent: String, error: RangedError): String = {
    val RangedError(range, title, description) = error
    val ranger = Doc.Ranger(inputContent, range.start, range.end)
    val msgTitle = Doc.title(error.severity, inputPath, range.start)
    val descText = description.map(_ ++ "\n").getOrElse("")
    msgTitle ++ "\n" ++ title ++ "\n" ++ descText ++ ranger.decorated ++ "\n"
  }

  def parseErrorString(inputPath: String, inputContent: String, error: ParseError): String = {
    val ParseError(pos) = error
    val locator = Doc.Locator(inputContent, pos)
    val msgTitle = Doc.title(Error, inputPath, pos)
    s"$msgTitle\nParse Error\n${locator.longString}\n"
  }
}
