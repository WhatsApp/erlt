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

import com.whatsapp.sterlang.patterns.PatternChecker
import org.fusesource.jansi.Ansi

import scala.collection.mutable

object Main {
  def ansi(s: String): String =
    Ansi.ansi.render(s).toString

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
          val files: Array[File] = file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl"))
          files.sortBy(_.getPath).foreach(f => processFile(options, f.getPath, None))
        } else {
          processFile(options, p, None)
        }
      case List(erlFile, etfFile) =>
        processFile(options, erlFile, Some(etfFile))
    }
  }

  private def processFile(options: Set[String], erlFile: String, etfFile: Option[String]): Unit = {
    Console.println(ansi(s"@|bold ${erlFile}|@"))
    lazy val text = new String(Files.readAllBytes(Paths.get(erlFile)))
    val mainFile = etfFile.getOrElse(erlFile)
    val rawProgram =
      try {
        loadProgram(mainFile)
      } catch {
        case error: ParseError =>
          printParseError(text, error)
          sys.exit(2)
        case error: PositionedError =>
          printError(text, error)
          Console.err.println("DEBUG INFO:")
          error.printStackTrace(Console.err)
          sys.exit(2)
      }
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val depContext = loadContext(mainFile, program, vars)
    val context = depContext.extend(program)

    try {
      new AstChecks(context).check(program)
      val elaborate = new Elaborate(vars, context, program)
      val (annFuns, env) = elaborate.elaborateFuns(program.funs)

      // Check patterns and print warnings, if any.
      if (options.contains("--check-patterns")) {
        val warnings = new PatternChecker(vars, context, program).warnings(annFuns)
        warnings.foreach(printError(text, _))
      }

      SyntaxUtil.checkPublicSpecs(program)
    } catch {
      case error: PositionedError =>
        printError(text, error)
        sys.exit(2)
    }
  }

  private def printParseError(text: String, error: ParseError): Unit = {
    val ParseError(loc) = error
    val locator = Pos.Locator(text, loc)
    val cTitle = ansi(s"@|bold,red Parse error at $loc|@")
    val cQuote = ansi(s"@|magenta,bold ${locator.longString}|@")
    Console.println(cTitle)
    Console.println(cQuote)
  }

  private def printError(text: String, error: PositionedError): Unit = {
    val PositionedError(pos: Pos.SP, title, description) = error
    val ranger = Pos.Ranger(text, pos.start, pos.end)

    val cTitle = ansi(s"@|bold,red $title at ${pos.start}|@")
    val cQuote = ansi(s"${ranger.prefix}@|magenta,bold ${ranger.text}|@${ranger.suffix}")
    Console.println(cTitle)
    Console.println(cQuote)
    description.foreach(Console.println)
    Console.println()
  }

  private def freshTypeVar(vars: Vars): Types.Type =
    Types.VarType(vars.tVar(Types.Open(0)))
  private def freshRTypeVar(vars: Vars)(kind: Types.RtVarKind): Types.RowTypeVar =
    vars.rVar(Types.RowOpen(0, kind))

  def loadContext(mainFile: String, program: S.Program, vars: Vars): Context = {
    val ext = mainFile.takeRight(4)
    val dir = Paths.get(mainFile).getParent
    val TU = new TypesUtil(vars)
    var loaded = Set.empty[String]
    val queue = mutable.Queue.empty[String]
    SyntaxUtil.getDeps(program).foreach(queue.enqueue)
    var api = List.empty[ModuleApi]

    while (queue.nonEmpty) {
      val module = queue.dequeue()
      val file = dir.resolve(module + ext).toFile
      if (file.exists()) {
        val rawProgram = loadProgram(file.getPath)
        val program = SyntaxUtil.normalizeTypes(rawProgram)
        api = SyntaxUtil.moduleApi(module, program) :: api
        val moduleDeps = SyntaxUtil.getDeps(program)
        loaded += module
        moduleDeps.filterNot(loaded).foreach(queue.enqueue)
      } else {
        Console.println(s"Warning: cannot load module $module")
      }
    }

    val enumDefs = api.flatMap(_.enumDefs)
    val specs = api.flatMap(_.specs)
    val aliases = nativeAliases ++ api.flatMap(_.aliases)
    val expander = new Expander(aliases, () => freshTypeVar(vars), freshRTypeVar(vars))
    val opaques = nativeOpaques ++ api.flatMap(_.opaques)
    val env = specs.map { spec =>
      val name = spec.name.stringId
      val funType = spec.funType
      val sVars = SyntaxUtil.collectNamedTypeVars(funType)
      val sub = sVars.map { v => v -> freshTypeVar(vars) }.toMap
      val specType = expander.mkType(funType, sub)
      val schema = TU.generalize(0)(specType)
      name -> schema
    }.toMap
    Context(enumDefs, List.empty, aliases, opaques, env)
  }

  def loadProgram(file: String): S.Program = {
    etf.programFromFile(file)
  }
}
