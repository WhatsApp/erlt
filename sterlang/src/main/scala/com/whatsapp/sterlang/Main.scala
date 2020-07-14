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
import java.nio.file.Paths

import com.whatsapp.sterlang.patterns.PatternChecker
import org.fusesource.jansi.Ansi

import scala.collection.mutable

object Main {
  def ansi(s: String): String =
    Ansi.ansi.render(s).toString

  def main(args: Array[String]): Unit = {
    val options = args.toList.filter(_.startsWith("-")).toSet
    val realArgs = args.filter(a => !a.startsWith("-"))
    process(options, realArgs(0))
  }

  def process(options: Set[String], path: String): Unit = {
    val file = new File(path)
    assert(file.exists())
    if (file.isDirectory) {
      val files = file.listFiles().filter(f => f.isFile && f.getPath.endsWith(".erl"))
      files.sortBy(_.getPath).foreach(processFile(options))
    } else {
      processFile(options)(file)
    }
  }

  def processFile(options: Set[String])(file: File): Unit = {
    Console.println(ansi(s"@|bold ${file.getAbsolutePath}|@"))
    val (text, rawProgram) = loadProgram(file)
    val program = SyntaxUtil.normalizeTypes(rawProgram)
    val vars = new Vars()
    val depContext = loadContext(file, program, vars)
    val context = depContext.extend(program)
    try {
      new AstChecks(context).check(program)
      val elaborate = new Elaborate(vars, context, program)
      val (annFuns, env) = elaborate.elaborateFuns(program.funs)

      // Check patterns and print warnings, if any.
      if (options.contains("--check-patterns")) {
        val warnings = new PatternChecker(context).warnings(annFuns)
        warnings.foreach(printError(text, _))
      }

      Console.println(ansi(s"@|bold,green OK|@"))
      TypePrinter2(vars, None).printFunsTypeSchemes(annFuns, env)
    } catch {
      case error: PositionedError =>
        printError(text, error)
        sys.exit(2)
    }
  }

  def printError(text: String, error: PositionedError): Unit = {
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

  def loadContext(mainFile: File, program: S.Program, vars: Vars): Context = {
    val dir = Paths.get(mainFile.getAbsolutePath).getParent
    val TU = new TypesUtil(vars)
    var loaded = Set.empty[String]
    val queue = mutable.Queue.empty[String]
    SyntaxUtil.getDeps(program).foreach(queue.enqueue)
    var api = List.empty[ModuleApi]

    while (queue.nonEmpty) {
      val module = queue.dequeue()
      val file = dir.resolve(module + ".erl").toFile
      if (file.exists()) {
        val (_, rawProgram) = loadProgram(file)
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

  def loadProgram(file: File): (String, S.Program) = {
    val source = scala.io.Source.fromFile(file.getPath)
    val lines = source.getLines().toList
    val content = lines.mkString("\n")
    val program = etf.programFromFile(file.getPath)
    (content, program)
  }
}
