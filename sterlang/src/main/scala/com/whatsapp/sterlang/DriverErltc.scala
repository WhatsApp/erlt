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

import java.nio.file.Paths

import com.ericsson.otp.erlang.{OtpExternal, OtpOutputStream}
import com.whatsapp.sterlang.Etf._

import scala.collection.mutable

object DriverErltc extends DriverApi {

  def main(args: Array[String]): Unit =
    args match {
      case Array(file) => process(file)
      case _           => Console.out.println("StErlang. More info: https://github.com/WhatsApp/erlt")
    }

  private def process(etfFile: String): Unit = {
    val start = System.currentTimeMillis()
    val sterlangTime = System.currentTimeMillis() - start
    val result = doProcessFile(etfFile) match {
      case Left(error)       => convertError(error)
      case Right(hoverSpecs) => ETuple(List(EAtom("ok"), EList(hoverSpecs.map(Etf.hoverTypeInfoToEMap))))
    }
    val response = ETuple(List(result, ELong(sterlangTime)))
    stdoutResponse(response)
  }

  private def doProcessFile(etfFile: String): Either[SterlangError, List[Doc.HoverTypeInfo]] = {
    val mainFile = etfFile
    val rawProgram =
      try loadProgram(mainFile)
      catch { case error: SterlangError => return Left(error) }
    val vars = new Vars()
    val program = AstUtil.normalizeTypes(rawProgram)
    val depContext = loadContext(mainFile, program, vars)
    val context = depContext.extend(program)

    try {
      val astChecks = new AstChecks(context)
      astChecks.check(program)
      val elaborate = new Elaborate(vars, context, program)
      val (annDefs, env) = elaborate.elaborate()

      // TODO - https://github.com/WhatsApp/erlt/issues/336
      // if (options.contains("--check-patterns")) {
      //  val warnings = new PatternChecker(new TypesUtil(vars), context, program).warnings(annFuns)
      //  warnings.foreach(displayRangeError(erltFile, text, _))
      //}
      // checking them in the very end - since it is possible to present inferred types here
      // astChecks.checkPublicSpecs(program)
      val render = new Render(vars)
      val hoverSpecs = render.hoverSpecs(program, annDefs, env)
      val elabTypes = render.varTypes(annDefs)
      val elabVarHovers = elabTypes.map {
        case Doc.ElaboratedTypeInfo(_, range, typeRepr) => Doc.HoverTypeInfo(range, typeRepr)
      }
      Right(hoverSpecs ++ elabVarHovers)
    } catch { case error: SterlangError => Left(error) }
  }

  private def freshTypeVar(vars: Vars): Types.Type =
    Types.VarType(vars.tVar(Types.Open(0)))
  private def freshRowTypeVar(vars: Vars, kind: Types.RtVarKind): Types.RowType =
    Types.RowVarType(vars.rVar(Types.RowOpen(0, kind)))
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
      val rVars = AstUtil.collectNamedRowTypeVars(funType)
      val sSub: Expander.Sub =
        sVars.map { v => v -> Left(freshTypeVar(vars)) }.toMap
      val rSub: Expander.Sub =
        rVars.map { case (rv, kind) => rv.name -> Right(freshRowTypeVar(vars, kind)) }.toMap
      val eSub: Expander.Sub =
        sSub ++ rSub
      val specType = expander.mkType(funType, eSub)
      val expSpecType = expander.expandType(specType)
      val scheme = TU.generalize(0)(expSpecType)
      name -> scheme
    }.toMap
    Context(enumDefs, structDefs, aliases, opaques, env)
  }

  def loadProgram(file: String): Ast.Program =
    EtfErltc.programFromFile(file)

  private def convertPos(pos: Doc.Pos): ETerm =
    ETuple(List(ELong(pos.line.toLong), ELong(pos.column.toLong)))

  private def convertRange(range: Doc.Range): ETerm =
    ETuple(List(convertPos(range.start), convertPos(range.end)))

  private def convertError(error: SterlangError): ETerm =
    error match {
      case PosError(pos, title) =>
        ETuple(List(EAtom("error"), convertPos(pos), EString(title)))
      case RangeError(range, title, description) =>
        val msg = (List(title) ++ description.toList).mkString("\n")
        ETuple(List(EAtom("error"), convertRange(range), EString(msg)))
    }

  private def stdoutResponse(response: ETerm): Unit = {
    val bytes = {
      val outs = new OtpOutputStream()
      outs.write(OtpExternal.versionTag)
      outs.write_any(Etf.toJava(response))
      outs.toByteArray
    }

    System.out.write(bytes)
    System.out.flush()
  }
}
