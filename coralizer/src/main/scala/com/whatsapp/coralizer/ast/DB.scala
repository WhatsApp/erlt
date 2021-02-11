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

package com.whatsapp.coralizer.ast

import com.whatsapp.coralizer.ast.Forms._
import com.whatsapp.coralizer.config
import com.whatsapp.coralizer.io.{Beam, EData, RPC}
import com.whatsapp.coralizer.config
import erlang.CErl
import java.nio.file.Paths

import com.whatsapp.coralizer.tc.BuiltIn

import scala.util.Using

object DB {

  // BEGIN new
  def loadCoreModule(beamFilePath: String): CErl.CModule = {
    Using.resource(RPC.connect())(loadData(beamFilePath))
  }

  private def loadData(beamFilePath: String)(rpc: RPC): CErl.CModule = {
    val Some(module) = rpc.loadCoreForms(beamFilePath)
    // pprint.pprintln(module)
    module
  }
  // END

  private def dirModules(dir: String): List[String] =
    Paths
      .get(dir)
      .toFile
      .listFiles((_, f) => f.endsWith(".beam"))
      .map(_.getName.dropRight(5))
      .toList
  private def appEbinDir(app: String): String =
    s"${config.libRoot}/$app/ebin"
  private lazy val otpEbinDirs: Map[String, String] = {
    val libRoot = config.otpLibRoot
    val libs =
      Paths.get(libRoot).toFile.listFiles().filter(_.isDirectory).map(_.getName)
    libs.map(dir => dir.split("-")(0) -> s"$libRoot/$dir/ebin").toMap
  }

  lazy val otpApps: Map[String, App] =
    otpEbinDirs.map { case (n, dir) => n -> App(n, dir, dirModules(dir)) }
  lazy val projectApps: Map[String, App] =
    config.apps
      .map(n => n -> App(n, appEbinDir(n), dirModules(appEbinDir(n))))
      .toMap
  lazy val apps: Map[String, App] =
    otpApps ++ projectApps

  private lazy val module2App: Map[String, Set[String]] = {
    var result = Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    for {
      (_, App(appName, _, modules)) <- apps
      module <- modules
    } result = result.updated(module, result(module) + appName)
    result
  }

  lazy val dupMod2App: Map[String, Set[String]] =
    module2App.filter(_._2.size > 1)

  private def loadModuleStub(app: App, module: String): ModuleStub = {
    val forms = {
      val beamFile = s"${app.ebinDir}/$module.beam"
      val formsJ = Beam.loadAbstractFormsJ(beamFile)
      val formsDef = (for {
        i <- 0 until formsJ.arity()
        f = formsJ.elementAt(i)
        if !isFunForm(f)
      } yield f).toArray
      formsDef.flatMap(f => Convert.convertForm(EData.fromJava(f))).toList
    }
    var exports: Set[Id] = Set.empty
    var imports: Map[Id, String] = Map.empty
    var exportTypes: Set[Id] = Set.empty
    var specs: Map[Id, FunSpec] = Map.empty
    var types: Map[Id, TypeDecl] = Map.empty
    var skippedSpecs: Map[Id, SkippedFunSpec] = Map.empty
    var skippedTypes: Map[Id, SkippedTypeDecl] = Map.empty
    for (f <- forms)
      f match {
        case e: Export          => exports ++= e.funs
        case i: Import          => imports ++= i.funs.map(_ -> i.module)
        case e: ExportType      => exportTypes ++= e.types
        case t: TypeDecl        => types += t.id -> t
        case s: FunSpec         => specs += s.id -> s
        case t: SkippedTypeDecl => skippedTypes += t.id -> t
        case s: SkippedFunSpec  => skippedSpecs += s.id -> s
        case _                  =>
      }
    ModuleStub(
      module,
      exports,
      imports,
      exportTypes,
      specs,
      types,
      skippedSpecs,
      skippedTypes
    )
  }

  private var globalizedModuleStubs: Map[String, ModuleStub] =
    Map.empty
  private var expandedModuleStubs: Map[String, ModuleStub] =
    Map.empty

  def getModuleStub(module: String): Option[ModuleStub] = {
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(
        s"module $module is defined in ${appNames.mkString(", ")}"
      )
    else {
      val moduleApi = loadModuleStub(apps(appNames.head), module)
      Some(moduleApi)
    }
  }

  def beamLocation(module: String): Option[String] = {
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(
        s"module $module is defined in ${appNames.mkString(", ")}"
      )
    else {
      val app = apps(appNames.head)
      val beamFile = s"${app.ebinDir}/$module.beam"
      Some(beamFile)
    }
  }

  def getGlobalizedModuleStub(module: String): Option[ModuleStub] = {
    if (globalizedModuleStubs.contains(module))
      Some(globalizedModuleStubs(module))
    else
      getModuleStub(module).map { s =>
        val stub = s.copy(
          types = s.types.view
            .mapValues(Globalize.globalizeTypeDecl(module, _))
            .toMap,
          specs =
            s.specs.view.mapValues(Globalize.globalizeSpec(module, _)).toMap
              ++ BuiltIn.moduleInfoSpecs
        )
        globalizedModuleStubs = globalizedModuleStubs.updated(module, stub)
        stub
      }
  }

  def getExpandedModuleStub(module: String): Option[ModuleStub] = {
    if (expandedModuleStubs.contains(module))
      Some(expandedModuleStubs(module))
    else
      getGlobalizedModuleStub(module).map { s =>
        val types = s.types.values.map(Expand.expandTypeDecl).collect {
          case t: TypeDecl => t
        }
        val specs = s.specs.values.map(Expand.expandFunSpec).collect {
          case s: FunSpec => s
        }
        val tMap = types.map(t => t.id -> t).toMap
        val sMap = specs.map(s => s.id -> s).toMap
        val stub = s.copy(types = tMap, specs = sMap)
        expandedModuleStubs = expandedModuleStubs.updated(module, stub)
        stub
      }
  }
}
