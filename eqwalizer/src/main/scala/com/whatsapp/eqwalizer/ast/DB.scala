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

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.config
import com.whatsapp.eqwalizer.io.{Beam, EData}

import java.nio.file.Paths

object DB {
  private def dirModules(dir: String): List[String] =
    Paths.get(dir).toFile.listFiles((_, f) => f.endsWith(".beam")).map(_.getName.dropRight(5)).toList
  private def appEbinDir(app: String): String =
    s"${config.libRoot}/$app/ebin"
  private lazy val otpEbinDirs: Map[String, String] = {
    val libRoot = config.otpLibRoot
    val libs = Paths.get(libRoot).toFile.listFiles().filter(_.isDirectory).map(_.getName)
    libs.map(dir => dir.split("-")(0) -> s"$libRoot/$dir/ebin").toMap
  }

  lazy val otpApps: Map[String, App] =
    otpEbinDirs.map { case (n, dir) => n -> App(n, dir, dirModules(dir)) }
  lazy val projectApps: Map[String, App] =
    config.apps.map(n => n -> App(n, appEbinDir(n), dirModules(appEbinDir(n)))).toMap
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
    ModuleStub(module, exports, imports, exportTypes, specs, types, skippedSpecs, skippedTypes)
  }

  private var moduleStubs: Map[String, ModuleStub] =
    Map.empty

  def getModuleStub(module: String): Option[ModuleStub] = {
    if (moduleStubs.contains(module))
      return Some(moduleStubs(module))
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(s"module $module is defined in ${appNames.mkString(", ")}")
    else {
      val moduleApi = loadModuleStub(apps(appNames.head), module)
      moduleStubs = moduleStubs.updated(module, moduleApi)
      Some(moduleApi)
    }
  }
}
