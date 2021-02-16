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

package com.whatsapp.analyzer

import erlang.Beam
import erlang.Data.EList
import erlang.forms.AbstractForm.AbstractForm
import erlang.forms.AbstractFormConvert

import java.nio.file.Paths

object BeamDb {
  case class App(name: String, ebinDir: String, modules: List[String])

  private def dirModules(dir: String): List[String] =
    Paths
      .get(dir)
      .toFile
      .listFiles()
      .collect {
        case f if f.isFile && f.getName.endsWith(".beam") => f.getName.dropRight(5)
      }
      .toList

  private lazy val otpEbinDirs: Map[String, String] = {
    val dirs = Paths.get(CodeDirs.otpLibRoot).toFile.listFiles().filter(_.isDirectory).map(_.getName)
    dirs.map(dir => dir.split("-")(0) -> s"${CodeDirs.otpLibRoot}/$dir/ebin").toMap
  }

  private lazy val projectEbinDirs: Map[String, String] = {
    val dirs = CodeDirs.projectEbinDirs
    dirs.map(dir => CodeDirs.libraryName(dir) -> dir).toMap
  }

  lazy val otpApps: Map[String, App] =
    otpEbinDirs.map { case (n, dir) => n -> App(n, dir, dirModules(dir)) }
  lazy val projectApps: Map[String, App] =
    projectEbinDirs.map { case (n, dir) => n -> App(n, dir, dirModules(dir)) }
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

  private var moduleApis: Map[String, ModuleApi] =
    Map.empty

  private def loadModuleApi(app: App, module: String): ModuleApi = {
    val Some(EList(rawForms, None)) = Beam.loadAbstractForms(s"${app.ebinDir}/$module.beam")
    val forms = rawForms.map(AbstractFormConvert.convertForm(_, lite = true)).filter(_ != null)
    SourceMap.put(module, forms)
    ModuleApi(forms)
  }

  def loadModule(app: App, module: String, lite: Boolean = true): List[AbstractForm] = {
    val Some(EList(rawForms, None)) = Beam.loadAbstractForms(s"${app.ebinDir}/$module.beam")
    rawForms.map(AbstractFormConvert.convertForm(_, lite = lite)).filter(_ != null)
  }

  def getApp(module: String): Option[App] = {
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(s"module $module is defined in ${appNames.mkString(", ")}")
    else {
      Some(apps(appNames.head))
    }
  }

  def getModuleApi(module: String): Option[ModuleApi] =
    moduleApis.get(module).orElse {
      getApp(module) map { app =>
        val moduleApi = loadModuleApi(app, module)
        moduleApis = moduleApis.updated(module, moduleApi)
        moduleApi
      }
    }

  def getAbstractForms(module: String): Option[List[AbstractForm]] = {
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(s"module $module is defined in ${appNames.mkString(", ")}")
    else
      Some(loadModule(apps(appNames.head), module))
  }

  // For testing/debugging
  def preload(otpOnly: Boolean = false): Unit = {
    println("OTP")
    for { app <- otpApps.values.toList.sortBy(_.name) } {
      val start = System.currentTimeMillis()
      println()
      print(s"${app.name}")
      for { module <- app.modules } {
        moduleApis = moduleApis.updated(module, loadModuleApi(app, module))
        print(".")
      }
      val time = System.currentTimeMillis() - start
      print(s"  ${time}ms")
    }
    if (otpOnly) return
    println()
    println("PROJECT")
    for { app <- projectApps.values.toList.sortBy(_.name) } {
      val start = System.currentTimeMillis()
      println()
      print(s"${app.name}")
      for { module <- app.modules } {
        moduleApis = moduleApis.updated(module, loadModuleApi(app, module))
        print(".")
      }
      val time = System.currentTimeMillis() - start
      print(s"  ${time}ms")
    }
    println()
  }
}
