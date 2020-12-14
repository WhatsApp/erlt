package com.whatsapp.eqwalizer

import com.whatsapp.analyzer.CodeDirs
import erlang.Beam
import erlang.Data.EList
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

  private lazy val dupMod2App: Map[String, Set[String]] =
    module2App.filter(_._2.size > 1)

  private var moduleApis: Map[String, ModuleApi] =
    Map.empty

  private def loadModuleApi(app: App, module: String): ModuleApi = {
    val Some(EList(rawForms, None)) = Beam.loadAbstractForms(s"${app.ebinDir}/$module.beam")
    val forms = rawForms.map(AbstractFormConvert.convertForm(_, lite = true)).filter(_ != null)
    ModuleApi(forms)
  }

  def getModuleApi(module: String): Option[ModuleApi] = {
    if (moduleApis.contains(module))
      return Some(moduleApis(module))
    val appNames = module2App(module)
    if (appNames.isEmpty)
      None
    else if (appNames.size > 1)
      throw new IllegalStateException(s"module $module is defined in ${appNames.mkString(", ")}")
    else {
      val moduleApi = loadModuleApi(apps(appNames.head), module)
      moduleApis = moduleApis.updated(module, moduleApi)
      Some(moduleApi)
    }
  }

  // For testing/debugging
  def preload(): Unit = {
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
