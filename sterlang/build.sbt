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

lazy val projectSetting = Seq(
  scalaVersion := "2.13.3",
  organization := "whatsapp",
  name := "sterlang",
  description := "Statically Typed Erlang",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.18",
  libraryDependencies += "org.erlang.otp" % "jinterface" % "1.6.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test,it",
  Test / testOptions += Tests.Argument("-oD"),
  IntegrationTest / testOptions += Tests.Argument("-oD"),
)

lazy val sterlang = (project in file("."))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .settings(projectSetting)
  .settings(
    mainClass in assembly := Some("com.whatsapp.sterlang.Main"),
    assemblyJarName in assembly := "sterlang.jar",
    resourceGenerators in Compile += erl2etf.taskValue,
  )

inConfig(IntegrationTest)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)

val erl2etf = taskKey[Seq[File]]("Generate erl2etf command line utility")
erl2etf / fileInputs += (Compile / sourceDirectory).value.toGlob / "erlang" / "erl2_epp.erl|erl2_parse.yrl|erl2etf.erl".r

erl2etf := {
  val log = streams.value.log
  val erlangSrcDir = (Compile / sourceDirectory).value / "erlang"
  val erl2etfInput = erlangSrcDir / "erl2etf"
  val erl2etfOutput = (Compile / resourceManaged).value / "erl2etf"

  if (erl2etf.inputFileChanges.hasChanges) {
    import scala.sys.process.Process
    log.info("building erl2etf")
    Process(Seq("erlc", "erl2_parse.yrl"), erlangSrcDir).!!
    Process(Seq("erlc", "erl2_epp.erl", "erl2_parse.erl", "erl2etf.erl"), erlangSrcDir).!!
    Process(Seq("escript", "make_escript.erl"), erlangSrcDir).!!
  }

  IO.copy(
    Seq((erl2etfInput, erl2etfOutput)),
    options = CopyOptions(true, true, true),
  )
  erl2etfOutput.setExecutable(true)
  Seq(erl2etfOutput)
}
