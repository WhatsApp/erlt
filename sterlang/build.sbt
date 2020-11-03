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

val ciSensitiveScalacOptions =
  if (sys.env.contains("GITHUB_ACTIONS")) Seq("-deprecation", "-feature", "-Xno-patmat-analysis")
  else Seq("-deprecation", "-feature")

lazy val projectSetting = Seq(
  scalaVersion := "2.13.3",
  organization := "whatsapp",
  name := "sterlang",
  description := "Statically Typed Erlang",
  scalacOptions ++= ciSensitiveScalacOptions,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  Test / testOptions += Tests.Argument("-oD"),
  test in assembly := {},
  fork in run := true,
)

lazy val sterlang = (project in file("."))
  .settings(projectSetting)
  .settings(
    mainClass in assembly := Some("com.whatsapp.sterlang.ErltcDriver"),
    assemblyJarName in assembly := "sterlang.jar",
    resourceGenerators in Compile += parser.taskValue,
  )

val parser = taskKey[Seq[File]]("Generate parser command line utility")
parser / fileInputs += (Compile / sourceDirectory).value.toGlob / "erlang" / "parser.yrl".r

// TODO - restore after re-integration
coverageMinimum := 98
coverageFailOnMinimum := true

parser := {
  val log = streams.value.log
  val erlangSrcDir = (Compile / sourceDirectory).value / "erlang"
  val parserInput = erlangSrcDir / "parser"
  val parserOutput = (Compile / resourceManaged).value / "parser"

  if (parser.inputFileChanges.hasChanges) {
    import scala.sys.process.Process
    Process(Seq("erlc", "parser.yrl"), erlangSrcDir).!!
    Process(Seq("erlc", "parser.erl"), erlangSrcDir).!!
    Process(Seq("escript", "make_escript.erl"), erlangSrcDir).!!
  }

  IO.copy(
    Seq((parserInput, parserOutput)),
    options = CopyOptions(true, true, true),
  )
  parserOutput.setExecutable(true)
  Seq(parserOutput)
}
