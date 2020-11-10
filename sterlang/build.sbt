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
    assemblyJarName in assembly := "sterlang.jar",
    resourceGenerators in Test += parser.taskValue,
  )

val parser = taskKey[Seq[File]]("Generate parser command line utility")
parser / fileInputs += (Test / sourceDirectory).value.toGlob / "erlang" / "parser.yrl".r

val bgStopAll = taskKey[Unit]("Stop all background jobs")
bgStopAll := bgList.value.foreach(bgJobService.value.stop)

commands += Command.command("sterlangd")(
    Command.process("bgStopAll ; test:bgRunMain com.whatsapp.sterlang.dev.SterlangD ", _)
)

// TODO - restore after re-integration
coverageMinimum := 98
coverageFailOnMinimum := true

parser := {
  val log = streams.value.log
  val erlangSrcDir = (Test / sourceDirectory).value / "erlang"
  val parserInput = erlangSrcDir / "parser"
  val parserOutput = (Test / resourceManaged).value / "parser"

  if (parser.inputFileChanges.hasChanges) {
    import scala.sys.process.Process
    log.info("parser: erlc parser.yrl")
    Process(Seq("erlc", "parser.yrl"), erlangSrcDir).!!
    log.info("parser: erlc parser.erl")
    Process(Seq("erlc", "parser.erl"), erlangSrcDir).!!
    log.info("parser: escript make_escript.erl")
    Process(Seq("escript", "make_escript.erl"), erlangSrcDir).!!
  }

  IO.copy(
    Seq((parserInput, parserOutput)),
    options = CopyOptions(true, true, true),
  )
  parserOutput.setExecutable(true)
  Seq(parserOutput)
}
