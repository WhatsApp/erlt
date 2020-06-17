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
  scalaVersion := "2.13.2",
  organization := "whatsapp",
  name := "sterlang",
  description := "Statically Typed Erlang",
  version := "0.1-SNAPSHOT",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.18",
  libraryDependencies += "org.erlang.otp" % "jinterface" % "1.6.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test,it",
  Test / testOptions += Tests.Argument("-oD"),
  IntegrationTest / testOptions += Tests.Argument("-oD"),
)

lazy val sterlang = (project in file("."))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)
  .settings(projectSetting)
  .settings(mainClass in assembly := Some("com.whatsapp.sterlang.Main"))

inConfig(IntegrationTest)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)