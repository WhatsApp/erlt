val dottyVersion = "3.0.0-M1"

scalaVersion := dottyVersion

name := "akka"

organization := "com.whatsapp"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.10",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
).map(_.withDottyCompat(scalaVersion.value))
