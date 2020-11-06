val dottyVersion = "0.27.0-RC1"

scalaVersion := dottyVersion

name := "akka"

organization := "com.whatsapp"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.10",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
).map(_.withDottyCompat(scalaVersion.value))
