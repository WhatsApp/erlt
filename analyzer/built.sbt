name := "analyzer"

scalaVersion := "2.13.3"

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6" % "test"

initialCommands in console := """import com.whatsapp.eqwalizer._"""

javaOptions += "-Xss10M"

fork in run := true
