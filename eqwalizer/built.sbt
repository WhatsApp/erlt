name := "eqwalizer"

scalaVersion := "2.13.4"

libraryDependencies += "org.erlang.otp" % "jinterface" % "1.6.1"

libraryDependencies += "com.typesafe" % "config" % "1.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

lazy val testProjects = taskKey[Seq[File]]("build beam files")

resourceGenerators in Test += testProjects.taskValue

coverageMinimum := 95
coverageFailOnMinimum := true

testProjects / fileInputs += (baseDirectory.value / "test_projects" / "*" / "src" / "*.erl").toGlob

testProjects := {
  import sys.process.Process
  streams.value.log.out("test_projects / rebar3 compile")
  val exitCode = Process("rebar3 compile", baseDirectory.value / "test_projects").!(pLog(streams.value.log))
  assert(exitCode == 0, "`rebar3 compile` == 0")
  val output = baseDirectory.value / "test_projects" / "_build"
  Seq(output)
}

def pLog(log: Logger): scala.sys.process.ProcessLogger =
  new scala.sys.process.ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = log.err(s)
    def out(s: => String): Unit = log.out(s)
  }

val otpLibConf = taskKey[Seq[File]]("Generate Configuration for otp_lib")

otpLibConf := {
  import sys.process.Process
  IO.createDirectory((Compile / resourceManaged).value)
  val file = (Compile / resourceManaged).value / "otp_lib_root.conf"
  streams.value.log.out(s"generating $file")
  val exitCode = Process(s"escript otp_lib.escript $file").!(pLog(streams.value.log))
  assert(exitCode == 0, "`escript otp_lib.escript` == 0")
  Seq(file)
}

Compile / resourceGenerators += otpLibConf.taskValue
