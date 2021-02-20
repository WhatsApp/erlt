name := "eqwalizer"

scalaVersion := "2.13.4"

libraryDependencies += "org.erlang.otp" % "jinterface" % "1.6.1"

libraryDependencies += "com.typesafe" % "config" % "1.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

lazy val testProjects = taskKey[Seq[File]]("build beam files")

resourceGenerators in Test += testProjects.taskValue

coverageMinimum := 98
coverageFailOnMinimum := true

testProjects / fileInputs += (baseDirectory.value / "test_projects" / "*" / "src" / "*.erl").toGlob

testProjects := {
  import sys.process.Process
  val cmd = "rebar3 build_info --to ../test_projects.build_info"
  streams.value.log.out(s"test_projects / $cmd")
  val exitCode = Process(cmd, baseDirectory.value / "test_projects").!(pLog(streams.value.log))
  assert(exitCode == 0, "`rebar3 ...` == 0")
  val output = baseDirectory.value / "test_projects.build_info"
  Seq(output)
}

def pLog(log: Logger): scala.sys.process.ProcessLogger =
  new scala.sys.process.ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = log.err(s)
    def out(s: => String): Unit = log.out(s)
  }
