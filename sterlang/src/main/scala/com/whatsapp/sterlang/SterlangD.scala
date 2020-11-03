package com.whatsapp.sterlang

import java.nio.file.{Files, Paths}
import java.util.concurrent.{Executor, Executors}

import scala.annotation.tailrec
import sys.process._
import com.ericsson.otp.erlang.{OtpErlangPid, OtpMbox, OtpNode}
import com.whatsapp.sterlang.Etf.{EAtom, EPid, ERef, EString, ETuple, ELong}

object SterlangD extends Executor {
  def main(args: Array[String]): Unit = {
    "epmd -daemon".!!
    val node = new OtpNode("sterlangd@localhost")
    new Server(node.createMbox("api"), mkExecutor(args)).serve()
    node.close()
  }

  override final def execute(command: Runnable): Unit = command.run()

  private def mkExecutor(args: Array[String]): Executor = {
    val optPar = if (args.isEmpty) None else args(0).toIntOption
    optPar.foreach { p => assert(p > 0, s"SterlangD: parallelism should be positive. Got: $p") }
    optPar.map(Executors.newFixedThreadPool).getOrElse(this)
  }

  private[SterlangD] class Server(mbox: OtpMbox, executor: Executor) {
    @tailrec
    final def serve(): Unit = {
      val msg = Etf.fromJava(mbox.receive())
      msg match {
        case ETuple(List(EAtom("check"), EPid(from), ref: ERef, EString(erltFile), EString(etfFile))) =>
          executor.execute(() => handleCheck(from, ref, erltFile, etfFile))
        case EAtom("exit") =>
          return
      }
      serve()
    }

    private def handleCheck(from: OtpErlangPid, ref: ERef, erltFile: String, etfFile: String): Unit = {
      val start = System.currentTimeMillis()
      val result = processFile(erltFile, etfFile) match {
        case Some(errorString) => ETuple(List(EAtom("error"), EString(errorString)))
        case None              => ETuple(List(EAtom("ok")))
      }
      val sterlangTime = System.currentTimeMillis() - start
      val response = ETuple(List(ref, result, ELong(sterlangTime)))
      mbox.send(from, Etf.toJava(response))
    }

    private def processFile(erltFile: String, etfFile: String): Option[String] = {
      lazy val text = new String(Files.readAllBytes(Paths.get(erltFile)))
      val rawProgram =
        try DriverErltc.loadProgram(etfFile)
        catch {
          case error: ParseError  => return Some(DriverErltc.parseErrorString(erltFile, text, error))
          case error: RangedError => return Some(DriverErltc.errorString(erltFile, text, error))
        }
      val vars = new Vars()
      val program = AstUtil.normalizeTypes(rawProgram)
      val context = DriverErltc.loadContext(etfFile, program, vars).extend(program)
      try {
        val astChecks = new AstChecks(context)
        astChecks.check(program)
        val elaborate = new Elaborate(vars, context, program)
        elaborate.elaborate()
        None
      } catch {
        case error: RangedError => Some(DriverErltc.errorString(erltFile, text, error))
      }
    }
  }
}
