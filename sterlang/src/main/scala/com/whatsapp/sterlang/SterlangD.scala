package com.whatsapp.sterlang

import java.util.concurrent.{Executor, Executors}

import scala.annotation.tailrec
import sys.process._
import com.ericsson.otp.erlang.{OtpErlangPid, OtpMbox, OtpNode}
import com.whatsapp.sterlang.Etf.{EAtom, ELong, EPid, ERef, EString, ETerm, ETuple}

object SterlangD {
  def serve(par: Int): Unit = {
    "epmd -daemon".!!
    val node = new OtpNode("sterlangd@localhost")
    new Server(node.createMbox("api"), Executors.newFixedThreadPool(par)).serve()
    node.close()
  }

  private[SterlangD] class Server(mbox: OtpMbox, executor: Executor) {
    @tailrec
    final def serve(): Unit = {
      val msg = Etf.fromJava(mbox.receive())
      msg match {
        case ETuple(List(EAtom("check"), EPid(from), ref: ERef, EString(etfFile))) =>
          executor.execute(() => handleCheck(from, ref, etfFile))
        case EAtom("exit") =>
          return
      }
      serve()
    }

    private def handleCheck(from: OtpErlangPid, ref: ERef, etfFile: String): Unit = {
      val start = System.currentTimeMillis()
      val result = processFile(etfFile) match {
        case Some(error) => convertError(error)
        case None        => ETuple(List(EAtom("ok")))
      }
      val sterlangTime = System.currentTimeMillis() - start
      val response = ETuple(List(ref, result, ELong(sterlangTime)))
      mbox.send(from, Etf.toJava(response))
    }

    private def processFile(etfFile: String): Option[SterlangError] = {
      val rawProgram =
        try DriverErltc.loadProgram(etfFile)
        catch {
          case error: SterlangError => return Some(error)
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
        case error: SterlangError => Some(error)
      }
    }
  }

  private def convertPos(pos: Doc.Pos): ETerm =
    ETuple(List(ELong(pos.line.toLong), ELong(pos.column.toLong)))

  private def convertRange(range: Doc.Range): ETerm =
    ETuple(List(convertPos(range.start), convertPos(range.end)))

  private def convertError(error: SterlangError): ETerm =
    error match {
      case PosError(pos, title) =>
        ETuple(List(EAtom("error"), convertPos(pos), EString(title)))
      case RangeError(range, title, description) =>
        val msg = (List(title) ++ description.toList).mkString("\n")
        ETuple(List(EAtom("error"), convertRange(range), EString(msg)))
    }
}
