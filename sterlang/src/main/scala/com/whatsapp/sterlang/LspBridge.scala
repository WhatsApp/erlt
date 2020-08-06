package com.whatsapp.sterlang

import java.nio.file.{Files, Paths}
import com.ericsson.otp.erlang.{OtpExternal, OtpOutputStream}
import com.whatsapp.sterlang.etf._

object LspBridge {

  sealed trait Status
  case object OK extends Status
  case object Error extends Status

  case class Result(
      status: Status,
      specs: List[(String, String)],
      errors: List[SterlangError],
      warnings: List[SterlangError],
  )

  private def encodeResult(result: Result): ETerm = {
    val status = result.status match {
      case OK    => EAtom("ok")
      case Error => EAtom("error")
    }
    val specs =
      EList(result.specs.map { case (k, v) => ETuple(List(EString(k), EString(v))) })
    val errors =
      EList(result.errors.map(convertError))
    val warnings =
      EList(result.warnings.map(convertError))
    EMap(
      List(
        EAtom("status") -> status,
        EAtom("specs") -> specs,
        EAtom("errors") -> errors,
        EAtom("warnings") -> warnings,
      )
    )
  }

  private def convertError(err: SterlangError): ETerm =
    err match {
      case PositionedError(pos: Pos.SP, title, description) =>
        EMap(
          List(
            EAtom("range") -> convertRange(pos),
            EAtom("title") -> EString(title),
            EAtom("description") -> description.map(EString).getOrElse(EAtom("undefined")),
          )
        )
      case ParseError(loc) =>
        EMap(
          List(
            EAtom("location") -> convertLocation(loc),
            EAtom("title") -> EString("Parse Error"),
            EAtom("description") -> EAtom("undefined"),
          )
        )
    }

  private def convertLocation(loc: Pos.Loc): ETerm =
    ETuple(List(ELong(loc.line), ELong(loc.column)))

  private def convertRange(range: Pos.SP): ETerm =
    ETuple(List(convertLocation(range.start), convertLocation(range.end)))

}

class LspBridge(outputFile: String) {

  def report(result: LspBridge.Result): Unit = {
    val eResult = LspBridge.encodeResult(result)
    val jResult = etf.toJava(eResult)
    val etfBytes = {
      val stream = new OtpOutputStream
      stream.write1(OtpExternal.versionTag)
      stream.write_any(jResult)
      stream.toByteArray
    }
    Files.write(Paths.get(outputFile), etfBytes)
  }

}
