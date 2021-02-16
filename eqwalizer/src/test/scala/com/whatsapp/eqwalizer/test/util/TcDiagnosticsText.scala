package com.whatsapp.eqwalizer.test.util

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.tc.TcDiagnostics.TypeError

import java.nio.file.{Files, Paths}

object TcDiagnosticsText {
  private val width: Int = 42

  private implicit class TextOps(text: String) {
    def colTrim(width: Int, lastColumn: Boolean = false): String =
      if (text.length <= width) {
        if (lastColumn) text else text.padTo(width, ' ')
      } else {
        text.take(width - 2) ++ "……"
      }
  }

  sealed trait Status
  object NoSpecStatus extends Status {
    override def toString: String = "NO SPEC"
  }
  object OkStatus extends Status {
    override def toString: String = "OK"
  }
  object ErrorStatus extends Status {
    override def toString: String = "ERROR"
  }
  object SkippedStatus extends Status {
    override def toString: String = "SKIP"
  }

  private case class DLine(
      line: Int,
      text: String,
      status: Option[Status],
      error: Option[TypeError],
  ) {
    def format(): String = {
      val lineNum = line.toString.reverse.padTo(3, ' ').reverse
      val lineText =
        if (text.length <= width) text.padTo(width, ' ') ++ " |"
        else text.take(width) ++ "……"
      val diagText = status.map(_.toString).getOrElse("").colTrim(7)
      val errorText = error.map(_.msg).getOrElse("").colTrim(120, lastColumn = true)
      s"$lineNum $lineText $diagText | $errorText"
    }
  }

  def checkFile(beamFile: String): List[String] =
    checkFileD(beamFile).map(_.format())

  private def checkFileD(beamFile: String): List[DLine] = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val forms = Pipeline.checkForms(beamFile)

    val erlFile = forms.collect({ case File(erlFile, _) => erlFile }).head
    val erlPathFromForms = Paths.get(erlFile)
    val erlPath =
      if (erlPathFromForms.isAbsolute) erlPathFromForms
      else Paths.get(beamFile.replace("/ebin/", "/src/").replace(".beam", ".erl"))
    val lines = Files.readAllLines(erlPath).asScala.toList.map(_.replace('\t', ' '))

    val statusDs = statusDiags(forms)
    val errorDs = errorDiags(forms)

    lines.zipWithIndex.map { case (text, i) =>
      val l = i + 1
      DLine(l, text, statusDs.get(l), errorDs.get(l))
    }
  }

  def checkFun(beamFile: String, id: Id): List[String] =
    checkFunD(beamFile, id).map(_.format())

  private def checkFunD(beamFile: String, id: Id): List[DLine] = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val (forms, start, end) = Pipeline.checkFun(beamFile, id)

    val erlFile = forms.collect({ case File(erlFile, _) => erlFile }).head
    val erlPathFromForms = Paths.get(erlFile)
    val erlPath =
      if (erlPathFromForms.isAbsolute) erlPathFromForms
      else Paths.get(beamFile.replace("/ebin/", "/src/").replace(".beam", ".erl"))
    val lines = Files.readAllLines(erlPath).asScala.toList.map(_.replace('\t', ' '))

    val trueEnd = if (end == 0) lines.length else (end - 1)

    val statusDs = statusDiags(forms)
    val errorDs = errorDiags(forms)

    val origLines = lines.zipWithIndex.map { case (text, i) =>
      val l = i + 1
      DLine(l, text, statusDs.get(l), errorDs.get(l))
    }
    origLines.slice(start - 1, trueEnd)
  }

  private def statusDiags(forms: List[Form]): Map[Int, Status] = {
    var diags = Map.empty[Int, Status]
    for (form <- forms) form match {
      case _: TypedFuncDecl    => diags += form.line -> OkStatus
      case _: NoSpecFuncDecl   => diags += form.line -> NoSpecStatus
      case _: MistypedFuncDecl => diags += form.line -> ErrorStatus
      case _: SkippedFunDecl   => diags += form.line -> SkippedStatus
      case _                   =>
    }
    diags
  }

  private def errorDiags(forms: List[Form]): Map[Int, TypeError] = {
    var diags = Map.empty[Int, TypeError]
    for (form <- forms) form match {
      case m: MistypedFuncDecl => diags += m.te.line -> m.te
      case _                   =>
    }
    diags
  }
}
