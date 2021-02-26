package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.WIPDiagnostics.{ExpansionFailure, SkippedConstruct}
import com.whatsapp.eqwalizer.ast.Forms._

import java.nio.file.{Files, Paths}

object WIPDiagnosticsText {
  private val width: Int = 40

  private implicit class TextOps(text: String) {
    def colTrim(width: Int, lastColumn: Boolean = false): String =
      if (text.length <= width) {
        if (lastColumn) text else text.padTo(width, ' ')
      } else {
        text.take(width - 2) ++ "……"
      }
  }

  private case class DLine(
      line: Int,
      text: String,
      formDiag: Option[FormDiag],
      skipDiag: Option[SkippedConstruct],
      expandDiag: Option[ExpansionFailure],
  ) {
    def format(): String = {
      val lineNum = line.toString.reverse.padTo(3, ' ').reverse
      val lineText =
        if (text.length <= width) text.padTo(width, ' ') ++ " |"
        else text.take(width) ++ "……"
      val diagText = formDiag.map(_.toString).getOrElse("").colTrim(7)
      val skipText = skipDiag.map(_.toString).getOrElse("").colTrim(30)
      val expandText = expandDiag.map(_.diag).getOrElse("").colTrim(30, lastColumn = true)
      s"$lineNum $lineText $diagText | $skipText | $expandText"
    }
  }

  sealed trait FormDiag
  object LoadedF extends FormDiag {
    override def toString: String = "LOADED"
  }
  object IgnoredF extends FormDiag {
    override def toString: String = "IGNORED"
  }
  object SkippedF extends FormDiag {
    override def toString: String = "SKIPPED"
  }

  def loadForms(beamFile: String): List[String] =
    loadFormsD(beamFile).map(_.format())

  private def loadFormsD(beamFile: String): List[DLine] = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val forms = Pipeline.loadForms(beamFile)

    val erlFile = forms.collect({ case File(erlFile, _) => erlFile }).head
    val erlPathFromForms = Paths.get(erlFile)
    val erlPath =
      if (erlPathFromForms.isAbsolute) erlPathFromForms
      else Paths.get(beamFile.replace("/ebin/", "/src/").replace(".beam", ".erl"))
    val lines = Files.readAllLines(erlPath).asScala.toList.map(_.replace('\t', ' '))

    val formDs = formDiags(forms)
    val skipDs = skipDiags(forms)
    val expandDs = expandDiags(forms)

    lines.zipWithIndex.map { case (text, i) =>
      val l = i + 1
      DLine(l, text, formDs.get(l), skipDs.get(l), expandDs.get(l))
    }
  }

  // Top-level status of TypeDecl/Spec
  private def formDiags(forms: List[Form]): Map[Int, FormDiag] = {
    var diags: Map[Int, FormDiag] = Map.empty
    for (form <- forms) form match {
      case _: SkippedForm =>
        diags += form.line -> SkippedF
      // ignoring
      case File(_, _) =>
      case _ =>
        diags += form.line -> LoadedF
    }
    diags
  }

  // More detailed status -> pointing to the root issues inside the form
  private def skipDiags(forms: List[Form]): Map[Int, SkippedConstruct] = {
    var diags: Map[Int, SkippedConstruct] = Map.empty
    for (form <- forms) form match {
      case sf: SkippedForm =>
        val reason = sf.diag
        diags += reason.line -> reason.construct
      // nothing
      case _ =>
    }
    diags
  }

  // Status of "form expansion" - normalisation of all the types inside the current form
  private def expandDiags(forms: List[Form]): Map[Int, ExpansionFailure] = {
    var diags: Map[Int, ExpansionFailure] = Map.empty
    for (form <- forms) form match {
      case FailedExpandFunSpec(_, reason)  => diags += form.line -> reason
      case FailedExpandTypeDecl(_, reason) => diags += form.line -> reason
      case FailedExpandRecDecl(_, reason)  => diags += form.line -> reason
      // nothing
      case _ =>
    }
    diags
  }
}