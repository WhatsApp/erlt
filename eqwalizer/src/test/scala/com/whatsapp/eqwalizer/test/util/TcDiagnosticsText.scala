/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.whatsapp.eqwalizer.test.util

import com.whatsapp.eqwalizer.Pipeline
import com.whatsapp.eqwalizer.ast.Forms._
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
      val errorText = error.map(_.msg).getOrElse("").colTrim(80, lastColumn = true)
      s"$lineNum $lineText $diagText | $errorText"
    }
  }

  def checkForms(beamFile: String): List[String] =
    checkFormsD(beamFile).map(_.format())

  private def checkFormsD(beamFile: String): List[DLine] = {
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
