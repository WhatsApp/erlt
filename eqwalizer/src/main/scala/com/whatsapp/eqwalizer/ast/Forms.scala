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

package com.whatsapp.eqwalizer.ast

import com.ericsson.otp.erlang._
import com.whatsapp.eqwalizer.ast.Diagnostics.SkippedConstructDiagnostics
import com.whatsapp.eqwalizer.ast.Exprs.Clause
import com.whatsapp.eqwalizer.ast.Types.{ConstrainedFunType, Type}
import com.whatsapp.eqwalizer.io.EData

object Forms {
  sealed trait Form { val line: Int }
  case class Module(name: String)(val line: Int) extends Form
  case class Export(funs: List[Id])(val line: Int) extends Form
  case class Import(module: String, funs: List[Id])(val line: Int) extends Form
  case class ExportType(types: List[Id])(val line: Int) extends Form
  case class TypeDecl(id: Id, params: List[String], body: Type)(val line: Int) extends Form
  case class FunSpec(id: Id, types: List[ConstrainedFunType])(val line: Int) extends Form
  case class FunDecl(id: Id, clauses: List[Clause])(val line: Int) extends Form
  case class File(file: String, start: Int)(val line: Int) extends Form

  sealed trait SkippedForm extends Form
  case class SkippedTypeDecl(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedFunSpec(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedFunDecl(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedRecordDecl(name: String)(val line: Int) extends SkippedForm

  case class IgnoredForm()(val line: Int) extends Form

  def load(beamFile: String): List[Form] = {
    import com.whatsapp.eqwalizer.io.Beam
    import com.whatsapp.eqwalizer.io.EData.EList

    val Some(EList(rawForms, None)) = Beam.loadAbstractForms(beamFile)
    rawForms.flatMap(Convert.convertForm)
  }

  def loadDefs(beamFile: String): List[Form] = {
    import com.whatsapp.eqwalizer.io.Beam

    val formsJ = Beam.loadAbstractFormsJ(beamFile)
    val formsDef = (for {
      i <- 0 until formsJ.arity()
      f = formsJ.elementAt(i)
      if !isFunForm(f)
    } yield f).toArray
    formsDef.flatMap(f => Convert.convertForm(EData.fromJava(f))).toList
  }

  def isFunForm(o: OtpErlangObject): Boolean =
    o match {
      case t: OtpErlangTuple =>
        t.elementAt(0) match {
          case a: OtpErlangAtom =>
            a.atomValue() == "function"
          case _ => false
        }
      case _ => false
    }
}
