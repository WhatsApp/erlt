package com.whatsapp.eqwalizer.ast

import com.ericsson.otp.erlang._
import com.whatsapp.eqwalizer.ast.WIPDiagnostics.{ExpansionFailure, SkippedConstructDiagnostics}
import com.whatsapp.eqwalizer.ast.Exprs.{Clause, Expr}
import com.whatsapp.eqwalizer.ast.Types.{ConstrainedFunType, Type}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.TypeError

object Forms {
  sealed trait Form { val line: Int }
  case class Module(name: String)(val line: Int) extends Form
  case class Export(funs: List[Id])(val line: Int) extends Form
  case class Import(module: String, funs: List[Id])(val line: Int) extends Form
  case class ExportType(types: List[Id])(val line: Int) extends Form
  case class TypeDecl(id: Id, params: List[String], body: Type)(val line: Int) extends Form
  case class RecDecl(name: String, fields: List[RecField])(val line: Int) extends Form
  case class FunSpec(id: Id, types: List[ConstrainedFunType])(val line: Int) extends Form
  case class FunDecl(id: Id, clauses: List[Clause])(val line: Int) extends Form
  case class File(file: String, start: Int)(val line: Int) extends Form

  case class RecField(name: String, tp: Type, defaultValue: Option[Expr])(val line: Int)

  sealed trait SkippedForm extends Form { val diag: SkippedConstructDiagnostics }
  case class SkippedTypeDecl(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedFunSpec(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedFunDecl(id: Id, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm
  case class SkippedRecordDecl(name: String, diag: SkippedConstructDiagnostics)(val line: Int) extends SkippedForm

  sealed trait FailedExpandForm extends Form
  case class FailedExpandTypeDecl(id: Id, diag: ExpansionFailure)(val line: Int) extends FailedExpandForm
  case class FailedExpandFunSpec(id: Id, diag: ExpansionFailure)(val line: Int) extends FailedExpandForm

  case class NoSpecFuncDecl(id: Id)(val line: Int) extends Form
  case class TypedFuncDecl(id: Id)(val line: Int) extends Form
  case class MistypedFuncDecl(id: Id, te: TypeError)(val line: Int) extends Form

  def load(beamFile: String): List[Form] = {
    import com.whatsapp.eqwalizer.io.Beam
    import com.whatsapp.eqwalizer.io.EData.EList

    val Some(EList(rawForms, None)) = Beam.loadAbstractForms(beamFile)
    rawForms.flatMap(Convert.convertForm)
  }

  def isFunForm(o: OtpErlangObject): Boolean =
    o.asInstanceOf[OtpErlangTuple].elementAt(0).equals(new OtpErlangAtom("function"))
}
