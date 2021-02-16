package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.WIPDiagnostics.{NotRequested, SkippedConstructDiagnostics}
import com.whatsapp.eqwalizer.ast.{DB, Expand, Forms, Globalize, Id}
import com.whatsapp.eqwalizer.tc.Check
import com.whatsapp.eqwalizer.tc.TcDiagnostics.TypeError

object Pipeline {
  def loadForms(beamFile: String): List[Form] =
    expandForms(globalizeForms(Forms.load(beamFile)))

  private def globalizeForms(forms: List[Form]): List[Form] = {
    val m = forms.collect { case Module(m) => m }.head
    forms.map {
      case s: FunSpec  => Globalize.globalizeSpec(m, s)
      case t: TypeDecl => Globalize.globalizeTypeDecl(m, t)
      case r: RecDecl  => Globalize.globalizeRecDecl(m, r)
      case x           => x
    }
  }

  private def expandForms(forms: List[Form]): List[Form] =
    forms.map {
      case s: FunSpec  => Expand.expandFunSpec(s)
      case t: TypeDecl => Expand.expandTypeDecl(t)
      case r: RecDecl  => Expand.expandRecDecl(r)
      case x           => x
    }

  def checkForms(beamFile: String): List[Form] = {
    val forms = loadForms(beamFile)
    val module = forms.collect { case Module(m) => m }.head
    val stub = DB.getExpandedModuleStub(module).get
    forms.map {
      case f: FunDecl =>
        stub.specs.get(f.id).map(checkFun(module, f, _)).getOrElse(NoSpecFuncDecl(f.id)(f.line))
      case x =>
        x
    }
  }

  def checkFun(beamFile: String, id: Id): (List[Form], Int, Int) = {
    val forms = loadForms(beamFile)
    val module = forms.collect { case Module(m) => m }.head
    val stub = DB.getExpandedModuleStub(module).get
    var start, end = 0
    val checkedForms = forms.map {
      case f: FunDecl =>
        if (f.id == id) stub.specs.get(f.id).map(checkFun(module, f, _)).getOrElse(NoSpecFuncDecl(f.id)(f.line))
        else SkippedFunDecl(id: Id, SkippedConstructDiagnostics(f.line, NotRequested))(f.line)
      case fSpec: FunSpec if fSpec.id == id =>
        start = fSpec.line
        fSpec
      case x =>
        if (start > 0 && end == 0) {
          end = x.line
        }
        x
    }
    (checkedForms, start, end)
  }

  private def checkFun(module: String, f: FunDecl, spec: FunSpec): Form = {
    try {
      new Check(module).checkFun(f, spec)
      TypedFuncDecl(f.id)(f.line)
    } catch {
      case te: TypeError =>
        MistypedFuncDecl(f.id, te)(f.line)
    }
  }
}
