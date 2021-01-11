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

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.{DB, Expand, Forms, Globalize}
import com.whatsapp.eqwalizer.tc.Check
import com.whatsapp.eqwalizer.tc.TcDiagnostics.TypeError

object Pipeline {
  def loadForms(beamFile: String): List[Form] =
    expandForms(globalizeForms(Forms.load(beamFile)))

  def loadFormsSummary(beamFile: String): List[Form] =
    expandForms(globalizeForms(Forms.loadDefs(beamFile)))

  private def globalizeForms(forms: List[Form]): List[Form] = {
    val m = forms.collect { case Module(m) => m }.head
    forms.map {
      case s: FunSpec  => Globalize.globalizeSpec(m, s)
      case t: TypeDecl => Globalize.globalizeTypeDecl(m, t)
      case x           => x
    }
  }

  private def expandForms(forms: List[Form]): List[Form] =
    forms.map {
      case s: FunSpec  => Expand.expandFunSpec(s)
      case t: TypeDecl => Expand.expandTypeDecl(t)
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

  private def checkFun(module: String, f: FunDecl, spec: FunSpec): Form = {
    try {
      Check(module).checkFun(f, spec)
      TypedFuncDecl(f.id)(f.line)
    } catch {
      case te: TypeError =>
        MistypedFuncDecl(f.id, te)(f.line)
    }
  }

}
