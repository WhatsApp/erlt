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

package com.whatsapp.corq

import com.whatsapp.corq.ast.Forms._
import com.whatsapp.corq.ast.{DB, Expand, Forms, Globalize, Id}
import com.whatsapp.corq.tc.Check
import com.whatsapp.corq.tc.TcDiagnostics.TypeError
import erlang.{CErl, Data}
import erlang.CErl.{CFun, CLiteral, CModule, CVar, VarNameAtomInt}
import erlang.Data.EAtom

object Pipeline {
  def loadForms(beamFile: String): List[Form] =
    expandForms(globalizeForms(Forms.load(beamFile)))

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
    // TODO: don't hard-code
    val fullPath = "/Users/mheiber/erlt/corq/" + beamFile
    val CModule(_, CLiteral(_, EAtom(module)), _, _, defs) =
      DB.loadCoreModule(fullPath)
    val specs = DB.getExpandedModuleStub(module).get.specs
    defs.map {
      case (CVar(_, VarNameAtomInt(name, arity)), expr: CFun) => {
        val id = Id(name, arity)
        // TODO: line
        specs
          .get(id)
          .map(checkFun(module, id, expr, _, 0))
          .getOrElse(NoSpecFuncDecl(id)(0))
      }
    }
  }

  private def checkFun(
      module: String,
      id: Id,
      f: CFun,
      spec: FunSpec,
      line: Int
  ): Form = {
    try {
      Check(module).checkFun(id, f, spec)
      TypedFuncDecl(id)(line)
    } catch {
      case te: TypeError =>
        MistypedFuncDecl(id, te)(line)
    }
  }
}
