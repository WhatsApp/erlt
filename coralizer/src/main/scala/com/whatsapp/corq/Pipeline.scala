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

package com.whatsapp.coralizer

import com.whatsapp.coralizer.ast.Forms._
import com.whatsapp.coralizer.ast.Types.ConstrainedFunType
import com.whatsapp.coralizer.ast.{
  DB,
  Expand,
  Forms,
  Globalize,
  Id,
  PrettyCErl,
  WIPDiagnostics
}
import com.whatsapp.coralizer.tc.{BuiltIn, Check, Env}
import com.whatsapp.coralizer.tc.TcDiagnostics.TypeError
import erlang.CErl._
import erlang.Data._

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

  def moduleToSourceFile(module: CModule): String =
    module.attrs
      .collect({
        case (
              "file",
              CLiteral(_, EList(ETuple(EString(fileName) :: _) :: _, _))
            ) =>
          fileName
      })
      .head

  def checkForms(module: CModule): (String, List[Form]) = {
    val srcFile = moduleToSourceFile(module)
    val CModule(_, moduleName, _, _, defs) = module

    val specs = DB.getExpandedModuleStub(moduleName).get.specs
    val forms = defs.map {
      case (CVar(_, VarNameAtomInt(id)), expr: CFun) => {
        if (BuiltIn.moduleInfoSpecs.contains(id)) {
          BuiltInFuncDecl(id)
        } else {
          specs
            .get(id)
            .map(checkFun(moduleName, id, expr, _, expr.line))
            .getOrElse(NoSpecFuncDecl(id)(expr.line))
        }
      }
      // $COVERAGE-OFF$
      case d => sys.error(s"unexpected def $d")
      // $COVERAGE-ON$
    }.toList
    (srcFile, forms)
  }

  private def checkFun(
      module: String,
      id: Id,
      f: CFun,
      spec: FunSpec,
      line: Int
  ): Form = {
    try {
      // no need to check these every time
      if (!BuiltIn.moduleInfoSpecs.contains(id)) {
        // not handling constraints
        Check(module).checkSpeccedFun(f, spec, Map.empty)
      }
      TypedFuncDecl(id)(line)
    } catch {
      case te: TypeError =>
        MistypedFuncDecl(id, te)(line)
      case diag @ WIPDiagnostics.SkippedConstructDiagnostics(
            line,
            _construct
          ) =>
        SkippedFunDecl(id, diag)(line)
    }
  }
}
