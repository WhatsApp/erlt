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

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Show.show
import com.whatsapp.eqwalizer.ast.Types.Type

object TcDiagnostics {
  case class TcDiagnostics(line: Int, msg: String)

  sealed trait TypeError extends Exception {
    val line: Int
    val msg: String
  }
  case class TypeMismatch(line: Int, expr: Expr, expected: Type, got: Type) extends TypeError {
    override val msg: String = s"${show(expr)}. Expected: ${show(expected)}, Got: ${show(got)}"
  }
  case class UndefinedField(line: Int, recName: String, fieldName: String) extends TypeError {
    override val msg: String = s"#$recName{...}: $fieldName is 'undefined'"
  }
  case class UnboundVar(line: Int, n: String) extends TypeError {
    override val msg: String = s"Unbound var: ${n}"
  }
}
