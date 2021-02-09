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

package com.whatsapp.coralizer.tc

import com.whatsapp.coralizer.ast.Exprs.Expr
import com.whatsapp.coralizer.ast.{Id, RemoteId}
import com.whatsapp.coralizer.ast.Show.show
import com.whatsapp.coralizer.ast.Types.Type
import erlang.CErl._

object TcDiagnostics {
  sealed trait TypeError extends Exception {
    val expr: CErl
    val line: Int
    val msg: String
  }
  case class TypeMismatch(line: Int, expr: CErl, expected: Type, got: Type)
      extends TypeError {
    override val msg: String =
      s"${show(expr)}. Expected: ${show(expected)}, Got: ${show(got)}"
  }
  case class UnboundVar(line: Int, expr: CVar) extends TypeError {
    override val msg: String = s"Unbound var: ${expr.name}"
  }
  case class UnboundId(line: Int, expr: CErl, id: Id) extends TypeError {
    override val msg: String = s"Unbound var: $id"
  }
  case class UnboundRemoteId(line: Int, expr: CErl, id: RemoteId)
      extends TypeError {
    override val msg: String = s"Unbound var: $id"
  }
}
