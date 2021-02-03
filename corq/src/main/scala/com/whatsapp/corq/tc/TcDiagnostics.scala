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

package com.whatsapp.corq.tc

import com.whatsapp.corq.ast.Exprs.Expr
import com.whatsapp.corq.ast.{Id, RemoteId}
import com.whatsapp.corq.ast.Show.show
import com.whatsapp.corq.ast.Types.Type
import erlang.CErl._

object TcDiagnostics {
  case class TcDiagnostics(line: Int, msg: String)

  sealed trait TypeError extends Exception {
    val line: Int
    val msg: String
  }
  case class TypeMismatch(expr: CErl, expected: Type, got: Type)
      extends TypeError {
    val line = expr.line
    override val msg: String =
      s"${show(expr)}. Expected: ${show(expected)}, Got: ${show(got)}"
  }
  case class UnboundVar(line: Int, n: CVar) extends TypeError {
    override val msg: String = s"Unbound var: ${n.name}"
  }
  case class UnboundId(line: Int, id: Id) extends TypeError {
    override val msg: String = s"Unbound var: $id"
  }
  case class UnboundRemoteId(line: Int, id: RemoteId) extends TypeError {
    override val msg: String = s"Unbound var: $id"
  }
}
