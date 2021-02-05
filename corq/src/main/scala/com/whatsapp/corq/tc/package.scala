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

import com.whatsapp.corq.ast.Types.Type
import erlang.CErl._
import erlang.Data._

package object tc {
  type Env = Map[VarName, Type]

  object LitAtom {
    def unapply(cerl: CErl): Option[String] =
      cerl match {
        case CLiteral(_, EAtom(str)) => Some(str)
        case _                       => None
      }
  }
  object ErlangCall {
    def unapply(cerl: CErl): Option[(String, List[CErl])] =
      cerl match {
        case CCall(_, LitAtom("erlang"), LitAtom(op), args) => Some(op, args)
        case _                                              => None
      }
  }
}
