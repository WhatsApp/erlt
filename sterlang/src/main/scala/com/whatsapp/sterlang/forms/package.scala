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

package com.whatsapp.sterlang

import com.whatsapp.sterlang.Etf._

package object forms {
  def r(term: ETerm): Doc.Range = {
    val ETuple(List(ETuple(List(ELong(line1), ELong(col1))), ETuple(List(ELong(line2), ELong(col2))))) = term
    val pos1 = Doc.Pos(line1.toInt, col1.toInt)
    val pos2 = Doc.Pos(line2.toInt, col2.toInt)
    assert(pos1 != Doc.Pos(0, 0))
    assert(pos2 != Doc.Pos(0, 0))
    // TODO: fix after re-integration
    // assert(pos1 != pos2)
    Doc.Range(pos1, pos2)
  }
}
