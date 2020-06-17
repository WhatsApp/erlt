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

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.{OffsetPosition, Position}

object Pos {

  // Position
  sealed trait P

  // No position
  case object NP extends P

  // Some position
  case class SP(start: Position, end: Position) extends P {

    private lazy val source: java.lang.CharSequence =
      start.asInstanceOf[OffsetPosition].source

    // An index that contains all line starts, including the first line, and eof
    private lazy val index: Array[Int] = {
      var lineStarts = new ArrayBuffer[Int]
      lineStarts += 0
      for (i <- 0 until source.length)
        if (source.charAt(i) == '\n') lineStarts += (i + 1)
      lineStarts += source.length
      lineStarts.toArray
    }

    def prefix: String =
      source
        .subSequence(
          index(start.line - 1),
          index(start.line - 1) + start.column - 1,
        )
        .toString

    def text: String =
      source
        .subSequence(
          index(start.line - 1) + start.column - 1,
          index(end.line - 1) + end.column - 1,
        )
        .toString

    def suffix: String =
      source
        .subSequence(
          index(end.line - 1) + end.column - 1,
          index(end.line),
        )
        .toString
  }

  trait Pos {
    val p: Pos
  }

}
