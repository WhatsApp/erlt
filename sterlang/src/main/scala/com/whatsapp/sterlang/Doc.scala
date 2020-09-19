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

object Doc {
  case class Pos(line: Int, column: Int) {
    def <(that: Pos): Boolean = this.line < that.line || this.line == that.line && this.column < that.column
  }

  val ZRange: Range =
    Range(start = Pos(0, 0), end = Pos(0, 0))
  case class Range(start: Pos, end: Pos) {
    def !(other: Range): Range = mergeNonZ(this, other)
  }

  case class Ranger(source: String, start: Pos, end: Pos) {
    import scala.collection.mutable.ArrayBuffer
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
      source.substring(
        index(start.line - 1),
        index(start.line - 1) + start.column - 1,
      )
    def text: String =
      source.substring(
        index(start.line - 1) + start.column - 1,
        index(end.line - 1) + end.column - 1,
      )
    def suffix: String =
      source.substring(
        index(end.line - 1) + end.column - 1,
        index(end.line),
      )
  }

  case class Locator(source: String, pos: Pos) {
    import scala.collection.mutable.ArrayBuffer
    private lazy val index: Array[Int] = {
      var lineStarts = new ArrayBuffer[Int]
      lineStarts += 0
      for (i <- 0 until source.length)
        if (source.charAt(i) == '\n') lineStarts += (i + 1)
      lineStarts += source.length
      lineStarts.toArray
    }
    def lineContents: String = {
      val lineStart = index(pos.line - 1)
      val lineEnd = index(pos.line)
      val endIndex =
        if (lineStart < lineEnd - 1 && source.charAt(lineEnd - 2) == '\r' && source.charAt(lineEnd - 1) == '\n') {
          lineEnd - 2
        } else if (lineStart < lineEnd && (source.charAt(lineEnd - 1) == '\r' || source.charAt(lineEnd - 1) == '\n')) {
          lineEnd - 1
        } else {
          lineEnd
        }
      source.subSequence(lineStart, endIndex).toString
    }
    def longString: String =
      lineContents + "\n" + lineContents.take(pos.column - 1).map { x => if (x == '\t') x else ' ' } + "^"
  }

  /** Combines two ranges to create a range that spans both. */
  def merge(pos1: Range, pos2: Range): Range =
    (pos1, pos2) match {
      case (`ZRange`, _)              => pos2
      case (_, `ZRange`)              => pos1
      case (pos1: Range, pos2: Range) => mergeNonZ(pos1, pos2)
    }

  /** Combines two ranges to create a range that spans both. */
  private def mergeNonZ(pos1: Range, pos2: Range): Range = {
    val start = if (pos1.start < pos2.start) pos1.start else pos2.start
    val end = if (pos1.end < pos2.end) pos2.end else pos1.end
    Range(start, end)
  }
}
