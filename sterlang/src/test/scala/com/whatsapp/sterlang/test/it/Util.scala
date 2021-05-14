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

package com.whatsapp.sterlang.test.it

import com.whatsapp.sterlang.Doc.Pos
import com.whatsapp.sterlang.{Doc, Error, PosError, RangeError, Severity}

object Util {
  def rangeErrorString(inputPath: String, inputContent: String, error: RangeError): String = {
    val RangeError(range, t, description) = error
    val ranger = Ranger(inputContent, range.start, range.end)
    val msgTitle = title(error.severity, inputPath, range.start)
    val descText = description.map(_ ++ "\n").getOrElse("")
    msgTitle ++ "\n" ++ t ++ "\n" ++ descText ++ ranger.decorated ++ "\n"
  }

  def posErrorString(inputPath: String, inputContent: String, error: PosError): String = {
    val PosError(pos, t) = error
    val locator = Locator(inputContent, pos)
    val msgTitle = title(Error, inputPath, pos)
    s"$msgTitle\n$t\n${locator.longString}\n"
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
    lazy val prefix: String =
      source.substring(
        index(start.line - 1),
        index(start.line - 1) + start.column - 1,
      )
    lazy val text: String =
      source.substring(
        index(start.line - 1) + start.column - 1,
        index(end.line - 1) + end.column - 1,
      )
    lazy val suffix: String =
      source.substring(
        index(end.line - 1) + end.column - 1,
        index(end.line),
      )

    def decorated: String = {
      val underline = if (text.length > 2) '~' else '^'
      def space(c: Char): Char = ' '
      def under(c: Char): Char = underline
      val t1 :: tail = text.split('\n').toList

      // dropping the last \n
      val suffix1 = suffix.dropRight(1)

      if (tail.isEmpty) {
        List(
          line(start) ++ "| " ++ prefix ++ t1 ++ suffix1,
          "...| " ++ prefix.map(space) ++ t1.map(under),
        ).mkString("\n")
      } else {
        val t2 = tail.last
        val between = tail.dropRight(1)

        List(
          List(
            line(start) ++ "| " ++ prefix ++ t1,
            "...| " ++ prefix.map(space) ++ t1.map(under),
          ),
          between.flatMap(l => List("...| " ++ l, "...| " ++ l.map(under))),
          List(
            line(end) ++ "| " ++ t2 + suffix1,
            "...| " ++ t2.map(under),
          ),
        ).flatten.mkString("\n")
      }
    }
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
    private def lineContents: String = {
      val lineStart = index(pos.line - 1)
      val lineEnd = index(pos.line)
      val endIndex = lineEnd - 1
      source.subSequence(lineStart, endIndex).toString
    }
    def longString: String = {
      val line1 = lineContents
      val line2 = lineContents.take(pos.column - 1).map { x => if (x == '\t') x else ' ' } + "^"
      List(
        line(pos) ++ "| " ++ line1,
        "...| " ++ line2,
      ).mkString("\n")
    }
  }

  private def line(pos: Pos): String =
    pos.line.toString.reverse.padTo(3, ' ').reverse

  private def title(severity: Severity, pathFile: String, pos: Pos): String = {
    val titleSuffix = s" $pathFile:${pos.line}:${pos.column}"
    val sevString = severity.toString.map(_.toUpper)
    val titlePrexif = s"-- $sevString "
    val rest = 80 - titlePrexif.length - titleSuffix.length
    val titleMiddle = "-" * rest
    s"$titlePrexif$titleMiddle$titleSuffix"
  }
}
