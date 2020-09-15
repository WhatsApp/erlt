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

package com.whatsapp.sterlang.patterns

/** A basic implementation of matrices where elements are patterns. */
private[patterns] object PatternMatrix {

  /** A one dimensional array of patterns. */
  type Vector = List[Pattern.Pat]

  /** Returns a pattern vector of the given length containing wildcards. */
  def wildcards(length: Int): Vector = List.fill(length)(Pattern.Wildcard)

  /** A two dimensional grid of patterns. */
  case class Matrix(rows: List[Vector]) {
    require(rows.forall(row => row.length == rows.head.length))

    /** The number of rows in the matrix. */
    def height: Int = rows.length

    /** The number of columns in the matrix. */
    def width: Int =
      rows match {
        case Nil    => 0
        case h :: _ => h.length
      }
  }

  object Empty {
    def unapply(matrix: Matrix): Boolean =
      matrix.rows match {
        case Nil => true
        case _   => false
      }
  }

  object AddColumn {

    /** Strip the first column in the matrix. */
    def unapply(matrix: Matrix): Option[(Vector, Matrix)] = {
      if (matrix.height == 0 || matrix.width == 0) {
        None
      } else {
        val firstColumn = matrix.rows.map(_.head)
        val rest = Matrix(matrix.rows.map(_.tail))
        Some((firstColumn, rest))
      }
    }
  }
}
