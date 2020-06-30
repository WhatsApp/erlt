package com.whatsapp.sterlang.patterns

import java.util.NoSuchElementException

/** A basic implementation of matrices where elements are patterns. */
// TODO: maybe replace this with an existing library
private[patterns] object PatternMatrix {

  /** A one dimensional array of patterns. */
  type Vector = List[Pattern.Pat]

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

  object AddRow {

    /** Strip the first row in matrix. */
    def unapply(matrix: Matrix): Option[(Vector, Matrix)] =
      matrix.rows match {
        case Nil         => None
        case row :: rows => Some(row, Matrix(rows))
      }
  }
}
