package com.whatsapp.sterlang.patterns

import java.util.NoSuchElementException

/** A basic implementation of matrices where elements are patterns. */
// TODO: maybe replace this with an existing library
private[patterns] object PatternMatrix {

  /** A one dimensional array of patterns. */
  // TODO: just make this an alias...
  case class Vector(elements: List[Pattern.Pat]) {
    def length: Int = elements.length
  }

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
    private val emptyMatrix = Matrix(rows = Nil)

    def apply(): Matrix = emptyMatrix

    def unapply(matrix: Matrix): Boolean =
      matrix.rows match {
        case Nil => true
        case _   => false
      }
  }

  object AddColumn {

    /** Add a column in front of the matrix. */
    def apply(first: Vector, rest: Matrix): Matrix = {
      assert(first.length == rest.rows.length)

      def vectorCons(t: (Pattern.Pat, Vector)): Vector = Vector(t._1 :: t._2.elements)

      def newRows: List[Vector] = first.elements.zip(rest.rows).map(vectorCons)
      Matrix(rows = newRows)
    }

    /** Strip the first column in the matrix. */
    def unapply(matrix: Matrix): Option[(Vector, Matrix)] = {
      try {
        val firstColumn = Vector(matrix.rows.map(_.elements.head))
        val rest = Matrix(matrix.rows.map(row => Vector(row.elements.tail)))
        Some(firstColumn, rest)
      } catch {
        case _: NoSuchElementException =>
          None
      }
    }
  }

  object AddRow {

    /** Add a row before the matrix. */
    def apply(first: Vector, rest: Matrix): Matrix =
      Matrix(first :: rest.rows)

    /** Strip the first row in matrix. */
    def unapply(matrix: Matrix): Option[(Vector, Matrix)] =
      matrix.rows match {
        case Nil         => None
        case row :: rows => Some(row, Matrix(rows))
      }
  }
}
