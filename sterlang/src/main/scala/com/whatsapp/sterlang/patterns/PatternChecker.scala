package com.whatsapp.sterlang.patterns

import com.whatsapp.sterlang.errors.MissingPatternsWarning
import com.whatsapp.sterlang.errors.UselessPatternWarning
import com.whatsapp.sterlang.{Absyn, Context, Values}

import scala.collection.mutable

/** Generates warnings for missing and redundant clauses in pattern matching.
  *
  * Based on [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]].
  * */
class PatternChecker(private val context: Context) {
  private val A = Absyn

  // TODO: what should be the interface to this module
  def check(functions: List[A.Fun]): Unit = {
    functions.foreach(checkFunction)
  }

  def checkFunction(function: A.Fun): Unit = {
    checkClauses(function.clauses)
    // TODO: check expressions in the body
  }

  /** Check clauses for exhaustiveness and redundancy.
    *
    * @throws MissingPatternsWarning if the clauses are inexhaustive
    * @throws UselessPatternWarning if there is a clause that can never match
    */
  private def checkClauses(clauses: List[A.Clause]): Unit = {
    require(clauses.nonEmpty)

    /** The pattern row that will match any value. */
    val any = PatternMatrix.Vector(
      clauses.head.pats.map(p => Pattern.Wildcard()(typ = p.typ, sourceLocation = p.sourceLocation))
    )

    val previousRows: mutable.ListBuffer[PatternMatrix.Vector] = mutable.ListBuffer()

    // Check for redundancy
    for (clause <- clauses) {
      val simple = simplifyClause(clause.pats)
      // FIXME: constructing the matrix here takes linear time
      if (!isUseful(PatternMatrix.Matrix(previousRows.toList), simple)) {
        throw new UselessPatternWarning()
      }
      previousRows += simple
    }

    // Check for exhaustiveness
    if (isUseful(PatternMatrix.Matrix(previousRows.toList), any)) {
      throw new MissingPatternsWarning()
    }
  }

  /** Converts a surface syntax clause to our simpler representation. */
  private def simplifyClause(clause: List[A.Pat]): PatternMatrix.Vector = {
    // Using the same variable multiple times in a clause introduces equality constraints.
    // We currently do not (cannot) reason about equality constraints.
    // This is used to detect such patterns.
    val seenVariables: mutable.Set[String] = mutable.Set()

    def simplifyPattern(pattern: A.Pat): Pattern.Pat =
      pattern match {
        case A.WildPat() =>
          Pattern.Wildcard()(typ = pattern.typ, sourceLocation = pattern.sourceLocation)

        case A.VarPat(name) => {
          assert(!seenVariables.contains(name))
          seenVariables.add(name)

          // Variable names are irrelevant for our purposes
          Pattern.Wildcard()(typ = pattern.typ, sourceLocation = pattern.sourceLocation)
        }

        case A.AndPat(p1, p2) =>
          (simplifyPattern(p1), simplifyPattern(p2)) match {
            case (_: A.WildPat, p2Simple) => p2Simple
            case (p1Simple, _: A.WildPat) => p1Simple
            case _                        =>
              // For now, we only reason about "and" patterns that are used to name the overall value.
              // For example, `{5, Y} = Z` is OK, whereas `{X, "string"} = {5, Y}` isn't.
              ???
          }

        case A.LiteralPat(value) =>
          Pattern.ConstructorApplication(Pattern.Literal(value), Nil)(
            typ = value.typ,
            sourceLocation = pattern.sourceLocation,
          )

        case A.TuplePat(elements) =>
          Pattern.ConstructorApplication(Pattern.Tuple(elements.length), elements.map(simplifyPattern))(
            typ = pattern.typ,
            sourceLocation = pattern.sourceLocation,
          )

        case A.RecordPat(fields, open) => ???

        case A.ListPat(Nil) =>
          Pattern.ConstructorApplication(Pattern.EmptyList, Nil)(
            typ = pattern.typ,
            sourceLocation = pattern.sourceLocation,
          )

        case A.ListPat(head :: tail) => ???

        case A.ConsPat(head, tail) =>
          Pattern.ConstructorApplication(Pattern.Cons, List(simplifyPattern(head), simplifyPattern(tail)))(
            typ = pattern.typ,
            sourceLocation = pattern.sourceLocation,
          )

        case A.EnumConstructorPat(enum, constructor, arguments) =>
          Pattern.ConstructorApplication(Pattern.EnumConstructor(enum, constructor), arguments.map(simplifyPattern))(
            typ = pattern.typ,
            sourceLocation = pattern.sourceLocation,
          )
      }

    PatternMatrix.Vector(clause.map(simplifyPattern))
  }

  /** Returns true if the clause is useful with respect to the pattern matrix.
    * A clause is useful if there is a value vector matched by the clause but not by the pattern matrix.
    *
    * Corresponds to the U_rec function in the paper.
    */
  private def isUseful(matrix: PatternMatrix.Matrix, clause: PatternMatrix.Vector): Boolean = {
    require(matrix.height == 0 || matrix.width == clause.length)

    (matrix, clause.elements) match {
      case (PatternMatrix.Empty(), _) => true
      case (_, Nil)                   => false
      case (PatternMatrix.AddColumn(col1, _), (_: Pattern.Wildcard) :: ps) => {
        val constructors: Map[Pattern.Constructor, Pattern.ConstructorApplication] =
          col1.elements.collect { case x: Pattern.ConstructorApplication => x.constructor -> x }.toMap

        if (isCompleteSignature(constructors.keySet)) {
          constructors.exists {
            case (_, constructor) => isUseful(specialize(matrix, constructor), specializeRow(clause, constructor).get)
          }
        } else {
          isUseful(defaultMatrix(matrix), PatternMatrix.Vector(ps))
        }
      }
      case (_, (p: Pattern.ConstructorApplication) :: ps) =>
        isUseful(specialize(matrix, p), PatternMatrix.Vector(p.arguments ++ ps))
    }
  }

  /** Extract clauses in the matrix that are relevant for checking the given pattern.
    *
    * Corresponds to the S function in the paper.
    * */
  private def specialize(
      matrix: PatternMatrix.Matrix,
      pattern: Pattern.ConstructorApplication,
  ): PatternMatrix.Matrix = {
    PatternMatrix.Matrix(matrix.rows.flatMap(row => specializeRow(row, pattern)))
  }

  /** Like [[specialize]] but specialized to a single row. */
  private def specializeRow(
      row: PatternMatrix.Vector,
      pattern: Pattern.ConstructorApplication,
  ): Option[PatternMatrix.Vector] =
    row.elements match {
      case (first @ Pattern.Wildcard()) :: rest => {
        val newPatterns =
          pattern.arguments.map(arg => Pattern.Wildcard()(typ = arg.typ, sourceLocation = first.sourceLocation))
        Some(PatternMatrix.Vector(newPatterns ++ rest))
      }
      case Pattern.ConstructorApplication(c1, arguments1) :: rest if c1 == pattern.constructor =>
        Some(PatternMatrix.Vector(arguments1 ++ rest))
      case _ =>
        None
    }

  /** Corresponds to the D function in the paper. */
  private def defaultMatrix(matrix: PatternMatrix.Matrix): PatternMatrix.Matrix = {
    val newRows = matrix.rows.flatMap(row =>
      row.elements match {
        case (_: Pattern.Wildcard) :: tail            => Some(PatternMatrix.Vector(tail))
        case (_: Pattern.ConstructorApplication) :: _ => None
        case Nil                                      => throw new IllegalArgumentException()
      }
    )
    PatternMatrix.Matrix(newRows)
  }

  /** Returns true if the given set of constructors is all the constructors of the (inferred) data type. */
  private def isCompleteSignature(constructors: Set[Pattern.Constructor]): Boolean = {

    /** Total number of constructors the type of the constructor has. */
    // TODO: this function should live somewhere else...
    def numberOfConstructors(constructor: Pattern.Constructor) =
      constructor match {
        case Pattern.Literal(Values.UnitValue)       => 1
        case Pattern.Literal(_: Values.BooleanValue) => 2
        case Pattern.Literal(_)                      => Int.MaxValue // TODO: this catch-all case is janky
        case Pattern.Tuple(_)                        => 1
        case Pattern.EmptyList | Pattern.Cons        => 2
        case Pattern.EnumConstructor(enum, _)        => context.enumDefs.find(_.name == enum).get.cons.length
      }

    if (constructors.isEmpty)
      false
    else
      numberOfConstructors(constructors.head) == constructors.size
  }
}
