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

import com.whatsapp.sterlang.Absyn.ValDef
import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang.{Absyn, Context, Pos, Values}

/** Generates warnings for missing and redundant clauses in pattern matching.
  *
  * Based on [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]].
  * */
class PatternChecker(private val context: Context) {
  private val A = Absyn

  def check(functions: List[A.Fun]): Unit = {
    functions.foreach(checkNode)
  }

  def checkNode(node: A.Node): Unit = {
    // Check this node
    node match {
      case node: A.Fun =>
        checkClauses(node, node.clauses.map(_.pats))
      case node: A.FnExp =>
        checkClauses(node, node.clauses.map(_.pats))
      case node: A.NamedFnExp =>
        checkClauses(node, node.clauses.map(_.pats))
      case node: A.CaseExp =>
        checkClauses(node, node.branches.map(b => List(b.pat)))
      case node: ValDef =>
        checkClauses(node, List(List(node.pat)))
      case _ => // nothing to check
    }

    // Recursively check all children nodes
    if (!node.isInstanceOf[A.Pat]) { // No need to recurse into patterns
      A.children(node).foreach(checkNode)
    }
  }

  /** Check clauses for exhaustiveness and redundancy.
    *
    * @throws MissingPatternsWarning if the clauses are inexhaustive
    * @throws UselessPatternWarning if there is a clause that can never match
    */
  private def checkClauses(node: HasSourceLocation, clauses: List[List[A.Pat]]): Unit = {
    require(clauses.nonEmpty)

    var previousRows: List[PatternMatrix.Vector] = List()

    // Check for redundancy
    for (clause <- clauses) {
      // TODO: check for nonlinear patterns

      val simpleClause = clause.map(Pattern.simplify)

      if (!isUseful(PatternMatrix.Matrix(previousRows), simpleClause)) {
        val location =
          if (clause.isEmpty) Pos.NP
          else Pos.merge(clause.head.sourceLocation, clause.last.sourceLocation)
        throw new UselessPatternWarning(location)
      }

      // FIXME: this takes linear time
      previousRows = previousRows.appended(simpleClause)
    }

    /** The pattern row that will match any value. */
    val any = clauses.head.map(_ => Pattern.Wildcard)

    // Check for exhaustiveness
    if (isUseful(PatternMatrix.Matrix(previousRows), any)) {
      throw new MissingPatternsWarning(node.sourceLocation)
    }
  }

  /** Returns true if the clause is useful with respect to the pattern matrix.
    * A clause is useful if there is a value vector matched by the clause but not by the pattern matrix.
    *
    * Corresponds to the U_rec function in the paper.
    */
  private def isUseful(matrix: PatternMatrix.Matrix, clause: PatternMatrix.Vector): Boolean = {
    require(matrix.height == 0 || matrix.width == clause.length)

    (matrix, clause) match {
      case (PatternMatrix.Empty(), _) => true
      case (_, Nil)                   => false
      case (PatternMatrix.AddColumn(col1, _), Pattern.Wildcard :: ps) =>
        val constructors: Set[Pattern.Constructor] = col1.collect {
          case x: Pattern.ConstructorApplication => x.constructor
        }.toSet

        if (isCompleteSignature(constructors)) {
          constructors.exists { constructor =>
            isUseful(specialize(matrix, constructor), specializeRow(clause, constructor).get)
          }
        } else {
          isUseful(defaultMatrix(matrix), ps)
        }
      case (_, (p: Pattern.ConstructorApplication) :: ps) =>
        isUseful(specialize(matrix, p.constructor), p.arguments ++ ps)
    }
  }

  /** Extract clauses in the matrix that are relevant for checking the given pattern.
    *
    * Corresponds to the S function in the paper.
    */
  private def specialize(matrix: PatternMatrix.Matrix, constructor: Pattern.Constructor): PatternMatrix.Matrix = {
    PatternMatrix.Matrix(matrix.rows.flatMap(row => specializeRow(row, constructor)))
  }

  /** Like [[specialize]] but specialized to a single row. */
  private def specializeRow(row: PatternMatrix.Vector, constructor: Pattern.Constructor): Option[PatternMatrix.Vector] =
    row match {
      case Pattern.Wildcard :: rest =>
        Some(List.fill(arity(constructor))(Pattern.Wildcard) ++ rest)
      case Pattern.ConstructorApplication(c1, arguments1) :: rest if c1 == constructor =>
        Some(arguments1 ++ rest)
      case _ =>
        None
    }

  /** Corresponds to the D function in the paper. */
  private def defaultMatrix(matrix: PatternMatrix.Matrix): PatternMatrix.Matrix = {
    require(matrix.width > 0)

    PatternMatrix.Matrix(matrix.rows.collect {
      case Pattern.Wildcard :: tail => tail
    })
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

  /** Returns the number of arguments the given constructor takes. */
  private def arity(constructor: Pattern.Constructor): Int =
    constructor match {
      case _: Pattern.Literal   => 0
      case Pattern.Tuple(arity) => arity
      case Pattern.EmptyList    => 0
      case Pattern.Cons         => 2
      case Pattern.EnumConstructor(enum, constructorName) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        enumDef.cons.find(_.name == constructorName).get.argTypes.length
    }
}
