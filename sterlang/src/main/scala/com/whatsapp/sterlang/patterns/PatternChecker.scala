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

import java.util.NoSuchElementException

import com.whatsapp.sterlang.Absyn.ValDef
import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang._

import scala.collection.mutable.ListBuffer

/** Generates warnings for missing and redundant clauses in pattern matching.
  *
  * Based on [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]].
  */
class PatternChecker(private val vars: Vars, private val context: Context, val program: Ast.Program) {
  private val A = Absyn

  /** Like [[A.Clause]] but without the body. */
  private case class ClauseHead(patterns: List[A.Pat], guards: List[A.Guard])

  /** Returns a list of all pattern matching related issues in the given functions and their subexpressions. */
  def warnings(functions: List[A.Fun]): List[PatternWarning] = {
    val warnings = ListBuffer[PatternWarning]()
    functions.foreach(f => checkNode(f, warnings))
    warnings.toList
  }

  /** Throws the first warning returned by [[warnings]], if any.
    *
    * @throws PatternWarning if {{{warning(functions)}}} is not empty.
    */
  def check(functions: List[A.Fun]): Unit = {
    warnings(functions) match {
      case Nil        => // no warnings
      case first :: _ => throw first
    }
  }

  private def checkNode(node: A.Node, warnings: ListBuffer[PatternWarning]): Unit = {
    def clauseHead(clause: A.Clause) = ClauseHead(clause.pats, clause.guards)
    def clauseHeads(clauses: List[A.Clause]) = clauses.map(clauseHead)
    def branchHeads(branches: List[A.Branch]) = branches.map(b => ClauseHead(List(b.pat), b.guards))

    // Check this node
    node match {
      case node: A.Fun =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: A.FnExp =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: A.NamedFnExp =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: A.CaseExp =>
        checkClauses(node, branchHeads(node.branches), warnings)
      case node: A.TryCatchExp =>
        checkRedundancy(node, branchHeads(node.catchBranches), warnings)
      case node: A.TryOfCatchExp =>
        checkClauses(node, branchHeads(node.tryBranches), warnings)
        checkRedundancy(node, branchHeads(node.catchBranches), warnings)
      case node: A.ReceiveExp =>
        checkRedundancy(node, branchHeads(node.branches), warnings)
      case _: ValDef => // ignore
      case _         => // nothing to check
    }

    // Recursively check all children nodes
    if (!node.isInstanceOf[A.Pat]) { // No need to recurse into patterns
      A.children(node).foreach(child => checkNode(child, warnings))
    }
  }

  /** Checks clauses for exhaustiveness and redundancy.
    *
    * @param node The pattern matching construct being checked.
    * @param warnings The list to append warnings to.
    * @throws MissingPatternsWarning if the clauses are inexhaustive
    * @throws UselessPatternWarning if there is a clause that can never match
    */
  private def checkClauses(
      node: HasSourceLocation,
      clauses: List[ClauseHead],
      warnings: ListBuffer[PatternWarning],
  ): Unit = {
    require(clauses.nonEmpty)

    var previousRows: List[PatternMatrix.Vector] = List()

    // Check for redundancy
    for (clause <- clauses) {
      val simpleClause = clause.patterns.map(Pattern.simplify(vars, program))

      if (!isUseful(PatternMatrix.Matrix(previousRows), simpleClause)) {
        val location =
          if (clause.patterns.isEmpty) Pos.NP
          else Pos.merge(clause.patterns.head.sourceLocation, clause.patterns.last.sourceLocation)
        warnings += new UselessPatternWarning(location)
      }

      if (countsTowardExhaustiveness(clause)) {
        previousRows = previousRows.appended(simpleClause)
      }
    }

    // Check for exhaustiveness
    missingClause(PatternMatrix.Matrix(previousRows), clauses.head.patterns.length) match {
      case None => // exhaustive
      case Some(clause) =>
        val confident = clauses.forall(countsTowardExhaustiveness)
        warnings += new MissingPatternsWarning(node.sourceLocation, confident, clause)
    }
  }

  /** Checks clauses for redundancy only, ignoring exhaustiveness.
    *
    * @param node The pattern matching construct being checked.
    * @param warnings The list to append warnings to.
    * @throws MissingPatternsWarning if the clauses are inexhaustive
    * @throws UselessPatternWarning if there is a clause that can never match
    */
  private def checkRedundancy(
      node: HasSourceLocation,
      clauses: List[ClauseHead],
      warnings: ListBuffer[PatternWarning],
  ) = {
    val allWarnings = ListBuffer[PatternWarning]()
    checkClauses(node, clauses, allWarnings)
    warnings.appendAll(allWarnings.filter { !_.isInstanceOf[MissingPatternsWarning] })
  }

  /** Returns true if the clause contributes to exhaustiveness. A clause does not contribute to exhaustiveness if it
    * uses features (like nonlinear patterns and pattern guards) that we cannot reason about.
    */
  private def countsTowardExhaustiveness(clause: ClauseHead): Boolean =
    clause.guards.isEmpty && isLinear(clause)

  /** Returns true if the patterns in the clause do not use the same variable multiple times.
    *
    * Multiple occurrences of the same variable adds equality constraints, which we cannot reason about.
    */
  private def isLinear(clause: ClauseHead): Boolean = {
    var variables: ListBuffer[String] = ListBuffer()

    def addVariables(node: A.Node): Unit =
      node match {
        case variable: Absyn.VarPat =>
          variables += variable.name
        case _ =>
          A.children(node).foreach(addVariables)
      }

    clause.patterns.foreach(addVariables)
    variables.size == variables.distinct.size
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
        val constructors = col1.collect { case x: Pattern.ConstructorApplication => x.constructor }.toSet

        if (missingConstructors(constructors) == MissingNone) {
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

  /** Returns a pattern vector that matches (some of the) values not matched by the matrix.
    * Returns [[None]] if the matrix matches all possible values.
    *
    * This is used to generate example clauses to help people debug inexhaustive match errors.
    */
  private def missingClause(matrix: PatternMatrix.Matrix, width: Int): Option[PatternMatrix.Vector] =
    (matrix, width) match {
      case (PatternMatrix.Empty(), _) => Some(PatternMatrix.wildcards(width))
      case (_, 0)                     => None
      case (PatternMatrix.AddColumn(col, _), _) =>
        val constructors = col.collect { case x: Pattern.ConstructorApplication => x.constructor }.toSet

        missingConstructors(constructors) match {
          case MissingNone =>
            def tryConstructor(c: Pattern.Constructor) = {
              val constructorArity = arity(c)
              missingClause(specialize(matrix, c), constructorArity + width - 1)
                .map(ps => Pattern.ConstructorApplication(c, ps.take(constructorArity)) :: ps.drop(constructorArity))
            }
            constructors.to(LazyList).flatMap(tryConstructor).headOption
          case MissingAtLeast(constructor) =>
            missingClause(defaultMatrix(matrix), width - 1)
              .map(ps => Pattern.ConstructorApplication(constructor, PatternMatrix.wildcards(arity(constructor))) :: ps)
          case MissingAll =>
            missingClause(defaultMatrix(matrix), width - 1).map(ps => Pattern.Wildcard :: ps)
        }
    }

  /** Specializes the pattern matrix to the case where the first pattern matches the given constructor.
    *
    * Corresponds to the S function in the paper.
    */
  private def specialize(matrix: PatternMatrix.Matrix, constructor: Pattern.Constructor): PatternMatrix.Matrix = {
    PatternMatrix.Matrix(matrix.rows.flatMap(row => specializeRow(row, constructor)))
  }

  /** Like [[specialize]] but takes a single row. */
  private def specializeRow(row: PatternMatrix.Vector, constructor: Pattern.Constructor): Option[PatternMatrix.Vector] =
    row match {
      case Pattern.Wildcard :: rest =>
        Some(PatternMatrix.wildcards(arity(constructor)) ++ rest)
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

  /** Used as the result type of [[missingConstructors]]. */
  private trait MissingConstructors

  /** No missing constructors. */
  private case object MissingNone extends MissingConstructors

  /** At least the specified constructor is missing. There may be others. */
  private case class MissingAtLeast(constructor: Pattern.Constructor) extends MissingConstructors

  /** All constructors are missing, or the data type has (effectively) infinitely many constructors. */
  private case object MissingAll extends MissingConstructors

  /** Returns a pattern describing the set of constructors missing from the given set. */
  private def missingConstructors(presentConstructors: Set[Pattern.Constructor]): MissingConstructors =
    try {
      val all = allConstructors(presentConstructors.head).get
      val missing = all.diff(presentConstructors)
      if (missing.isEmpty) MissingNone else MissingAtLeast(missing.head)
    } catch {
      // May be thrown by [[head]] or [[get]]. Result is the same either way.
      case _: NoSuchElementException => MissingAll
    }

  /** Returns the set of all constructors of a data type, or [[None]] if this set is (effectively) infinite.
    *
    * The data type is specified by giving one of its constructors. This works because each constructor can belong
    * to only one data type.
    */
  private def allConstructors(constructor: Pattern.Constructor): Option[Set[Pattern.Constructor]] =
    constructor match {
      case Pattern.Literal(Values.UnitValue) =>
        Some(Set(constructor))
      case Pattern.Literal(_: Values.BooleanValue) =>
        Some(Set(true, false).map(b => Pattern.Literal(Values.BooleanValue(b))))
      case Pattern.Literal(_: Values.IntegerValue) => None
      case Pattern.Literal(_: Values.CharValue)    => None
      case Pattern.Literal(_: Values.StringValue)  => None
      case Pattern.Tuple(_)                        => Some(Set(constructor))
      case Pattern.EmptyList | Pattern.Cons        => Some(Set(Pattern.EmptyList, Pattern.Cons))
      case Pattern.Record(_, _)                    => Some(Set(constructor))
      case Pattern.ErlangRecord(_, _)              => Some(Set(constructor))
      case Pattern.ExceptionRecord(_, _)           => None
      case Pattern.EnumConstructor(enum, _) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        val constructors = enumDef.cons.map(c => Pattern.EnumConstructor(enum, c.name))
        Some(constructors.toSet)
    }

  /** Returns the number of arguments the given constructor takes. */
  private def arity(constructor: Pattern.Constructor): Int =
    constructor match {
      case _: Pattern.Literal                 => 0
      case Pattern.Tuple(arity)               => arity
      case Pattern.EmptyList                  => 0
      case Pattern.Cons                       => 2
      case Pattern.Record(fields, _)          => fields.length
      case Pattern.ErlangRecord(_, fields)    => fields.length
      case Pattern.ExceptionRecord(_, fields) => fields.length
      case Pattern.EnumConstructor(enum, constructorName) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        enumDef.cons.find(_.name == constructorName).get.argTypes.length
    }
}
