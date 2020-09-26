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

import com.whatsapp.sterlang._

import scala.collection.mutable.ListBuffer

object PatternChecker {
  type Vector = List[Pattern.Pat]
  type Matrix = List[Vector]

  /** Returns a pattern vector of the given length containing wildcards. */
  def wildcards(length: Int): Vector =
    List.fill(length)(Pattern.Wildcard)
  def firstColumn(rows: List[Vector]): Option[Vector] =
    if (rows.isEmpty) None else Some(rows.map(_.head))
}

/** Generates warnings for missing and redundant clauses in pattern matching.
  *
  * Based on [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]].
  */
class PatternChecker(private val vars: Vars, private val context: Context, val program: Ast.Program) {
  import PatternChecker._

  /** Like [[AnnAst.Clause]] but without the body. */
  private case class ClauseHead(patterns: List[AnnAst.Pat], guards: List[AnnAst.Guard])

  /** Returns a list of all pattern matching related issues in the given functions and their subexpressions. */
  def warnings(functions: List[AnnAst.Fun]): List[PatternWarning] = {
    val warnings = ListBuffer[PatternWarning]()
    functions.foreach(f => checkNode(f, warnings))
    warnings.toList
  }

  /** Throws the first warning returned by [[warnings]], if any.
    *
    * @throws PatternWarning if {{{warning(functions)}}} is not empty.
    */
  def check(functions: List[AnnAst.Fun]): Unit = {
    warnings(functions) match {
      case Nil        => // no warnings
      case first :: _ => throw first
    }
  }

  private def checkNode(node: AnnAst.Node, warnings: ListBuffer[PatternWarning]): Unit = {
    def clauseHead(clause: AnnAst.Clause) = ClauseHead(clause.pats, clause.guards)
    def clauseHeads(clauses: List[AnnAst.Clause]) = clauses.map(clauseHead)
    def branchHeads(branches: List[AnnAst.Branch]) = branches.map(b => ClauseHead(List(b.pat), b.guards))

    // Check this node
    node match {
      case node: AnnAst.Fun =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: AnnAst.FnExp =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: AnnAst.NamedFnExp =>
        checkClauses(node, clauseHeads(node.clauses), warnings)
      case node: AnnAst.CaseExp =>
        checkClauses(node, branchHeads(node.branches), warnings)
      case node: AnnAst.TryCatchExp =>
        checkRedundancy(node, branchHeads(node.catchBranches), warnings)
      case node: AnnAst.TryOfCatchExp =>
        checkClauses(node, branchHeads(node.tryBranches), warnings)
        checkRedundancy(node, branchHeads(node.catchBranches), warnings)
      case node: AnnAst.ReceiveExp =>
        checkRedundancy(node, branchHeads(node.branches), warnings)
      case _: AnnAst.ValDef => // ignore
      case _                => // nothing to check
    }

    // Recursively check all children nodes
    if (!node.isInstanceOf[AnnAst.Pat]) { // No need to recurse into patterns
      AnnAst.children(node).foreach(child => checkNode(child, warnings))
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
      node: AnnAst.Node,
      clauses: List[ClauseHead],
      warnings: ListBuffer[PatternWarning],
  ): Unit = {
    require(clauses.nonEmpty)

    var previousRows: List[Vector] = List()

    // Check for redundancy
    for (clause <- clauses) {
      val simpleClause = clause.patterns.map(Pattern.simplify(vars, program))

      if (!isUseful(previousRows, simpleClause)) {
        val location =
          if (clause.patterns.isEmpty) Doc.ZRange
          else Doc.merge(clause.patterns.head.r, clause.patterns.last.r)
        warnings += new UselessPatternWarning(location)
      }

      if (countsTowardExhaustiveness(clause)) {
        previousRows = previousRows.appended(simpleClause)
      }
    }

    // Check for exhaustiveness
    missingClause(previousRows, clauses.head.patterns.length) match {
      case None => // exhaustive
      case Some(clause) =>
        val confident = clauses.forall(countsTowardExhaustiveness)
        warnings += new MissingPatternsWarning(node.r, confident, clause)
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
      node: AnnAst.Node,
      clauses: List[ClauseHead],
      warnings: ListBuffer[PatternWarning],
  ) = {
    val allWarnings = ListBuffer[PatternWarning]()
    checkClauses(node, clauses, allWarnings)
    warnings.appendAll(allWarnings.filter { !_.isInstanceOf[MissingPatternsWarning] })
  }

  /** Returns true if the clause contributes to exhaustiveness. AnnAst clause does not contribute to exhaustiveness if it
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

    def addVariables(node: AnnAst.Node): Unit =
      node match {
        case variable: AnnAst.VarPat =>
          variables += variable.name
        case _ =>
          AnnAst.children(node).foreach(addVariables)
      }

    clause.patterns.foreach(addVariables)
    variables.size == variables.distinct.size
  }

  /** Returns true if the clause is useful with respect to the pattern matrix.
    * AnnAst clause is useful if there is a value vector matched by the clause but not by the pattern matrix.
    *
    * Corresponds to the U_rec function in the paper.
    */
  private def isUseful(matrix: Matrix, clause: Vector): Boolean =
    if (matrix.isEmpty) true
    else
      clause match {
        case Nil => false
        case p :: ps =>
          (firstColumn(matrix), p) match {
            case (Some(col), Pattern.Wildcard) =>
              val ctrs = col.collect { case x: Pattern.CtrApp => x.ctr }.toSet
              missingCtrs(ctrs) match {
                case MissingNone =>
                  ctrs.exists { ctr => isUseful(specialize(matrix, ctr), specializeRow(clause, ctr).get) }
                case _ => isUseful(defaultMatrix(matrix), ps)
              }
            case (_, Pattern.CtrApp(ctr, args)) =>
              isUseful(specialize(matrix, ctr), args ++ ps)
          }
      }

  /** Returns a pattern vector that matches (some of the) values not matched by the matrix.
    * Returns [[None]] if the matrix matches all possible values.
    *
    * This is used to generate example clauses to help people debug inexhaustive match errors.
    */
  private def missingClause(matrix: Matrix, width: Int): Option[Vector] = {
    if (matrix.isEmpty) Some(wildcards(width))
    else if (width == 0) None
    else {
      val Some(col) = firstColumn(matrix)
      val ctrs = col.collect { case Pattern.CtrApp(ctr, _) => ctr }.toSet
      missingCtrs(ctrs) match {
        case MissingNone | MissingAbstract =>
          def tryConstructor(ctr: Pattern.Ctr) = {
            val ctrArity = arity(ctr)
            missingClause(specialize(matrix, ctr), ctrArity + width - 1)
              .map(ps => Pattern.CtrApp(ctr, ps.take(ctrArity)) :: ps.drop(ctrArity))
          }
          ctrs.to(LazyList).flatMap(tryConstructor).headOption
        case MissingAtLeast(ctr) =>
          missingClause(defaultMatrix(matrix), width - 1)
            .map(ps => Pattern.CtrApp(ctr, wildcards(arity(ctr))) :: ps)
        case MissingAll =>
          missingClause(defaultMatrix(matrix), width - 1).map(ps => Pattern.Wildcard :: ps)
      }
    }
  }

  /** Specializes the pattern matrix to the case where the first pattern matches the given constructor.
    *
    * Corresponds to the S function in the paper.
    */
  private def specialize(matrix: Matrix, ctr: Pattern.Ctr): Matrix =
    matrix.flatMap(row => specializeRow(row, ctr))

  /** Like [[specialize]] but takes a single row. */
  private def specializeRow(row: Vector, ctr: Pattern.Ctr): Option[Vector] =
    row match {
      case Pattern.Wildcard :: rest =>
        Some(wildcards(arity(ctr)) ++ rest)
      case Pattern.CtrApp(`ctr`, args) :: rest =>
        Some(args ++ rest)
      case _ =>
        None
    }

  /** Corresponds to the D function in the paper. */
  private def defaultMatrix(matrix: Matrix): Matrix =
    matrix.collect { case Pattern.Wildcard :: tail => tail }

  /** Used as the result type of [[missingCtrs]]. */
  private trait MissingConstructors

  /** No missing constructors. */
  private case object MissingNone extends MissingConstructors

  /** At least the specified constructor is missing. There may be others. */
  private case class MissingAtLeast(ctr: Pattern.Ctr) extends MissingConstructors

  /** There were [[Pattern.AbstractCtr]]s, so an unknown set of constructors is missing. */
  private case object MissingAbstract extends MissingConstructors

  /** All constructors are missing, or the data type has (effectively) infinitely many constructors. */
  private case object MissingAll extends MissingConstructors

  /** Returns a pattern describing the set of constructors missing from the given set. */
  private def missingCtrs(ctrs: Set[Pattern.Ctr]): MissingConstructors = {
    val concreteCtrs = ctrs.filter(!_.isInstanceOf[Pattern.AbstractCtr])
    val hasAbstract = concreteCtrs.size < ctrs.size
    val missingOption = concreteCtrs.headOption.flatMap(allCtrs).map(_.diff(concreteCtrs))
    missingOption match {
      case Some(missing) if missing.isEmpty =>
        MissingNone
      case _ if hasAbstract =>
        MissingAbstract
      case Some(missing) =>
        MissingAtLeast(missing.head)
      case None =>
        MissingAll
    }
  }

  /** Returns the set of all constructors of a data type, or [[None]] if this set is (effectively) infinite.
    *
    * The data type is specified by giving one of its constructors. This works because each constructor can belong
    * to only one data type.
    */
  private def allCtrs(ctr: Pattern.Ctr): Option[Set[Pattern.Ctr]] =
    ctr match {
      case Pattern.Literal(_: Ast.BooleanVal) =>
        Some(Set(true, false).map(b => Pattern.Literal(Ast.BooleanVal(b))))
      case Pattern.Literal(_: Ast.IntVal)    => None
      case Pattern.Literal(_: Ast.CharVal)   => None
      case Pattern.Literal(_: Ast.StringVal) => None
      case Pattern.Tuple(_)                  => Some(Set(ctr))
      case Pattern.EmptyList | Pattern.Cons  => Some(Set(Pattern.EmptyList, Pattern.Cons))
      case Pattern.Shape(_)                  => Some(Set(ctr))
      case Pattern.Struct(_, _)              => Some(Set(ctr))
      case Pattern.OpenStruct(_, _)          => None
      case Pattern.EnumCtr(enum, _) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        val ctrs = enumDef.ctrs.map(c => Pattern.EnumCtr(enum, c.name))
        Some(ctrs.toSet)
      case Pattern.AbstractCtr(_) => throw new IllegalArgumentException()
    }

  /** Returns the number of arguments the given constructor takes. */
  private def arity(ctr: Pattern.Ctr): Int =
    ctr match {
      case _: Pattern.Literal            => 0
      case Pattern.Tuple(arity)          => arity
      case Pattern.EmptyList             => 0
      case Pattern.Cons                  => 2
      case Pattern.Shape(fields)         => fields.length
      case Pattern.Struct(_, fields)     => fields.length
      case Pattern.OpenStruct(_, fields) => fields.length
      case Pattern.EnumCtr(enum, ctrName) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        enumDef.ctrs.find(_.name == ctrName).get.argTypes.length
      case Pattern.AbstractCtr(_) => 0
    }
}
