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

import scala.collection.mutable.ListBuffer

object PatternChecker {
  private type Vector = List[Pat]
  private type Matrix = List[Vector]

  private def wildcards(length: Int): Vector =
    List.fill(length)(Wildcard)
  private def firstColumn(rows: List[Vector]): Vector =
    rows.map(_.head)

  private sealed trait Pat
  private case object Wildcard extends Pat
  private case class CtrApp(ctr: Ctr, args: List[Pat]) extends Pat

  private sealed trait Ctr
  private case class Literal(value: Ast.Val) extends Ctr
  private case class Tuple(length: Int) extends Ctr
  private case object EmptyList extends Ctr
  private case object Cons extends Ctr
  private case class Shape(fields: List[String]) extends Ctr
  private case class Struct(struct: String, fields: List[String]) extends Ctr
  private case class OpenStruct(struct: String, fields: List[String]) extends Ctr
  private case class EnumCtr(enum: String, ctr: String) extends Ctr

  /** An unknown constructor of some/any data type.
    *
   * Intuitively, this constructor will match some unknown subset of the value space.
    * We compile patterns we cannot reason about to this class.
    *
   * Constructors corresponding to different patterns should be different.
    * So, we use the range of the original pattern as an identity of AbstractCtr
    */
  private case class AbstractCtr(r: Doc.Range) extends Ctr
  private def clauseHead(clause: AnnAst.Clause) = ClauseHead(clause.pats, clause.guards)
  private def clauseHeads(clauses: List[AnnAst.Clause]) = clauses.map(clauseHead)
  private def branchHeads(branches: List[AnnAst.Branch]) = branches.map(b => ClauseHead(List(b.pat), b.guards))

  /** Like [[AnnAst.Clause]] but without the body. */
  private case class ClauseHead(patterns: List[AnnAst.Pat], guards: List[AnnAst.Guard])

  private def show(p: Pat): String = {
    def tuple(args: List[Pat]): String =
      args.map(show).mkString("{", ", ", "}")

    p match {
      case Wildcard                                    => "_"
      case CtrApp(Literal(Ast.BooleanVal(value)), Nil) => value.toString
      case CtrApp(Tuple(_), args)                      => tuple(args)
      case CtrApp(EmptyList, Nil)                      => "[]"
      case CtrApp(Cons, List(head, tail))              =>
        // Logic for displaying multi element lists nicely, e.g., [E1, E2, E3 | T].
        val result = new StringBuilder("[")
        result ++= show(head)

        def processTail(tail: Pat): Unit =
          tail match {
            case CtrApp(EmptyList, Nil) =>
              result += ']'
            case CtrApp(Cons, h :: t :: Nil) =>
              result ++= ", "
              result ++= show(h)
              processTail(t)
            case _ =>
              result ++= " | "
              result ++= show(tail)
              result += ']'
          }

        processTail(tail)
        result.toString()

      // $COVERAGE-OFF$ unreachable
      case CtrApp(EmptyList, _) | CtrApp(Cons, _) =>
        throw new IllegalArgumentException()
      case CtrApp(Literal(_), _) =>
        throw new IllegalArgumentException()
      case CtrApp(OpenStruct(_, _), _) =>
        throw new IllegalArgumentException()
      // $COVERAGE-ON$

      case CtrApp(Shape(fieldNames), args) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(args).filter(_._2 != Wildcard)
        fields.map(f => s"${f._1} := ${show(f._2)}").mkString(start = "#{", sep = ", ", end = "}")

      case CtrApp(Struct(struct, fieldNames), args) =>
        // Remove fields mapped to wildcards to reduce clutter
        val fields = fieldNames.zip(args).filter(_._2 != Wildcard)
        fields.map(f => s"${f._1} = ${show(f._2)}").mkString(start = s"#$struct{", sep = ", ", end = "}")

      case CtrApp(EnumCtr(enum, ctr), args) =>
        s"$enum.$ctr${tuple(args)}"

      case CtrApp(AbstractCtr(_), _) =>
        "_"
    }
  }
}

/** Generates warnings for missing and redundant clauses in pattern matching.
  *
  * Based on [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]].
  */
class PatternChecker(private val tu: TypesUtil, private val context: Context, val program: Ast.Program) {
  import PatternChecker._

  /** Returns a list of all pattern matching related issues in the given functions and their subexpressions. */
  def warnings(functions: List[AnnAst.Fun]): List[PatternWarning] = {
    val warnings = ListBuffer[PatternWarning]()
    functions.foreach(f => checkNode(f, warnings))
    warnings.toList
  }

  /** Converts a surface syntax pattern to the simplified representation here. */
  private def simplify(pattern: AnnAst.Pat): Pat =
    pattern match {
      case AnnAst.WildPat() | AnnAst.VarPat(_) =>
        Wildcard
      case AnnAst.AndPat(p1, p2) =>
        (simplify(p1), simplify(p2)) match {
          // For now, we only reason about "and" patterns that are used to name the overall value.
          // For example, `{5, Y} = Z` is OK, whereas `{5, Y} = {X, "string"}` isn't.
          case (Wildcard, simple) => simple
          case (simple, Wildcard) => simple
        }
      case AnnAst.LiteralPat(value) =>
        CtrApp(Literal(value), Nil)
      case AnnAst.TuplePat(elements) =>
        CtrApp(Tuple(elements.length), elements.map(simplify))
      case AnnAst.ShapePat(patternFields) =>
        val patternFieldsMap = patternFields.map { f => (f.label, f.value) }.toMap
        val allFieldNames: List[String] = tu.labels(pattern.typ).sorted
        CtrApp(
          Shape(allFieldNames),
          allFieldNames.map(f => patternFieldsMap.get(f).map(simplify).getOrElse(Wildcard)),
        )
      case AnnAst.StructPat(name, patternFields) =>
        val posFields = patternFields.collect { case pf: AnnAst.PosField[AnnAst.Pat] => pf }
        val lblFields = patternFields.collect { case lf: AnnAst.LblField[AnnAst.Pat] => lf }

        val lblFieldsMap = lblFields.map { f => (f.label, f.value) }.toMap
        val posFieldsMap = posFields.zipWithIndex.map { case (f, i) => (s"_$i", f.value) }.toMap
        val allFieldsMap = lblFieldsMap ++ posFieldsMap

        val structDef = program.structDefs.find(_.name == name).get
        val posFieldDecls = structDef.fields.collect { case pf: Ast.PosFieldDecl => pf }
        val lblFieldDecls = structDef.fields.collect { case lf: Ast.LblFieldDecl => lf }

        val posFieldNames = posFieldDecls.indices.map(i => s"_$i")
        val lblFieldNames = lblFieldDecls.map(_.label)

        val allFieldNames = (lblFieldNames ++ posFieldNames).sorted

        val ctr = if (structDef.kind == Ast.StrStruct) Struct else OpenStruct
        CtrApp(
          ctr(name, allFieldNames),
          allFieldNames.map(f => allFieldsMap.get(f).map(simplify).getOrElse(Wildcard)),
        )
      case _: AnnAst.BinPat =>
        CtrApp(AbstractCtr(pattern.r), Nil)
      case _: AnnAst.PinnedVarPat =>
        CtrApp(AbstractCtr(pattern.r), Nil)
      case AnnAst.NilPat() =>
        CtrApp(EmptyList, Nil)
      case AnnAst.ConsPat(head, tail) =>
        CtrApp(Cons, List(simplify(head), simplify(tail)))
      case AnnAst.EnumPat(enum, ctr, args) =>
        CtrApp(EnumCtr(enum, ctr), args.map(simplify))
    }

  private def checkNode(node: AnnAst.Node, warnings: ListBuffer[PatternWarning]): Unit = {
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
      val simpleClause = clause.patterns.map(simplify)
      if (clause.patterns.nonEmpty && !isUseful(previousRows, simpleClause)) {
        val location = Doc.merge(clause.patterns.head.r, clause.patterns.last.r)
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
        val clauseStr = clause.map(show).mkString(sep = ", ")
        warnings += new MissingPatternsWarning(node.r, confident, clauseStr)
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

  /** Returns true if the clause contributes to exhaustiveness.
    *
    * AnnAst clause does not contribute to exhaustiveness if it uses features
    * (like nonlinear patterns and pattern guards) that we cannot reason about.
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
        case CtrApp(ctr, args) :: ps =>
          isUseful(specialize(matrix, ctr), args ++ ps)
        case Wildcard :: ps =>
          val ctrs = firstColumn(matrix).collect { case x: CtrApp => x.ctr }.toSet
          missingCtrs(ctrs) match {
            case MissingNone =>
              ctrs.exists { ctr => isUseful(specialize(matrix, ctr), specializeRow(clause, ctr).get) }
            case _ => isUseful(defaultMatrix(matrix), ps)
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
      val ctrs = firstColumn(matrix).collect { case CtrApp(ctr, _) => ctr }.toSet
      missingCtrs(ctrs) match {
        case MissingNone | MissingAbstract =>
          def tryConstructor(ctr: Ctr) = {
            val ctrArity = arity(ctr)
            missingClause(specialize(matrix, ctr), ctrArity + width - 1)
              .map(ps => CtrApp(ctr, ps.take(ctrArity)) :: ps.drop(ctrArity))
          }
          ctrs.to(LazyList).flatMap(tryConstructor).headOption
        case MissingAtLeast(ctr) =>
          missingClause(defaultMatrix(matrix), width - 1).map(CtrApp(ctr, wildcards(arity(ctr))) :: _)
        case MissingAll =>
          missingClause(defaultMatrix(matrix), width - 1).map(Wildcard :: _)
      }
    }
  }

  /** Specializes the pattern matrix to the case where the first pattern matches the given constructor.
    *
    * Corresponds to the S function in the paper.
    */
  private def specialize(matrix: Matrix, ctr: Ctr): Matrix =
    matrix.flatMap(row => specializeRow(row, ctr))

  /** Like [[specialize]] but takes a single row. */
  private def specializeRow(row: Vector, ctr: Ctr): Option[Vector] =
    row match {
      case Wildcard :: rest =>
        Some(wildcards(arity(ctr)) ++ rest)
      case CtrApp(`ctr`, args) :: rest =>
        Some(args ++ rest)
      case _ =>
        None
    }

  /** Corresponds to the D function in the paper. */
  private def defaultMatrix(matrix: Matrix): Matrix =
    matrix.collect { case Wildcard :: tail => tail }

  /** Used as the result type of [[missingCtrs]]. */
  private trait MissingConstructors

  /** No missing constructors. */
  private case object MissingNone extends MissingConstructors

  /** At least the specified constructor is missing. There may be others. */
  private case class MissingAtLeast(ctr: Ctr) extends MissingConstructors

  /** There were [[AbstractCtr]]s, so an unknown set of constructors is missing. */
  private case object MissingAbstract extends MissingConstructors

  /** All constructors are missing, or the data type has (effectively) infinitely many constructors. */
  private case object MissingAll extends MissingConstructors

  /** Returns a pattern describing the set of constructors missing from the given set. */
  private def missingCtrs(ctrs: Set[Ctr]): MissingConstructors = {
    val concreteCtrs = ctrs.filter(!_.isInstanceOf[AbstractCtr])
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
  private def allCtrs(ctr: Ctr): Option[Set[Ctr]] =
    ctr match {
      case Literal(_: Ast.BooleanVal) =>
        Some(Set(true, false).map(b => Literal(Ast.BooleanVal(b))))
      case Literal(_: Ast.NumberVal) => None
      case Literal(_: Ast.CharVal)   => None
      case Literal(_: Ast.StringVal) => None
      case Tuple(_)                  => Some(Set(ctr))
      case EmptyList | Cons          => Some(Set(EmptyList, Cons))
      case Shape(_)                  => Some(Set(ctr))
      case Struct(_, _)              => Some(Set(ctr))
      case OpenStruct(_, _)          => None
      case EnumCtr(enum, _) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        val ctrs = enumDef.ctrs.map(c => EnumCtr(enum, c.name))
        Some(ctrs.toSet)
      // $COVERAGE-OFF$ unreachable
      case AbstractCtr(_) => throw new IllegalArgumentException()
      // $COVERAGE-ON$
    }

  /** Returns the number of arguments the given constructor takes. */
  private def arity(ctr: Ctr): Int =
    ctr match {
      case _: Literal            => 0
      case Tuple(arity)          => arity
      case EmptyList             => 0
      case Cons                  => 2
      case Shape(fields)         => fields.length
      case Struct(_, fields)     => fields.length
      case OpenStruct(_, fields) => fields.length
      case EnumCtr(enum, ctrName) =>
        val enumDef = context.enumDefs.find(_.name == enum).get
        enumDef.ctrs.find(_.name == ctrName).get.argTypes.length
      case AbstractCtr(_) => 0
    }
}
