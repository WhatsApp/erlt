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

import scala.collection.immutable.TreeSet

class Unify(private val vars: Vars) {

  case class UnifyError(s: String) extends Exception(s)

  private val T = Types

  private sealed trait UVar
  private case class UTypeVar(tv: T.TypeVar) extends UVar
  private case class URowTypeVar(rtv: T.RowTypeVar) extends UVar

  // occurs check + adjusting
  private def adjust(v: UVar, d: T.Depth, t: T.Type): Unit = {
    var visited: TreeSet[T.TypeVar] = TreeSet.empty(vars.TVarOrdering)

    def loop(v: UVar)(t: T.Type): Unit =
      t match {
        case T.VarType(tv1) =>
          if (!visited(tv1)) {
            visited = visited + tv1
            vars.tGet(tv1) match {
              case T.Instance(t) =>
                loop(v)(t)
              case T.Open(d1) =>
                vars.tSet(tv1, T.Open(Integer.min(d, d1)))
                v match {
                  case UTypeVar(tv) if vars.tEq(tv, tv1) =>
                    throw Circularity
                  case _ =>
                  // OK
                }
            }
          }
        case T.ConType(_, ts, rts) =>
          ts.foreach(loop(v))
          rts.foreach(rowLoop(v))
      }

    @scala.annotation.tailrec
    def rowLoop(v: UVar)(rt: T.RowType): Unit =
      rt match {
        case T.RowVarType(rtv1) =>
          vars.rGet(rtv1) match {
            case T.RowInstance(rt) =>
              rowLoop(v)(rt)
            case T.RowOpen(d1, k) =>
              vars.rSet(rtv1, T.RowOpen(Integer.min(d, d1), k))
              v match {
                case URowTypeVar(rtv) if vars.rEq(rtv, rtv1) =>
                  throw RowCircularity
                case _ =>
                // OK
              }
          }
        case T.RowEmptyType =>
        // OK
        case T.RowFieldType(T.Field(_, t), rt) =>
          loop(v)(t)
          rowLoop(v)(rt)
      }

    loop(v)(t)
  }

  private def unifyV(tv: T.TypeVar, conType: T.ConType): Unit =
    vars.tGet(tv) match {
      case T.Instance(t) =>
        unify(t, conType)
      case T.Open(d) =>
        adjust(UTypeVar(tv), d, conType)
        vars.tSet(tv, T.Instance(conType))
    }

  private def unifyCon(t1: T.ConType, t2: T.ConType): Unit =
    if (T.sameTyc(t1.tyCon, t2.tyCon) && t1.ts.size == t2.ts.size && t1.rts.size == t2.rts.size) {
      (t1.ts zip t2.ts).foreach(p => unify(p._1, p._2))
      (t1.rts zip t2.rts).foreach(p => rowUnify(p._1, p._2))
    } else {
      throw TyConMismatch(t1.tyCon, t2.tyCon)
    }

  private def rowUnify(rt1: T.RowType, rt2: T.RowType): Unit = {
    @scala.annotation.tailrec
    def collect(
        rowType: T.RowType,
        m: Map[String, T.Field],
    ): (Map[String, T.Field], Option[(T.RowTypeVar, T.RowOpen)]) =
      rowType match {
        case T.RowVarType(v) =>
          vars.rGet(v) match {
            case T.RowInstance(rt) => collect(rt, m)
            case ro: T.RowOpen     => (m, Some((v, ro)))
          }
        case T.RowFieldType(f, rt) => collect(rt, m + (f.label -> f))
        case T.RowEmptyType        => (m, None)
      }

    val (fields1, opVar1) = collect(rt1, Map.empty)
    val (fields2, opVar2) = collect(rt2, Map.empty)

    val commonLabels = (fields1.keySet intersect fields2.keySet).toList.sorted
    val only1Labels = (fields1.keySet -- commonLabels).toList.sorted
    val only2Labels = (fields2.keySet -- commonLabels).toList.sorted
    val only1 = only1Labels.map(fields1)
    val only2 = only2Labels.map(fields2)
    val commonFields = commonLabels.map(l => (fields1(l).value, fields2(l).value))

    def inst(v: T.RowTypeVar, open: T.RowOpen, fields: List[T.Field], base: T.RowType): Unit = {
      fields.foreach { f => adjust(URowTypeVar(v), open.d, f.value) }
      // $COVERAGE-OFF$ extensible-shapes
      fields.find { f => open.kind(f.label) } foreach { f => throw FieldMismatch(f.label) }
      // $COVERAGE-ON$
      vars.rSet(v, T.RowInstance(fields.foldRight(base)(T.RowFieldType)))
    }

    (opVar1, opVar2) match {
      case (Some((v1, open1)), Some((v2, open2))) =>
        val kind = open1.kind union open2.kind
        val level = Integer.min(open1.d, open2.d)
        val base = T.RowVarType(vars.rVar(T.RowOpen(level, kind)))
        inst(v1, open1, only2, base)
        inst(v2, open2, only1, base)
      case (Some((v1, open1)), None) =>
        only1 match {
          case Nil                => inst(v1, open1, only2, T.RowEmptyType)
          case T.Field(l, _) :: _ => throw FieldMismatch(l)
        }
      case (None, Some((v2, open2))) =>
        only2 match {
          case Nil                => inst(v2, open2, only1, T.RowEmptyType)
          case T.Field(l, _) :: _ => throw FieldMismatch(l)
        }
      case (None, None) =>
        (only1, only2) match {
          case (T.Field(l, _) :: _, _) => throw FieldMismatch(l)
          case (_, T.Field(l, _) :: _) => throw FieldMismatch(l)
          case (Nil, Nil)              => // OK
        }
    }

    // Unification of common fields could instantiate opVar1 or opVar2 so
    // we must defer these steps until the shapes of rt1 and rt2 have been
    // unified already. Therefore, we run unification of these common fields last.
    commonFields.foreach { case (f1, f2) => unify(f1, f2) }
  }

  @throws[UnifyError]
  def unify(t1: T.Type, t2: T.Type): Unit =
    (t1, t2) match {
      case (T.VarType(v1), T.VarType(v2)) =>
        if (!vars.tEq(v1, v2)) (vars.tGet(v1), vars.tGet(v2)) match {
          case (T.Open(d1), T.Open(d2)) =>
            vars.tLink(v1, v2)
            vars.tSet(v1, T.Open(Integer.min(d1, d2)))
          case (T.Instance(t), T.Open(_)) =>
            vars.tLink(v1, v2)
            unifyV(v2, t)
          case (T.Open(_), T.Instance(t)) =>
            vars.tLink(v2, v1)
            unifyV(v1, t)
          case (T.Instance(t1), T.Instance(t2)) =>
            vars.tLink(v1, v2)
            unifyCon(t1, t2)
        }
      case (T.VarType(v), t: T.ConType)   => unifyV(v, t)
      case (t: T.ConType, T.VarType(v))   => unifyV(v, t)
      case (t1: T.ConType, t2: T.ConType) => unifyCon(t1, t2)
    }
}
