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
// adapted from analyzer
package erlang

import com.whatsapp.coralizer.ast.Id
import erlang.Data._
import erlang.CErl._

object CErlConvert {

  def convertAll[T](objs: List[EObject]): List[T] =
    objs.map(convert).asInstanceOf[List[T]]

  def convert(eObject: EObject): CErl =
    eObject match {
      case ETuple(List(EAtom("c_alias"), anno, cvar, pat)) =>
        CAlias(
          convertAnno(anno),
          convert(cvar).asInstanceOf[CVar],
          convert(pat)
        )
      case ETuple(List(EAtom("c_apply"), anno, op, EList(args, None))) =>
        CApply(convertAnno(anno), convert(op), args.map(convert))
      case ETuple(List(EAtom("c_binary"), anno, EList(segments, None))) =>
        CBinary(convertAnno(anno), segments.map(convertToCBitstr))
      case ETuple(
            List(EAtom("c_bitstr"), anno, cVal, size, unit, cType, flags)
          ) =>
        CBitstr(
          convertAnno(anno),
          convert(cVal),
          convert(size),
          convert(unit),
          convert(cType),
          convert(flags)
        )
      case ETuple(
            List(EAtom("c_call"), anno, module, name, EList(args, None))
          ) =>
        CCall(
          convertAnno(anno),
          convert(module),
          convert(name),
          args.map(convert)
        )
      case ETuple(List(EAtom("c_case"), anno, arg, EList(clauses, None))) =>
        CCase(convertAnno(anno), convert(arg), convertAll[CClause](clauses))
      case ETuple(List(EAtom("c_catch"), anno, body)) =>
        CCatch(convertAnno(anno), convert(body))
      case ETuple(
            List(EAtom("c_clause"), anno, EList(pats, None), guard, body)
          ) =>
        CClause(
          convertAnno(anno),
          pats.map(convert),
          convert(guard),
          convert(body)
        )
      case ETuple(List(EAtom("c_cons"), anno, hd, tl)) =>
        CCons(convertAnno(anno), convert(hd), convert(tl))
      case ETuple(List(EAtom("c_fun"), anno, EList(vars, None), body)) =>
        CFun(convertAnno(anno), convertAll[CVar](vars), convert(body))
      case ETuple(List(EAtom("c_let"), anno, EList(vars, None), arg, body)) =>
        CLet(
          convertAnno(anno),
          convertAll[CVar](vars),
          convert(arg),
          convert(body)
        )
      case ETuple(List(EAtom("c_letrec"), anno, EList(defs, None), body)) =>
        CLetRec(
          convertAnno(anno),
          defs.map(convertTuple2(_).asInstanceOf[(CVar, CFun)]).toMap,
          convert(body)
        )
      case ETuple(List(EAtom("c_literal"), anno, value)) =>
        CLiteral(convertAnno(anno), value)
      // c_map - TODO clarify
      case ETuple(
            List(EAtom("c_map"), anno, arg, EList(es, None), isPat: EAtom)
          ) =>
        CMap(
          convertAnno(anno),
          convert(arg),
          es.map(convert),
          isPat.asBoolean()
        )
      // c_map_pair TODO - clarify
      case ETuple(List(EAtom("c_map_pair"), anno, op, key, cVal)) =>
        CMapPair(convertAnno(anno), convert(op), convert(key), convert(cVal))
      case ETuple(
            List(
              EAtom("c_module"),
              anno,
              name,
              EList(exports, None),
              EList(attrs, None),
              EList(defs, None)
            )
          ) =>
        CModule(
          convertAnno(anno),
          convert(name),
          exports.map(convert),
          attrs.map(convertTuple2),
          defs.map(convertTuple2)
        )
      case ETuple(List(EAtom("c_primop"), anno, name, EList(args, None))) =>
        CPrimOp(convertAnno(anno), convert(name), args.map(convert))
      case ETuple(
            List(
              EAtom("c_receive"),
              anno,
              EList(clauses, None),
              timeout,
              action
            )
          ) =>
        CReceive(
          convertAnno(anno),
          clauses.map(convert),
          convert(timeout),
          convert(action)
        )
      case ETuple(List(EAtom("c_seq"), anno, arg, body)) =>
        CSeq(convertAnno(anno), convert(arg), convert(body))
      case ETuple(
            List(
              EAtom("c_try"),
              anno,
              arg,
              EList(vars, None),
              body,
              EList(evars, None),
              handler
            )
          ) =>
        CTry(
          convertAnno(anno),
          convert(arg),
          convertAll[CVar](vars),
          convert(body),
          convertAll[CVar](evars),
          convert(handler)
        )
      case ETuple(List(EAtom("c_tuple"), anno, EList(es, None))) =>
        CTuple(convertAnno(anno), es.map(convert))
      case ETuple(List(EAtom("c_values"), anno, EList(es, None))) =>
        CValues(convertAnno(anno), es.map(convert))
      case ETuple(List(EAtom("c_var"), anno, eVar)) =>
        CVar(convertAnno(anno), convertEvar(eVar))
      case _ =>
        sys.error(s"unexpected: $eObject")
    }

  def convertToCBitstr(eObject: EObject): CBitstr = {
    val cErl = convert(eObject)
    cErl match {
      case cBitstr: CBitstr =>
        cBitstr
      case _ =>
        sys.error(s"Unexpected: ${cErl.getClass}")
    }
  }

  private def convertAnno(data: EObject): Anno =
    data match {
      case EList(List(ETuple(_), ELong(line), ETuple(_)), _) =>
        Anno(line.intValue)
      case EList(List(ELong(line), _), None) => Anno(line.intValue)
      // special value: should not appear in any error messages
      case _ => Anno(0)
    }

  private def convertEvar(eVar: EObject): VarName =
    eVar match {
      case ELong(i)    => VarNameInt(i.intValue)
      case EAtom(atom) => VarNameAtom(atom)
      case ETuple(List(EAtom(atom), ELong(i))) =>
        VarNameAtomInt(Id(atom, i.intValue))
      case _ => sys.error(s"unexpected: $eVar")
    }

  def convertTuple2(eObject: EObject): (CErl, CErl) =
    eObject match {
      case ETuple(List(e1, e2)) => (convert(e1), convert(e2))
      case _                    => sys.error(s"unexpected $eObject")
    }
}
