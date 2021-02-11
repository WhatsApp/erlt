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

package com.whatsapp.coralizer.ast

import com.whatsapp.coralizer.tc.TcDiagnostics.TypeError
import erlang.Data._
import erlang.CErl._

object PrettyCErl {
  def apply(
      e: CErl,
      moduleStub: ModuleStub,
      errors: Map[Int, TypeError],
      width: Int
  ): String =
    new PrettyCErl(moduleStub, errors).formattedLayout(e, width)
}

private class PrettyCErl(moduleStub: ModuleStub, errors: Map[Int, TypeError])
    extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

  def formattedLayout(e: CErl, width: Int): String =
    pretty(e, w = width).layout

  def doc(e: CErl): Doc =
    e match {
      case CAlias(_, v, pat)    => v <> "=" <> pat
      case CApply(_, f, args)   => f <> parens(args)
      case CBinary(_, segments) => "<<" <> segments <> ">>"
      case CBitstr(_, value, size, unit, typ, flags) =>
        value <> ":" <+> "size-" <> size <+> "unit-" <> unit <+> "type-" <> typ <+> "flags-" <> flags
      case CCall(_, m, f, args) =>
        m <> ":" <> f <> parens(args)
      case CCase(_, sel, clauses) =>
        softbreak <> "case" <+> sel <+> "of" <+> block(vsep(clauses map doc))
      case CCatch(_, body) => "catch" <+> body
      case CClause(_, pats, guard, body) =>
        val left: Doc =
          if (pats.sizeCompare(1) == 0) pats else "<" <> pats <> ">"
        val right = "when" <+> guard <+> "->" </> nest(body)
        left <+> right
      case CCons(_, hd, tl) => "[" <> hd <+> "|" <+> tl <> "]"
      case CFun(_, vars, body) =>
        softbreak <> "fun" <+> parens(vars) <+> block(body)
      case CLet(_, vars, arg, body) =>
        softbreak <> "let " <> vars <+> "=" <+> arg <+> "in" <+> block(body)
      case CLetRec(_, defs, body) =>
        val args =
          for ((k, v) <- defs)
            yield k <+> "=" <+> v
        softbreak <> "letrec" <+> parens(docArgs(args)) <+> "in" <+> block(body)
      case CLiteral(_, data)           => data
      case CMap(__, arg, es, _isPat)   => arg <> "#" <> braces(es)
      case CMapPair(_, _op, key, cVal) => key <+> "=>" <+> cVal
      case CModule(_, name, exports, attrs, defs) =>
        def defToDoc(d: (CVar, CFun)): Doc =
          line <> d._1 <+> "=" <+> d._2

        val typeDocs = moduleStub.types.map {
          case (Id(name, arity), decl) =>
            text(s"-type $name($arity) :: " + Show.show(decl.body))
        }.toList

        def specToDoc(id: Id): Doc =
          moduleStub.specs
            .get(id)
            .map(spec =>
              line <> vsep(spec.types map { x =>
                ":: " + Show.show(x.ty)
              })
            )
            .getOrElse(emptyDoc)

        val defsDocs = defs map {
          case d @ (CVar(_, VarNameAtomInt(id)), _) =>
            specToDoc(id) <> defToDoc(d)
          // $COVERAGE-OFF$
          case d => sys.error(s"unexpected $d")
          // $COVERAGE-ON$
        }

        val attrDocs =
          for (
            (k, v) <- attrs filter (x =>
              x._1 != "spec" && x._1 != "type" && x._1 != "opaque"
            )
          )
            yield "-" <> k <> "(" <> v <> ")"

        val moduleDoc = "-module" <> parens(name)
        val exportsDoc = "-exports" <> parens(exports)

        vsep(moduleDoc :: exportsDoc :: attrDocs ++ typeDocs ++ defsDocs)

      case CPrimOp(_, name, args) => "primop:" <> name <> parens(args)
      case CSeq(_, arg, body)     => arg <+> "," <+> body
      case CTry(_, arg, bodyVars, body, evars, handler) =>
        "try" <+> nest(arg) <+> "of" <+> block(
          bodyVars <+> "->" <+> body
        ) <@> "catch {" <@> indent(
          evars <+> "->" <+> handler
        ) <> line <> "}" <> line
      case CTuple(_, elems)  => braces(elems)
      case CValues(_, elems) => "<" <> elems <> ">"
      case e: CVar           => Show.show(e)
      // $COVERAGE-OFF$
      case _: C___XXX => sys.error(s"unexpected $e")
      // $COVERAGE-ON$
    }

  def doc(data: EObject): Doc =
    data match {
      case EAtom(atom) => atom
      case EBitStr(bin, pad_bits) =>
        val binDocs = bin.map(_.toString).map(text)
        "<<" <> nest(hsep(binDocs)) <+> s"pad($pad_bits)" <> ">>"
      case EDouble(d)                      => d.toString
      case EExternalFun(RemoteId(m, f, a)) => s"$m:$f/$a"
      case EList(elems, lastTail) =>
        val improper: Doc = lastTail match {
          case Some(tl) => space <> "| improper" <> parens(tl)
          case None     => emptyDoc
        }
        "[" <> parens(elems) <> improper <> "]"
      case ELong(value)                                                      => value.toString
      case data: EMap                                                        => Show.show(data)
      case EString(str)                                                      => str
      case ETuple(elems)                                                     => braces(elems)
      case _: Anno | _: EPid | _: EPort | _: ERef | _: C___XXX | _: CReceive =>
        // $COVERAGE-OFF$
        sys.error(s"unexpected $data")
      // $COVERAGE-ON$
    }

  implicit def cerlToDoc(e: CErl): Doc =
    if (errors.contains(e.nodeId)) s"¦⊢${e.nodeId}⊣¦" <> doc(e)
    else doc(e)

  implicit def cerlsToDoc(es: Iterable[CErl]): Doc = docArgs(es map doc)

  implicit def datasToDoc(datas: Iterable[EObject]): Doc =
    nest(hsep(datas.toList map dataToDoc))

  implicit def dataToDoc(data: EObject): Doc = doc(data)

  def docArgs(
      es: Iterable[Doc]
  ): Doc =
    if (es.isEmpty) { emptyDoc }
    else nest(hsep(es.toList, comma))

  def block(doc: Doc) =
    group("{" <+> nest(group(line <> doc)) <@> "}")
}
