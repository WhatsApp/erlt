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

object Ast {

  sealed trait BoolConn
  case object And extends BoolConn
  case object Or extends BoolConn
  case object Xor extends BoolConn

  sealed trait ListConn
  case object `++` extends ListConn
  case object `--` extends ListConn

  sealed trait CmpOp
  case object Eq extends CmpOp
  case object LtEq extends CmpOp
  case object Lt extends CmpOp
  case object GtEq extends CmpOp
  case object Gt extends CmpOp
  case object NEq extends CmpOp

  sealed trait ArithOp
  case object Plus extends ArithOp
  case object Minus extends ArithOp
  case object Times extends ArithOp
  case object Div extends ArithOp
  case object Rem extends ArithOp
  case object BAnd extends ArithOp
  case object BOr extends ArithOp
  case object BXor extends ArithOp
  case object BSL extends ArithOp
  case object BSR extends ArithOp

  sealed trait BinOp
  case class BoolConnOp(boolConn: BoolConn) extends BinOp
  case class Cmp(cmpOp: CmpOp) extends BinOp
  case class Arith(arithOp: ArithOp) extends BinOp
  case class ListOp(listConn: ListConn) extends BinOp

  sealed trait UOp
  case object UMinus extends UOp
  case object UPlus extends UOp
  case object UNot extends UOp
  case object BNot extends UOp

  val unOps1: Map[String, UOp] =
    Map("not" -> UNot, "bnot" -> BNot)
  val unOps2: Map[String, UOp] =
    Map("-" -> UMinus, "+" -> UPlus)
  val binOps1: Map[String, BinOp] =
    Map(
      "orelse" -> BoolConnOp(Or),
      "or" -> BoolConnOp(Or),
      "andalso" -> BoolConnOp(And),
      "and" -> BoolConnOp(And),
      "xor" -> BoolConnOp(Xor),
      "div" -> Arith(Div),
      "rem" -> Arith(Rem),
      "band" -> Arith(BAnd),
      "bor" -> Arith(BOr),
      "bxor" -> Arith(BXor),
      "bsl" -> Arith(BSL),
      "bsr" -> Arith(BSR),
    )
  val binOps2: Map[String, BinOp] =
    Map(
      "/" -> Arith(Div),
      "*" -> Arith(Times),
      "-" -> Arith(Minus),
      "+" -> Arith(Plus),
      "<>" -> Cmp(NEq),
      "=/=" -> Cmp(NEq),
      "/=" -> Cmp(NEq),
      "==" -> Cmp(Eq),
      "=:=" -> Cmp(Eq),
      "<=" -> Cmp(LtEq),
      "=<" -> Cmp(LtEq),
      "<" -> Cmp(Lt),
      ">=" -> Cmp(GtEq),
      ">" -> Cmp(Gt),
      "++" -> ListOp(`++`),
      "--" -> ListOp(`--`),
    )

  sealed trait Lang
  case object ST extends Lang
  case object FFI extends Lang
  case class TypeId(name: Name, arity: Int)
  sealed trait Name {
    val stringId: String
  }
  case class LocalName(name: String) extends Name {
    override val stringId: String = name
  }
  case class RemoteName(module: String, name: String) extends Name {
    override val stringId: String = module + ":" + name
  }

  sealed trait VarName {
    val stringId: String
    override def equals(obj: Any): Boolean =
      obj match {
        case vn: VarName => this.stringId == vn.stringId
        case _           => false
      }
    override def toString: String = stringId
  }
  class LocalVarName(private val name: String) extends VarName {
    override val stringId: String = name
  }
  class LocalFunName(val name: String, val arity: Int) extends VarName {
    override val stringId: String = s"$name/$arity"
  }
  class RemoteFunName(val module: String, val name: String, val arity: Int) extends VarName {
    override val stringId: String = s"$module:$name/$arity"
  }

  sealed trait Type { val p: Pos.P }
  case class WildTypeVar()(val p: Pos.P) extends Type
  case class TypeVar(name: String)(val p: Pos.P) extends Type
  case class TupleType(params: List[Type])(val p: Pos.P) extends Type
  case class RecordType(fields: List[Field[Type]])(val p: Pos.P) extends Type
  case class OpenRecordType(fields: List[Field[Type]], extType: WildTypeVar)(val p: Pos.P) extends Type
  case class FunType(argTypes: List[Type], resType: Type)(val p: Pos.P) extends Type
  case class ListType(elemType: Type)(val p: Pos.P) extends Type
  case class UserType(name: Name, params: List[Type])(val p: Pos.P) extends Type

  case class Spec(name: VarName, funType: FunType)(val p: Pos.P)

  case class TypeAlias(name: String, params: List[TypeVar], body: Type)(val p: Pos.P)
  case class Opaque(name: String, params: List[TypeVar], body: Type)(val p: Pos.P)
  case class EnumDef(name: String, params: List[TypeVar], cons: List[EnumCon])(val p: Pos.P)
  case class EnumCon(name: String, argTypes: List[Type])(val p: Pos.P)
  case class Require(modules: List[String])

  sealed trait Exp { val p: Pos.P }
  case class BlockExpr(body: Body)(val p: Pos.P) extends Exp
  case class IfExp(exp1: Exp, exp2: Exp, exp3: Exp)(val p: Pos.P) extends Exp
  case class RecordUpdateExp(exp: Exp, delta: RecordExp)(val p: Pos.P) extends Exp
  case class BinOpExp(binOp: BinOp, exp1: Exp, exp2: Exp)(val p: Pos.P) extends Exp
  case class UOpExp(uOp: UOp, exp: Exp)(val p: Pos.P) extends Exp
  case class AppExp(head: Exp, args: List[Exp])(val p: Pos.P) extends Exp
  case class SelExp(exp: Exp, label: String)(val p: Pos.P) extends Exp
  case class BoolExp(bool: Boolean)(val p: Pos.P) extends Exp
  case class NumberExp(n: Int)(val p: Pos.P) extends Exp
  case class CharExp(c: String)(val p: Pos.P) extends Exp
  case class StringExp(s: String)(val p: Pos.P) extends Exp
  case class VarExp(v: VarName)(val p: Pos.P) extends Exp
  case class RecordExp(fields: List[Field[Exp]])(val p: Pos.P) extends Exp
  case class TupleExp(elems: List[Exp])(val p: Pos.P) extends Exp
  case class EnumConExp(enumName: Name, dataCon: String, args: List[Exp])(val p: Pos.P) extends Exp
  case class ListExp(elems: List[Exp])(val p: Pos.P) extends Exp
  case class ConsExp(head: Exp, tail: Exp)(val p: Pos.P) extends Exp
  case class CaseExp(selector: Exp, rules: List[Rule])(val p: Pos.P) extends Exp
  case class NamedFnExp(name: LocalVarName, clauses: List[Clause])(val p: Pos.P) extends Exp
  case class FnExp(clauses: List[Clause])(val p: Pos.P) extends Exp

  case class Body(prelude: List[ValDef], main: ValDef)
  case class ValDef(pat: Pat, exp: Exp)
  case class Fun(name: LocalFunName, clauses: List[Clause])(val p: Pos.P)

  case class Field[A](label: String, value: A)
  case class Rule(pat: Pat, guard: Option[Exp], exp: Body)
  case class Clause(pats: List[Pat], guard: Option[Exp], exp: Body)

  sealed trait Pat { val p: Pos.P }
  case class WildPat()(val p: Pos.P) extends Pat
  case class VarPat(v: String)(val p: Pos.P) extends Pat
  case class TuplePat(pats: List[Pat])(val p: Pos.P) extends Pat
  case class RecordPat(fields: List[Field[Pat]], open: Boolean)(val p: Pos.P) extends Pat
  case class AndPat(p1: Pat, p2: Pat)(val p: Pos.P) extends Pat
  case class EnumCtrPat(enumName: Name, conLabel: String, pats: List[Pat])(val p: Pos.P) extends Pat
  case class ListPat(pats: List[Pat])(val p: Pos.P) extends Pat
  case class ConsPat(hPat: Pat, tPat: Pat)(val p: Pos.P) extends Pat
  case class BoolPat(bool: Boolean)(val p: Pos.P) extends Pat
  case class NumberPat(n: Int)(val p: Pos.P) extends Pat
  case class StringPat(s: String)(val p: Pos.P) extends Pat

  case class Program(
      lang: Lang,
      module: String,
      require: Require,
      enumDefs: List[EnumDef],
      typeAliases: List[TypeAlias],
      opaques: List[Opaque],
      specs: List[Spec],
      exports: Set[(String, Int)],
      imports: Map[LocalFunName, RemoteFunName],
      exportTypes: Set[(String, Int)],
      importTypes: Map[LocalFunName, RemoteFunName],
      funs: List[Fun],
  ) {
    val typeMap: Map[LocalName, RemoteName] =
      importTypes.map { case (k, v) => LocalName(k.name) -> RemoteName(v.module, v.name) }
  }

  // "High-level" program element
  sealed trait ProgramElem
  case class RequireElem(modules: List[String]) extends ProgramElem
  case class FunElem(fun: Fun) extends ProgramElem
  case class SpecElem(spec: Spec) extends ProgramElem
  case class LangElem(mods: List[String]) extends ProgramElem
  case class ModuleElem(module: String) extends ProgramElem
  case class ExportElem(ids: List[(String, Int)]) extends ProgramElem
  case class ImportElem(module: String, ids: List[LocalFunName]) extends ProgramElem
  case class ImportTypeElem(module: String, ids: List[LocalFunName]) extends ProgramElem
  case class ExportTypeElem(ids: List[(String, Int)]) extends ProgramElem
  case class EnumElem(enumDef: EnumDef) extends ProgramElem
  case class TypeAliasElem(typeAlias: TypeAlias) extends ProgramElem
  case class OpaqueElem(opaque: Opaque) extends ProgramElem
  case class CompileElem(options: List[String]) extends ProgramElem

  case class RawProgram(elems: List[ProgramElem]) {
    def program: Program = {
      val mods = elems.find { _.isInstanceOf[LangElem] }.get.asInstanceOf[LangElem].mods.toSet
      val lang: Lang =
        if (mods == Set("erl2", "st")) ST
        else if (mods == Set("erl2", "ffi")) FFI
        else sys.error("unexpected mode")
      Program(
        lang,
        module = elems.find { _.isInstanceOf[ModuleElem] }.get.asInstanceOf[ModuleElem].module,
        require = Require(elems.collect { case e: RequireElem => e.modules }.flatten.distinct),
        enumDefs = elems.collect { case e: EnumElem => e.enumDef },
        typeAliases = elems.collect { case e: TypeAliasElem => e.typeAlias },
        opaques = elems.collect { case e: OpaqueElem => e.opaque },
        specs = elems.collect { case e: SpecElem => e.spec },
        exports = elems.collect { case e: ExportElem => e.ids }.flatten.toSet,
        imports = elems
          .collect {
            case i: ImportElem => i.ids.map(id => id -> new RemoteFunName(i.module, id.name, id.arity))
          }
          .flatten
          .toMap,
        exportTypes = elems.collect { case e: ExportTypeElem => e.ids }.flatten.toSet,
        importTypes = elems
          .collect {
            case i: ImportTypeElem => i.ids.map(id => id -> new RemoteFunName(i.module, id.name, id.arity))
          }
          .flatten
          .toMap,
        funs = elems.collect { case e: FunElem => e.fun },
      )
    }
  }
}
