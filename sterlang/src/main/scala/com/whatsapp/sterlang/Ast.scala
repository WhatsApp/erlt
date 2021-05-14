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

  sealed trait StructKind
  case object StrStruct extends StructKind
  case object ExnStruct extends StructKind
  case object MsgStruct extends StructKind

  sealed trait Val
  case class AtomVal(value: String) extends Val
  case class BooleanVal(value: Boolean) extends Val
  case class NumberVal(value: Int) extends Val
  case class CharVal(value: Char) extends Val
  case class StringVal(value: String) extends Val

  val unOps1: Map[String, UOp] =
    Map("not" -> UNot, "bnot" -> BNot)
  val unOps2: Map[String, UOp] =
    Map("-" -> UMinus, "+" -> UPlus)
  val unOps: Map[String, UOp] =
    unOps1 ++ unOps2
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
  val binOps: Map[String, BinOp] =
    binOps1 ++ binOps2

  case class TypeId(name: Name)
  sealed trait Name {
    val stringId: String
  }
  // Unqualified name (global or local)
  case class UName(name: String) extends Name {
    override val stringId: String = name
  }
  // Qualified name
  case class QName(module: String, name: String) extends Name {
    override val stringId: String = module + ":" + name
  }

  // TODO - refactor this hierarchy
  sealed trait VarName {
    val stringId: String
    override def equals(obj: Any): Boolean = {
      require(obj.isInstanceOf[VarName])
      this.stringId == obj.asInstanceOf[VarName].stringId
    }
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

  type ShapeExtType = Either[WildTypeVar, TypeVar]

  sealed trait Type { val r: Doc.Range }
  case class WildTypeVar()(val r: Doc.Range) extends Type
  case class TypeVar(name: String)(val r: Doc.Range) extends Type
  case class TupleType(params: List[Type])(val r: Doc.Range) extends Type
  case class ShapeType(fields: List[LblField[Type]])(val r: Doc.Range) extends Type
  case class OpenShapeType(fields: List[LblField[Type]], extType: ShapeExtType)(val r: Doc.Range) extends Type
  case class FunType(argTypes: List[Type], resType: Type)(val r: Doc.Range) extends Type
  case class ListType(elemType: Type)(val r: Doc.Range) extends Type
  case class UserType(name: Name, params: List[Type])(val r: Doc.Range) extends Type

  case class Spec(name: VarName, funType: FunType)(val r: Doc.Range)

  case class TypeAlias(name: String, params: List[TypeVar], body: Type)(val r: Doc.Range)
  case class Opaque(name: String, params: List[TypeVar], body: Type)(val r: Doc.Range)
  case class UncheckedOpaque(name: String, params: List[TypeVar])(val r: Doc.Range)
  case class EnumDef(name: String, params: List[TypeVar], ctrs: List[EnumCtr])(val r: Doc.Range)
  case class StructDef(name: String, params: List[TypeVar], fields: List[FieldDecl], kind: StructKind)(
      val r: Doc.Range
  )
  case class EnumCtr(name: String, fields: List[FieldDecl])(val r: Doc.Range)

  sealed trait Exp { val r: Doc.Range }
  case class BlockExpr(body: Body)(val r: Doc.Range) extends Exp
  case class BinOpExp(binOp: BinOp, exp1: Exp, exp2: Exp)(val r: Doc.Range) extends Exp
  case class UOpExp(uOp: UOp, exp: Exp)(val r: Doc.Range) extends Exp
  case class AppExp(head: Exp, args: List[Exp])(val r: Doc.Range) extends Exp
  case class AtomExp(atom: String)(val r: Doc.Range) extends Exp
  case class BoolExp(bool: Boolean)(val r: Doc.Range) extends Exp
  case class NumberExp(n: Int)(val r: Doc.Range) extends Exp
  case class CharExp(c: Char)(val r: Doc.Range) extends Exp
  case class StringExp(s: String)(val r: Doc.Range) extends Exp
  case class VarExp(v: VarName)(val r: Doc.Range) extends Exp

  case class ShapeCreateExp(fields: List[LblField[Exp]])(val r: Doc.Range) extends Exp
  case class ShapeSelectExp(exp: Exp, label: String)(val r: Doc.Range) extends Exp
  case class ShapeUpdateExp(exp: Exp, fields: List[LblField[Exp]])(val r: Doc.Range) extends Exp

  case class TupleExp(elems: List[Exp])(val r: Doc.Range) extends Exp
  case class EnumExp(enumName: Name, ctr: String, fields: List[Field[Exp]])(val r: Doc.Range) extends Exp
  case class NilExp()(val r: Doc.Range) extends Exp
  case class Bin(elems: List[BinElement])(val r: Doc.Range) extends Exp
  case class ConsExp(head: Exp, tail: Exp)(val r: Doc.Range) extends Exp
  case class CaseExp(selector: Exp, rules: List[Rule])(val r: Doc.Range) extends Exp
  case class IfExp(ifClauses: List[IfClause])(val r: Doc.Range) extends Exp
  case class NamedFnExp(name: LocalVarName, clauses: List[Clause])(val r: Doc.Range) extends Exp
  case class FnExp(clauses: List[Clause])(val r: Doc.Range) extends Exp
  case class Comprehension(template: Exp, qualifiers: List[Qualifier])(val r: Doc.Range) extends Exp
  case class BComprehension(template: Exp, qualifiers: List[Qualifier])(val r: Doc.Range) extends Exp
  case class StructCreate(name: Name, fields: List[Field[Exp]])(val r: Doc.Range) extends Exp
  case class StructUpdate(struct: Exp, structName: Name, fields: List[Field[Exp]])(val r: Doc.Range) extends Exp
  case class StructSelect(struct: Exp, structName: Name, index: Index)(val r: Doc.Range) extends Exp
  case class TryCatchExp(tryBody: Body, catchRules: List[Rule], after: Option[Body])(val r: Doc.Range) extends Exp
  case class TryOfCatchExp(tryBody: Body, tryRules: List[Rule], catchRules: List[Rule], after: Option[Body])(
      val r: Doc.Range
  ) extends Exp
  case class ReceiveExp(rules: List[Rule], after: Option[AfterBody])(val r: Doc.Range) extends Exp

  case class Body(prelude: List[ValDef], main: ValDef)
  case class AfterBody(timeout: Exp, body: Body)
  case class ValDef(pat: Pat, exp: Exp)
  case class Fun(name: LocalFunName, clauses: List[Clause])(val r: Doc.Range)
  case class UncheckedFun(name: LocalFunName)

  sealed trait Field[A] {
    val value: A
    val r: Doc.Range
  }
  case class LblField[A](label: String, value: A)(val r: Doc.Range) extends Field[A]
  case class PosField[A](value: A)(val r: Doc.Range) extends Field[A]

  // struct index
  sealed trait Index
  case class LblIndex(label: String) extends Index
  case class PosIndex(pos: Int) extends Index

  sealed trait FieldDecl {
    val tp: Type
    val r: Doc.Range
  }
  case class LblFieldDecl(label: String, tp: Type, default: Option[Exp])(val r: Doc.Range) extends FieldDecl
  case class PosFieldDecl(tp: Type)(val r: Doc.Range) extends FieldDecl

  case class Rule(pat: Pat, guards: List[Guard], exp: Body)
  case class Clause(pats: List[Pat], guards: List[Guard], exp: Body)
  case class IfClause(guards: List[Guard], exp: Body)
  case class Guard(exprs: List[Exp])

  sealed trait Qualifier
  case class Filter(exp: Exp) extends Qualifier
  case class Generator(pat: Pat, exp: Exp) extends Qualifier
  case class BGenerator(pat: Pat, exp: Exp) extends Qualifier

  case class BinElement(expr: Exp, size: Option[Exp], binElemType: Option[BinElemType])

  sealed trait BinElemType
  case object IntegerBinElemType extends BinElemType
  case object FloatBinElemType extends BinElemType
  case object BinaryBinElemType extends BinElemType
  case object BytesBinElemType extends BinElemType
  case object BitstringBinElemType extends BinElemType
  case object BitsBinElemType extends BinElemType
  case object Utf8BinElemType extends BinElemType
  case object Utf16BinElemType extends BinElemType
  case object Utf32BinElemType extends BinElemType

  sealed trait Pat { val r: Doc.Range }
  case class WildPat()(val r: Doc.Range) extends Pat
  case class VarPat(v: String)(val r: Doc.Range) extends Pat
  case class PinnedVarPat(v: String)(val r: Doc.Range) extends Pat
  case class TuplePat(pats: List[Pat])(val r: Doc.Range) extends Pat
  case class ShapePat(fields: List[LblField[Pat]])(val r: Doc.Range) extends Pat
  case class AndPat(p1: Pat, p2: Pat)(val r: Doc.Range) extends Pat
  case class EnumPat(enumName: Name, ctr: String, fields: List[Field[Pat]])(val r: Doc.Range) extends Pat
  case class NilPat()(val r: Doc.Range) extends Pat
  case class BinPat(pats: List[BinElementPat])(val r: Doc.Range) extends Pat
  case class ConsPat(hPat: Pat, tPat: Pat)(val r: Doc.Range) extends Pat
  case class AtomPat(atom: String)(val r: Doc.Range) extends Pat
  case class BoolPat(bool: Boolean)(val r: Doc.Range) extends Pat
  case class CharPat(char: Char)(val r: Doc.Range) extends Pat
  case class NumberPat(n: Int)(val r: Doc.Range) extends Pat
  case class StringPat(s: String)(val r: Doc.Range) extends Pat
  case class StructPat(structName: Name, fields: List[Field[Pat]])(val r: Doc.Range) extends Pat

  case class BinElementPat(pat: Pat, size: Option[Exp], binElemType: Option[BinElemType])

  case class Program(
      module: String,
      enumDefs: List[EnumDef],
      structDefs: List[StructDef],
      typeAliases: List[TypeAlias],
      opaques: List[Opaque],
      uncheckedOpaques: List[UncheckedOpaque],
      specs: List[Spec],
      exports: Set[(String, Int)],
      imports: Map[LocalFunName, RemoteFunName],
      exportTypes: Set[(String, Int)],
      importTypes: Map[LocalFunName, RemoteFunName],
      funs: List[Fun],
      uncheckedFuns: List[UncheckedFun],
  ) {
    val typeMap: Map[UName, QName] =
      importTypes.map { case (k, v) => UName(k.name) -> QName(v.module, v.name) }
  }

  // "High-level" program element
  sealed trait ProgramElem
  case class FunElem(fun: Fun) extends ProgramElem
  case class UncheckedFunElem(fun: UncheckedFun) extends ProgramElem
  case class SpecElem(spec: Spec) extends ProgramElem
  case class ModuleElem(module: String) extends ProgramElem
  case class ExportElem(ids: List[(String, Int)]) extends ProgramElem
  case class ImportElem(module: String, ids: List[LocalFunName]) extends ProgramElem
  case class ImportTypeElem(module: String, ids: List[LocalFunName]) extends ProgramElem
  case class ExportTypeElem(ids: List[(String, Int)]) extends ProgramElem
  case class EnumElem(enumDef: EnumDef) extends ProgramElem
  case class TypeAliasElem(typeAlias: TypeAlias) extends ProgramElem
  case class StructElem(structDef: StructDef) extends ProgramElem
  case class OpaqueElem(opaque: Opaque) extends ProgramElem
  case class UncheckedOpaqueElem(uncheckedOpaque: UncheckedOpaque) extends ProgramElem
  case class CompileElem(options: List[String]) extends ProgramElem

  case class RawProgram(elems: List[ProgramElem]) {
    def program: Program =
      Program(
        module = elems.find { _.isInstanceOf[ModuleElem] }.get.asInstanceOf[ModuleElem].module,
        enumDefs = elems.collect { case e: EnumElem => e.enumDef },
        structDefs = elems.collect { case e: StructElem => e.structDef },
        typeAliases = elems.collect { case e: TypeAliasElem => e.typeAlias },
        opaques = elems.collect { case e: OpaqueElem => e.opaque },
        uncheckedOpaques = elems.collect { case e: UncheckedOpaqueElem => e.uncheckedOpaque },
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
        uncheckedFuns = elems.collect { case e: UncheckedFunElem => e.fun },
      )
  }
}
