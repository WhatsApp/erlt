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

class AstChecks(val context: Context) {
  import Ast._

  def check(program: Program): Unit = {
    checkUniqueFuns(program)
    checkUniqueTypes(program)
    try {
      program.typeAliases.foreach { checkTypeAlias(program, _) }
      program.opaques.foreach { checkOpaque(program, _) }
      program.enumDefs.foreach { checkEnumDef(program, _) }
      program.structDefs.foreach { checkStructDef(program, _) }
    } catch {
      case Cycle(name) =>
        val typeAlias = program.typeAliases.find(_.name == name).get
        throw new CyclicTypeAlias(typeAlias.r, name)
    }
    checkSpecs(program)
  }

  def checkPublicSpecs(program: Program): Unit = {
    val speced: Set[VarName] =
      program.specs.map(_.name).toSet
    val exported: Set[VarName] =
      program.exports.map { e => new LocalFunName(e._1, e._2) }
    val unspeced: Set[VarName] =
      exported -- speced
    for (f <- program.funs)
      if (unspeced(f.name)) throw new UnSpecedExportedFun(f.r, f.name.stringId)
  }

  private def checkSpecs(program: Program): Unit =
    program.specs.foreach { spec =>
      expandType(program, Set.empty)(spec.funType)
    }

  def checkUniqueTypes(program: Program): Unit = {
    var typeNames = Set.empty[String]
    for (ta <- program.typeAliases) {
      if (typeNames(ta.name)) throw new DuplicateType(ta.r, ta.name)
      typeNames = typeNames + ta.name
    }
    for (ed <- program.enumDefs) {
      if (typeNames(ed.name)) throw new DuplicateType(ed.r, ed.name)
      typeNames = typeNames + ed.name
    }
    for (op <- program.opaques) {
      if (typeNames(op.name)) throw new DuplicateType(op.r, op.name)
      typeNames = typeNames + op.name
    }
    var recNames = Set.empty[String]
    for (recDef <- program.structDefs) {
      if (recNames(recDef.name)) throw new DuplicateRecord(recDef.r, recDef.name)
      recNames = recNames + recDef.name
    }
  }

  def checkUniqueFuns(program: Program): Unit = {
    var funNames = Set.empty[String]
    for (fun <- program.funs) {
      if (funNames(fun.name.stringId)) throw new DuplicateFun(fun.r, fun.name.stringId)
      funNames = funNames + fun.name.stringId
    }
  }

  private def checkUsage(bound: List[TypeVar], used: Set[TypeVar]): Unit =
    for (b <- bound if !used(b)) throw new UselessTypeVar(b.r, b.name)

  private def collectParams(vars: List[TypeVar]): Set[TypeVar] = {
    var result = Set.empty[TypeVar]
    for (v <- vars) {
      if (result(v)) {
        throw new DuplicateTypeVar(v.r, v.name)
      }
      result = result + v
    }
    result
  }

  // TODO - move to erlT compiler in the first place.
  // The following conditions should hold for a well-formed type alias:
  // - All type params on RHS should be used in LHS (no "useless type param")
  // - All type variable on RHS should be bound (no "unbound type var")
  // - The type alias should not have cycles: ie - it is possible to fully expand it.
  //   See `expandType` implementation - it tries to expand the RHS of the type alias
  //   recursively, tracking all the already expanded aliases, - if it encounters
  //   an already expanded alias, it reports an error.
  // - RHS cannot have wild type variables (`_`) - this is a difference from erl1!
  private def checkTypeAlias(program: Program, alias: TypeAlias): Unit = {
    val tp = UserType(LocalName(alias.name), alias.params)(Doc.ZRange)
    val bound = collectParams(alias.params)
    val used = collectTypeVars(bound)(alias.body)
    checkUsage(alias.params, used)
    expandType(program, Set.empty)(tp)
  }

  private def checkOpaque(program: Program, opaque: Opaque): Unit = {
    val tp = UserType(LocalName(opaque.name), opaque.params)(Doc.ZRange)
    val bound = collectParams(opaque.params)
    val used = collectTypeVars(bound)(opaque.body)
    checkUsage(opaque.params, used)
    expandType(program, Set.empty)(tp)
  }

  private def checkEnumDef(program: Program, enumDef: EnumDef): Unit = {
    checkUniqueCons(enumDef.cons)
    val bound = collectParams(enumDef.params)
    val used = enumDef.cons
      .map { con =>
        con.argTypes.map(collectTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      }
      .foldLeft(Set.empty[TypeVar])(_ ++ _)
    checkUsage(enumDef.params, used)
    enumDef.cons.foreach { con => con.argTypes.foreach(expandType(program, Set.empty)) }
  }

  private def checkUniqueCons(cons: List[EnumCon]): Unit = {
    var defined = Set.empty[String]
    for (con <- cons) {
      if (defined(con.name)) {
        throw new DuplicateEnumCon(con.r, con.name)
      }
      defined = defined + con.name
    }
  }

  private def checkStructDef(program: Program, recordDef: StructDef): Unit = {
    var fieldNames = Set.empty[String]
    for (f <- recordDef.fields) {
      if (fieldNames(f.label)) {
        throw new DuplicateFields(f.r, List(f.label))
      }
      fieldNames = fieldNames + f.label
    }
    recordDef.fields.foreach { f =>
      expandType(program, Set.empty)(f.value)
      collectTypeVars(Set.empty)(f.value)
    }
  }

  private def expandType(program: Program, visited: Set[LocalName])(tp: Type): Unit =
    tp match {
      case UserType(name, params) if context.opaques(TypeId(name, params.size)) =>
        params.foreach(expandType(program, visited))
      case UserType(name: LocalName, params) =>
        program.enumDefs.find(e => e.name == name.stringId && e.params.size == params.size) match {
          case Some(_) =>
            params.foreach(expandType(program, visited))
          case None =>
            if (visited(name)) {
              throw Cycle(name.name)
            }
            program.typeAliases.find(a => a.name == name.name && a.params.size == params.size) match {
              case Some(alias) =>
                expandType(program, visited + name)(alias.body)
                params.foreach(expandType(program, visited))
              case None =>
                program.opaques.find(a => a.name == name.name && a.params.size == params.size) match {
                  case Some(opaque) =>
                    expandType(program, visited + name)(opaque.body)
                    params.foreach(expandType(program, visited))
                  case None =>
                    throw new UnknownType(tp.r, name.stringId, params.size)
                }
            }
        }
      case UserType(name: RemoteName, params) =>
        context.enumDefs.find(_.name == name.stringId) match {
          case Some(_) =>
            params.foreach(expandType(program, visited))
          case None =>
            context.aliases.find(a => a.name == name.stringId && a.params.size == params.size) match {
              case Some(alias) =>
                expandType(program, visited)(alias.body)
                params.foreach(expandType(program, visited))
              case None =>
                throw new UnknownType(tp.r, name.stringId, params.size)
            }
        }
      case _: TypeVar =>
      // OK
      case WildTypeVar() =>
      // OK
      case TupleType(params) =>
        params.foreach(expandType(program, visited))
      case ShapeType(fields) =>
        val types = fields.map(_.value)
        types.foreach(expandType(program, visited))
      case OpenShapeType(fields, rt) =>
        val types = fields.map(_.value)
        types.foreach(expandType(program, visited))
        expandType(program, visited)(rt)
      case FunType(argTypes, resType) =>
        argTypes.foreach(expandType(program, visited))
        expandType(program, visited)(resType)
      case ListType(elemType) =>
        expandType(program, visited)(elemType)
      case StructType(name) =>
        program.structDefs.find(_.name == name) match {
          case Some(eRec) =>
            eRec.kind match {
              case ExnStruct =>
                throw new ExceptionType(tp.r, name)
              case MsgStruct =>
                throw new MessageType(tp.r, name)
              case StrStruct =>
              // OK
            }
          case None =>
            throw new UnknownStruct(tp.r, name)
        }
    }

  private def collectTypeVars(bound: Set[TypeVar])(t: Type): Set[TypeVar] =
    t match {
      case t: TypeVar =>
        if (!bound(t)) {
          throw new UnboundTypeVariable(t.r, t.name)
        }
        Set(t)
      case WildTypeVar() =>
        throw new IllegalWildTypeVariable(t.r)
      case UserType(_, args) =>
        args.map(collectTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case TupleType(args) =>
        args.map(collectTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case ShapeType(fields) =>
        fields.map(f => collectTypeVars(bound)(f.value)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case OpenShapeType(fields, _) =>
        sys.error("Unexpected to see it here")
      case FunType(args, res) =>
        (res :: args).map(collectTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case ListType(elemType) =>
        collectTypeVars(bound)(elemType)
      case StructType(_) =>
        Set.empty
    }
}
