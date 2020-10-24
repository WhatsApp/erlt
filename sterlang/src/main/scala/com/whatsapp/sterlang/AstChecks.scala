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
      checkShapeExtensions(spec.funType)
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
    var structNames = Set.empty[String]
    for (structDef <- program.structDefs) {
      if (structNames(structDef.name)) throw new DuplicateStruct(structDef.r, structDef.name)
      structNames = structNames + structDef.name
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
    val used = collectRHSTypeVars(bound)(alias.body)
    checkUsage(alias.params, used)
    expandType(program, Set.empty)(tp)
  }

  private def checkOpaque(program: Program, opaque: Opaque): Unit = {
    val tp = UserType(LocalName(opaque.name), opaque.params)(Doc.ZRange)
    val bound = collectParams(opaque.params)
    val used = collectRHSTypeVars(bound)(opaque.body)
    checkUsage(opaque.params, used)
    expandType(program, Set.empty)(tp)
  }

  private def checkEnumDef(program: Program, enumDef: EnumDef): Unit = {
    checkUniqueCtrs(enumDef.ctrs)
    val bound = collectParams(enumDef.params)
    val used = enumDef.ctrs
      .map { con =>
        con.argTypes.map(collectRHSTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      }
      .foldLeft(Set.empty[TypeVar])(_ ++ _)
    checkUsage(enumDef.params, used)
    enumDef.ctrs.foreach { con => con.argTypes.foreach(expandType(program, Set.empty)) }
  }

  private def checkUniqueCtrs(ctrs: List[EnumCtr]): Unit = {
    var defined = Set.empty[String]
    for (ctr <- ctrs) {
      if (defined(ctr.name)) {
        throw new DuplicateEnumCon(ctr.r, ctr.name)
      }
      defined = defined + ctr.name
    }
  }

  private def checkStructDef(program: Program, structDef: StructDef): Unit = {
    if (structDef.params.nonEmpty) {
      if (structDef.kind == ExnStruct) {
        throw new PolymorphicException(Doc.merge(structDef.params.head.r, structDef.params.last.r))
      }
      if (structDef.kind == MsgStruct) {
        throw new PolymorphicMessage(Doc.merge(structDef.params.head.r, structDef.params.last.r))
      }
    }
    var fieldNames = Set.empty[String]
    for (f @ LblFieldDecl(label, _, _) <- structDef.fields) {
      if (fieldNames(label)) {
        throw new DuplicateFields(f.r, List(f.label))
      }
      fieldNames = fieldNames + f.label
    }
    val bound = collectParams(structDef.params)
    structDef.fields.foreach { f =>
      expandType(program, Set.empty)(f.tp)
    }
    val used = structDef.fields.map { f => collectRHSTypeVars(bound)(f.tp) }.foldLeft(Set.empty[TypeVar])(_ ++ _)
    checkUsage(structDef.params, used)
  }

  private def expandType(program: Program, visited: Set[LocalName])(tp: Type): Unit =
    tp match {
      case UserType(name: LocalName, Nil) if nativeAliases.exists(a => a.name == name.stringId) =>
      // OK - fast check for global aliases like integer() -> number
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
                    val strName = name.stringId
                    program.structDefs.find(s => s.name == strName && s.params.size == params.size) match {
                      case Some(eRec) =>
                        params.foreach(expandType(program, visited))
                        eRec.kind match {
                          case ExnStruct =>
                            throw new ExceptionType(tp.r, strName)
                          case MsgStruct =>
                            throw new MessageType(tp.r, strName)
                          case StrStruct =>
                          // OK
                        }
                      case None =>
                        throw new UnknownType(tp.r, name.stringId, params.size)
                    }
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

      case OpenShapeType(fields, _) =>
        val types = fields.map(_.value)
        types.foreach(expandType(program, visited))
      // It makes no sense to expand an extension here -
      // Since it is always either a TypeVar or a WildTypeVar

      case FunType(argTypes, resType) =>
        argTypes.foreach(expandType(program, visited))
        expandType(program, visited)(resType)
      case ListType(elemType) =>
        expandType(program, visited)(elemType)
    }

  // It collects the type vars of RHS of a definition:
  // - Type vars of a type alias
  // - Type vars of an opaque
  // - Type vars of an enum definitions (going down into constructors)
  // - Type vars of a struct
  // It collects and at the same time performs corresponding checks:
  // - There are no `_` (Wild type vars)
  // - There are no open shapes (open shapes are supported in specs only for now)
  private def collectRHSTypeVars(bound: Set[TypeVar])(t: Type): Set[TypeVar] =
    t match {
      case t: TypeVar =>
        if (!bound(t)) {
          throw new UnboundTypeVariable(t.r, t.name)
        }
        Set(t)
      case WildTypeVar() =>
        // The wild type var is not allowed on the RHS
        throw new IllegalWildTypeVariable(t.r)
      case UserType(_, args) =>
        args.map(collectRHSTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case TupleType(args) =>
        args.map(collectRHSTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case FunType(args, res) =>
        (res :: args).map(collectRHSTypeVars(bound)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case ListType(elemType) =>
        collectRHSTypeVars(bound)(elemType)
      case ShapeType(fields) =>
        fields.map(f => collectRHSTypeVars(bound)(f.value)).foldLeft(Set.empty[TypeVar])(_ ++ _)
      case OpenShapeType(_, Left(extType)) =>
        throw new RHSOpenShape(extType.r)
      case OpenShapeType(_, Right(extType)) =>
        throw new RHSOpenShape(extType.r)
    }

  // checks that
  // - Each type var is either a standard type var or a shape extension type var
  // - Shape extension type variables are used consistently
  private def checkShapeExtensions(funType: FunType): Unit = {
    val types = funType.argTypes ++ List(funType.resType)
    val vars = types.flatMap(AstUtil.collectNamedTypeVars).toSet
    val rowVars = types.flatMap(AstUtil.collectNamedRowTypeVars)

    for ((rv, _) <- rowVars) {
      if (vars(rv.name)) {
        throw new TypeVarKindConflict(rv.r, rv.name)
      }
    }

    var kinds = Map.empty[String, Set[String]]
    for ((rv, thisKind) <- rowVars) {
      kinds.get(rv.name) match {
        case None =>
          kinds = kinds + (rv.name -> thisKind)
        case Some(prevKind) =>
          if (prevKind != thisKind) {
            throw new InconsistentShapeExtension(rv.r, rv.name, prevKind.toList.sorted, thisKind.toList.sorted)
          }
      }
    }
  }
}
