package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Forms.RecDecl
import com.whatsapp.eqwalizer.ast.{DB, Id, RemoteId}
import com.whatsapp.eqwalizer.ast.Types.{AnyType, FunType}

object Util {
  def getFunType(module: String, id: Id): Option[FunType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(module)
      hostModule = moduleStub.imports.getOrElse(id, module)
      ft <- getFunType(RemoteId(hostModule, id.name, id.arity))
    } yield ft

  def getRecord(module: String, name: String): Option[RecDecl] =
    for {
      moduleStub <- DB.getExpandedModuleStub(module)
      recRecl <- moduleStub.records.get(name)
    } yield recRecl

  def getFunType(fqn: RemoteId): Option[FunType] =
    for {
      moduleStub <- DB.getExpandedModuleStub(fqn.module)
      spec <- moduleStub.specs.get(Id(fqn.name, fqn.arity)) if spec.types.size == 1
    } yield spec.types.head.ty

  def enterScope(env0: Env, scopeVars: Set[String]): Env = {
    var env = env0
    for {
      v <- scopeVars if !env0.contains(v)
    } env = env.updated(v, AnyType)
    env
  }

  def exitScope(env0: Env, env1: Env, scopeVars: Set[String]): Env = {
    val allVars = env0.keySet ++ scopeVars
    env1.view.filterKeys(allVars).toMap
  }
}
