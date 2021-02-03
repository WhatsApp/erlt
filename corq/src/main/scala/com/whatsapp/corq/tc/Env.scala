package com.whatsapp.corq.tc

import com.whatsapp.corq.ast.Types.Type
import com.whatsapp.corq.tc.TcDiagnostics.UnboundVar
import erlang.CErl._

import scala.collection.IterableOnce

class Env private (
    private val module: String,
    private val store: Map[VarName, Type]
) {
  type K = VarName
  type V = Type

  // maplike
  def apply(cvar: CVar): Type =
    get(cvar).getOrElse(throw UnboundVar(cvar.anno.line, cvar))
  def get(cvar: CVar): Option[Type] =
    cvar.name match {
      case VarNameAtomInt(id) => Util.getApplyType(module, id)
      case _                  => store.get(cvar.name)
    }

  // store.get(cvar.name)
  def contains(cvar: CVar): Boolean = store.keySet.contains(cvar.name)
  def ++(xs: Iterable[(CVar, Type)]): Env =
    new Env(module, store ++ Env.from(module, xs).store)
  def +(update: (CVar, Type)): Env =
    new Env(module, store + (update._1.name -> update._2))

  override def toString = store.toString
}

object Env {
  def empty(module: String): Env = Env.from(module, Nil)
  def from(module: String, it: Iterable[(CVar, Type)]): Env = {
    val types = Map.from(it.map { case (cvar, ty) => (cvar.name, ty) })
    new Env(module, types)
  }
  def combine(
      initialEnv: Env,
      op: (Type, Type) => Type,
      envs: Iterable[Env]
  ): Env = {
    val vars = initialEnv.store.keySet
    var acc = envs.head.store
    for (v <- vars) {
      for (other <- envs map (_.store)) {
        val ty = op(acc(v), other(v))
        acc = acc + (v -> ty)
      }
    }
    new Env(initialEnv.module, acc)
  }
}
