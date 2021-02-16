package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.ast.Forms._

package object ast {
  case class Id(name: String, arity: Int) {
    override def toString: String = s"$name/$arity"
  }
  case class RemoteId(module: String, name: String, arity: Int) {
    override def toString: String = s"$module:$name/$arity"
  }

  case class App(
      name: String,
      ebinDir: String,
      modules: List[String],
  )

  case class ModuleStub(
      module: String,
      exports: Set[Id],
      imports: Map[Id, String],
      exportTypes: Set[Id],
      specs: Map[Id, FunSpec],
      types: Map[Id, TypeDecl],
      records: Map[String, RecDecl],
      skippedSpecs: Map[Id, SkippedFunSpec],
      skippedTypes: Map[Id, SkippedTypeDecl],
  )
}
