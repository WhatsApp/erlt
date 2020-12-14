package com.whatsapp.eqwalizer

import erlang.forms.AbstractForm._

import scala.collection.mutable.ListBuffer

object ModuleApi {
  def apply(forms: List[AbstractForm]): ModuleApi = {
    var module: AF_Module = null
    var behaviour: Option[AF_Behaviour] = None
    val exports: ListBuffer[IdWithArity] = ListBuffer()
    var imports: Map[IdWithArity, String] = Map.empty
    val exportTypes: ListBuffer[IdWithArity] = ListBuffer()
    var records: ListBuffer[AF_RecordDecl] = ListBuffer()
    var specs: ListBuffer[AF_FunctionSpec] = ListBuffer()
    var types: ListBuffer[AF_TypeDecl] = ListBuffer()
    for (f <- forms)
      f match {
        case m: AF_Module       => module = m
        case b: AF_Behaviour    => behaviour = Some(b)
        case e: AF_Export       => exports ++= e.funs
        case i: AF_Import       => imports ++= i.funs.map(_ -> i.module)
        case e: AF_ExportType   => exportTypes ++= e.types
        case r: AF_RecordDecl   => records += r
        case t: AF_TypeDecl     => types += t
        case s: AF_FunctionSpec => specs += s
        case _                  =>
      }
    ModuleApi(
      module,
      behaviour,
      exports.toList.sorted,
      imports,
      exportTypes.toList.sorted,
      records.toList.sortBy(_.name),
      specs.toList.sortBy(_.id),
      types.toList.sortBy(t => (t.typeName, t.params.size)),
    )
  }
}

case class ModuleApi(
    module: AF_Module,
    behaviour: Option[AF_Behaviour],
    exports: List[IdWithArity],
    imports: Map[IdWithArity, String],
    exportTypes: List[IdWithArity],
    records: List[AF_RecordDecl],
    specs: List[AF_FunctionSpec],
    types: List[AF_TypeDecl],
)
