package com.whatsapp.eqwalizer

import erlang.forms.AbstractForm._

import scala.collection.mutable.ListBuffer

object ModuleApi {
  def apply(forms: List[AbstractForm]): ModuleApi = {
    var module: String = null
    var behaviours: ListBuffer[String] = ListBuffer()
    val exports: ListBuffer[IdWithArity] = ListBuffer()
    var imports: Map[IdWithArity, String] = Map.empty
    val exportTypes: ListBuffer[IdWithArity] = ListBuffer()
    var records: ListBuffer[AF_RecordDecl] = ListBuffer()
    var specs: ListBuffer[AF_FunctionSpec] = ListBuffer()
    var types: ListBuffer[AF_TypeDecl] = ListBuffer()
    for (f <- forms)
      f match {
        case m: AF_Module       => module = m.name
        case b: AF_Behaviour    => behaviours += b.name
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
      behaviours.toList,
      exports.toList.sorted,
      imports,
      exportTypes.toList.sorted,
      records.toList.sortBy(_.name).map(TypeUtils.globalizeRecord(module, _)),
      specs.toList.sortBy(_.id).map(TypeUtils.globalizeSpec(module, _)),
      types.toList.sortBy(t => (t.typeName, t.params.size)).map(TypeUtils.globalizeTypeDecl(module, _)),
    )
  }
}

case class ModuleApi(
    module: String,
    behaviours: List[String],
    exports: List[IdWithArity],
    imports: Map[IdWithArity, String],
    exportTypes: List[IdWithArity],
    records: List[AF_RecordDecl],
    specs: List[AF_FunctionSpec],
    types: List[AF_TypeDecl],
)
