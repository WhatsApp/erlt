package com.whatsapp.eqwalizer

import erlang.forms.AbstractForm._

object SourceMap {
  var maps: Map[String, SourceMap] =
    Map.empty

  def get(module: String): Option[SourceMap] =
    maps.get(module)

  def put(module: String, forms: List[AbstractForm]): Unit = {
    maps += module -> buildMap(module, forms)
  }

  def buildMap(module: String, forms: List[AbstractForm]): SourceMap = {
    var curFile: String =
      null
    var records: Map[String, String] =
      Map.empty
    var types: Map[(String, Int), String] =
      Map.empty
    for (form <- forms)
      form match {
        case AF_File(file) =>
          curFile = file
        case AF_RecordDecl(name, _) =>
          records += name -> curFile
        case AF_TypeDecl(_, typeName, _, params) =>
          types += (typeName, params.size) -> curFile
        case _ =>
      }
    SourceMap(module, records, types)
  }
}

case class SourceMap(
    module: String,
    // recordName -> file
    records: Map[String, String],
    // (name, arity) -> file
    types: Map[(String, Int), String],
)
