package com.whatsapp.sterlang

trait EtfApi {
  def programFromFile(path: String): Ast.Program
  def moduleApiFromFile(path: String): ModuleApi
}
