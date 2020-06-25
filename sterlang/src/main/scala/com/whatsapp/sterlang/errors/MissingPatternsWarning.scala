package com.whatsapp.sterlang.errors

import com.whatsapp.sterlang.Pos
import com.whatsapp.sterlang.errors.CompilationMessage.CompilationWarning

// TODO: fields
final class MissingPatternsWarning extends CompilationWarning {
  override protected def category: String = "Missing Patterns"

  override protected def source: String = ""

  override protected def description: String = ""

  override protected def relevantLocations: List[Pos.Located[String]] = List()
}
