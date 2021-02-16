package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Show.show
import com.whatsapp.eqwalizer.ast.Types.Type

object TcDiagnostics {
  case class TcDiagnostics(line: Int, msg: String)

  sealed trait TypeError extends Exception {
    val line: Int
    val msg: String
  }
  case class TypeMismatch(line: Int, expr: Expr, expected: Type, got: Type) extends TypeError {
    override val msg: String = s"${show(expr)}. Expected: ${show(expected)}, Got: ${show(got)}"
  }
  case class UndefinedKey(line: Int, expr: Expr, key: String, got: Type) extends TypeError {
    override val msg: String = s"${show(expr)}. Undef key `$key`. Type: ${show(got)}"
  }
  case class UndefinedField(line: Int, recName: String, fieldName: String) extends TypeError {
    override val msg: String = s"#$recName{...}: $fieldName is 'undefined'"
  }
  case class UnboundVar(line: Int, n: String) extends TypeError {
    override val msg: String = s"Unbound var: ${n}"
  }
}
