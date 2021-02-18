package com.whatsapp.eqwalizer.ast

// These are diagnostics about work in progress:
// What is supported and what is not supported yet.
// Later - when all the constructs are supported,
// these items would not be needed at all.
object WIPDiagnostics {

  case class SkippedConstructDiagnostics(line: Int, construct: SkippedConstruct) extends Exception

  sealed trait SkippedConstruct
  sealed trait SkippedExpr extends SkippedConstruct
  sealed trait SkippedPat extends SkippedConstruct
  sealed trait SkippedType extends SkippedConstruct
  sealed trait SkippedGuard extends SkippedConstruct
  case object NotRequested extends SkippedConstruct
  case object TypeFunAnyArg extends SkippedType {
    override def toString: String = "T: fun((...) -> Type)"
  }
  case class RefinedRecord(name: String) extends SkippedType {
    override def toString: String = s"T: #$name{_::_}"
  }
  case class TypeUnOp(op: String) extends SkippedType {
    override def toString: String = s"T: $op _"
  }
  case class TypeBinOp(op: String) extends SkippedType {
    override def toString: String = s"T: _ $op _"
  }
  case class TypePredefined(name: String) extends SkippedType {
    override def toString: String = s"T: $name()"
  }
  case object TypeIntersection extends SkippedType {
    override def toString: String = "Intersection"
  }
  case object BadMapKey extends SkippedType {
    override def toString: String = "Bad map key"
  }
  case object PatListConcat extends SkippedPat {
    override def toString: String = "P: _ ++ _"
  }
  case object PatString extends SkippedPat {
    override def toString: String = """P: "...""""
  }
  case object ExpString extends SkippedExpr {
    override def toString: String = """E: "...""""
  }
  case object ExpDynFun extends SkippedExpr {
    override def toString: String = "E: dynfun"
  }
  case object ExpDCall extends SkippedExpr {
    override def toString: String = "E: dcall"
  }
  case object ExpAnonFun extends SkippedExpr {
    override def toString: String = "E: lambda"
  }
  case object ExpNamedFun extends SkippedExpr {
    override def toString: String = "E: named_fun"
  }
  case object ExpListConcat extends SkippedExpr {
    override def toString: String = "E: _ ++ _"
  }
  case object ExpListSubtract extends SkippedExpr {
    override def toString: String = "E: _ -- _"
  }
  case object TestString extends SkippedExpr {
    override def toString: String = """G: "...""""
  }

  sealed trait ExpansionFailure {
    def diag: String
  }
  case class RecursiveType(id: RemoteId) extends Exception with ExpansionFailure {
    def diag: String = s"R: $id"
  }
  case class UnknownId(id: RemoteId) extends Exception with ExpansionFailure {
    def diag: String = s"U: $id"
  }
  case class RecursiveConstraint(n: String) extends Exception with ExpansionFailure {
    def diag: String = s"C: $n"
  }
}
