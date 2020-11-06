package com.whatsapp.sterlang.test

import com.whatsapp.sterlang._

class UnifySpec extends org.scalatest.flatspec.AnyFlatSpec {
  val vars = new Vars()
  val tu = new TypesUtil(vars)
  val unify = new Unify(vars)

  private def freshRowTypeVar(d: Int, kind: Types.RtVarKind = Types.empty): Types.RowType =
    Types.RowVarType(vars.rVar(Types.RowOpen(d, kind)))

  "Unify" should "unify a type with itself" in {
    val numberType = METypes.NumberType
    unify.unify(numberType, numberType)
  }

  "it" should "throw TyConMismatch when matching different constructors" in {
    val numberType = METypes.NumberType
    val stringType = METypes.StringType
    assertThrows[TyConMismatch] {
      unify.unify(numberType, stringType)
    }
  }

  "it" should "throw FieldMismatch when matching incorrect fields" in {
    val emptyShape: Types.RowType =
      Types.RowEmptyType

    val shapeWithIdInt =
      METypes.ShapeType(Types.RowFieldType(Types.Field("id", METypes.NumberType), emptyShape))
    val shapeWithNameString =
      METypes.ShapeType(Types.RowFieldType(Types.Field("name", METypes.StringType), emptyShape))

    assertThrows[FieldMismatch] {
      unify.unify(shapeWithIdInt, shapeWithNameString)
    }
  }

  "it" should "throw RowCircularity errors when there is circularity among row types" in {
    val d = 0

    assertThrows[RowCircularity.type] {
      val rtv1 = freshRowTypeVar(d, Types.single("id"))
      val shape = METypes.ShapeType(rtv1)
      val shapeWithIdInt1 =
        METypes.ShapeType(Types.RowFieldType(Types.Field("id", shape), Types.RowEmptyType))

      unify.unify(shape, shapeWithIdInt1)
    }

    assertThrows[RowCircularity.type] {
      val rtv1 = freshRowTypeVar(d, Types.single("id"))
      val shape = METypes.ShapeType(rtv1)
      val shapeWithIdInt1 =
        METypes.ShapeType(Types.RowFieldType(Types.Field("id", shape), Types.RowEmptyType))

      unify.unify(shapeWithIdInt1, shape)
    }
  }
}
