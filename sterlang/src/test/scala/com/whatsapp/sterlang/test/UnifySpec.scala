/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
