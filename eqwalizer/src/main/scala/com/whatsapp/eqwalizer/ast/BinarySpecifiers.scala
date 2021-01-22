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

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

object BinarySpecifiers {
  sealed trait Specifier
  case object SignedIntegerSpecifier extends Specifier
  case object UnsignedIntegerSpecifier extends Specifier
  case object FloatSpecifier extends Specifier
  case object BinarySpecifier extends Specifier
  case object BytesSpecifier extends Specifier
  case object BitstringSpecifier extends Specifier
  case object BitsSpecifier extends Specifier
  case object Utf8Specifier extends Specifier
  case object Utf16Specifier extends Specifier
  case object Utf32Specifier extends Specifier

  def expType(s: Specifier, stringLiteral: Boolean): Types.Type =
    s match {
      case SignedIntegerSpecifier | UnsignedIntegerSpecifier | Utf8Specifier | Utf16Specifier | Utf32Specifier =>
        // $COVERAGE-OFF$
        if (stringLiteral) throw new IllegalStateException()
        // $COVERAGE-ON$
        integerType
      case FloatSpecifier =>
        floatType
      case BinarySpecifier | BytesSpecifier | BitstringSpecifier | BitsSpecifier =>
        BinaryType
    }
}
