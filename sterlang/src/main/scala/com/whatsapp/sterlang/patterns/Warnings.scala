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

package com.whatsapp.sterlang.patterns

import com.whatsapp.sterlang.{Doc, RangedError, Severity, Warning}

sealed trait PatternWarning extends RangedError {
  override val severity: Severity = Warning
}

final class MissingPatternsWarning(range: Doc.Range, confident: Boolean, exampleClause: PatternChecker.Vector)
    extends RangedError(
      range = range,
      title = (if (confident) "" else "Possibly ") + "Missing Patterns",
      description = Some(s"missing: ${exampleClause.map(Pattern.show).mkString(sep = ", ")}"),
    )
    with PatternWarning

final class UselessPatternWarning(range: Doc.Range)
    extends RangedError(range = range, title = "Useless Pattern", description = None)
    with PatternWarning
