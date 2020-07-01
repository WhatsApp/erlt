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

import com.whatsapp.sterlang.Pos.HasSourceLocation
import com.whatsapp.sterlang.PositionedError

sealed trait PatternWarning

final class MissingPatternsWarning(node: HasSourceLocation)
    extends PositionedError(pos = node.sourceLocation, title = "Missing Patterns", description = None)
    with PatternWarning

final class UselessPatternWarning(clause: HasSourceLocation)
    extends PositionedError(pos = clause.sourceLocation, title = "Useless Pattern", description = None)
    with PatternWarning
