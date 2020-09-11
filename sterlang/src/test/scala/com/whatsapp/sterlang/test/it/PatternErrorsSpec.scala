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

package com.whatsapp.sterlang.test.it

class PatternErrorsSpec extends DirSpec {

  testDir("examples/pattern-error")

  override def testFile(erlPath: String, etfPath: String): Unit = {
    if (erlPath.endsWith("core.erl")) {
      super.testFile(erlPath, etfPath)
    } else {
      assert(SterlangTestUtil.processIllPatterns(erlPath, etfPath))
    }
  }

  override def testFileVerbose(erlPath: String, etfPath: String): Unit = {
    // doing nothing
  }
}
