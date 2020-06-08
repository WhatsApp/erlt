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

package com.whatsapp.analyzer

object CodeDirs {
  /**
   * A list of ebin directories of local OTP installation.
   * Something like:
   * {{{
   * val otpEbinDirs: List[String] = List(
   *     "/usr/local/Cellar/erlang/22.3.4/lib/erlang/lib/asn1-5.0.12/ebin",
   *     "/usr/local/Cellar/erlang/22.3.4/lib/erlang/lib/common_test-1.18.2/ebin",
   *     ...
   *  )
   * }}}
   */
  val otpEbinDirs: List[String] =
    sys.error("populate the list")

  /**
   * A list of ebin directories of an application being analyzed.
   * The application should be build with debug info, so that abstract forms
   * can be restored from beam files.
   * Something like:
   * {{{
   * val projectEbinDirs: List[String] = List(
   *     "/Users/batman/myapp/_build/test/lib/lib1/ebin",
   *     "/Users/batman/myapp/_build/test/lib/lib2/ebin",
   * }}}
   */
  val projectEbinDirs: List[String] =
    sys.error("populate the list")

  /**
   * A list of names of third-party libs of an application being analyzed.
   * These libs are generally excluded from analysis.
   * Something like:
   * {{{
   * val thirdParty: List[String] = List(
   *     "meck",
   *     "yaws",
   *     "proper",
   *  )
   * }}}
   */
  val thirdParty: List[String] =
    sys.error("populate the list")
}
