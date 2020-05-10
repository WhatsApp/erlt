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

package com.whatsapp.sterlang

class TypePrinterUtil {
  val T = Types

  private var ftcounter = 0
  private var frcounter = 0
  private var btCounter = 0
  private var brCounter = 0
  
  def nextSchematicTypeName(): String = {
    val prefix = ""
    val n = btCounter
    btCounter = btCounter + 1
    prefix + (if (n < 26)  ('A' + n).toChar.toString else "t_" + n)
  }

  def nextSchematicRowTypeName(): String = {
    val prefix = "'"
    val n = brCounter
    brCounter = brCounter + 1
    prefix + (if (n < 26)  ('A' + n).toChar.toString else "t_" + n)
  }

  // free type
  def nextftname(): String = {
    val prefix = ""
    val n = ftcounter
    ftcounter = ftcounter + 1
    prefix + (if (n < 26) ('A' + n).toChar.toString else "T_" + n)
  }

  // row type
  def nextfrname(): String = {
    val prefix = ""
    val n = frcounter
    frcounter = frcounter + 1
    prefix + (if (n < 26) ('A' + n).toChar.toString else "T_" + n)
  }
}
