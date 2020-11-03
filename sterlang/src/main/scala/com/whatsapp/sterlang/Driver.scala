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

trait Driver {
  def loadProgram(file: String): Ast.Program
  def loadContext(mainFile: String, program: Ast.Program, vars: Vars): Context
  def errorString(inputPath: String, inputContent: String, error: RangedError): String
  def parseErrorString(inputPath: String, inputContent: String, error: ParseError): String
}
