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

package com.whatsapp.analyzer.test

import com.whatsapp.analyzer.RPC
import erlang.Data.EList
import erlang.forms.AbstractFormConvert

object LoadForms {
  def main(args: Array[String]): Unit = {

    val beamFilePath = args(0)
    val rpc = RPC.connect()
    val rawForms =
      try {
        rpc.getForms(beamFilePath)
      } finally {
        rpc.close()
      }

    val absForms = rawForms match {
      case EList(elems, None) =>
        elems.map(AbstractFormConvert.convertForm(_, lite = false))
      case _ =>
        sys.error("wrong forms")
    }
    pprint.pprintln(absForms, height = 1000)
  }
}
