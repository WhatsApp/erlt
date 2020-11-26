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

import java.nio.file.{Files, Paths}

import scala.io.Source
import scala.util.Using

object CodeDirs {

  lazy val root: String =
    Using.resource(Source.fromFile("root")) { _.getLines().toList.head }

  lazy val projectEbinDirs: List[String] = {
    val rawPaths = Using.resource(Source.fromFile("paths")) { _.getLines().toList.head }
    rawPaths.split(" ").toList.distinct.sorted
  }

  lazy val thirdParty: List[String] = {
    val rawPaths = Using.resource(Source.fromFile("third_party")) { _.getLines().toList.headOption.getOrElse("") }
    rawPaths.split(" ").toList.distinct.sorted
  }

  /** Paths in [[projectEbinDirs]] minus those that belong to [[thirdParty]] libraries. */
  lazy val firstPartyEbinDirs: List[String] =
    projectEbinDirs.filter(d => !thirdParty.contains(libraryName(d)))

  /** Returns the app or library name given the directory name. */
  def libraryName(dir: String): String =
    dir match {
      case libraryNameRegex(name) => name
    }

  private val libraryNameRegex = "/lib/(.*)/".r.unanchored

  def isGenerated(appName: String, moduleName: String): Boolean = {
    val erlFile = s"$root/$appName/src/$moduleName.erl"
    if (Files.exists(Paths.get(erlFile))) {
      val src = Source.fromFile(erlFile)
      val line = src.getLines.take(1).toList.head
      src.close
      line.contains("@generated")
    } else {
      false
    }
  }
}
