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

import java.nio.file.{Files, Path, Paths}

import scala.io.Source
import scala.util.Using

object CodeDirs {

  lazy val root: String =
    Using.resource(Source.fromFile("root")) { _.getLines().toList.head }

  lazy val otpLibRoot: String =
    Using.resource(Source.fromFile("otp")) { _.getLines().toList.head }

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

  sealed trait Provenance
  case class FirstParty(sourcePath: Path) extends Provenance
  case class Generated(sourcePath: Path) extends Provenance
  case object OtpProvenance extends Provenance
  case object ThirdParty extends Provenance
  case class UnknownProvenance(reason: String) extends Provenance

  private lazy val classified = collection.mutable.Map[String, Provenance]()

  private lazy val otpLibAbs = Paths.get(otpLibRoot).toAbsolutePath

  private def isChildOfOtpDir(path: String): Boolean = Paths.get(path).toAbsolutePath.startsWith(otpLibAbs)

  def sourcePath(moduleName: String): Option[Path] =
    toProvenance(moduleName) match {
      case FirstParty(sourcePath)     => Some(sourcePath)
      case Generated(sourcePath)      => Some(sourcePath)
      case OtpProvenance              => None
      case ThirdParty                 => None
      case UnknownProvenance(_reason) => None
    }

  def toProvenance(moduleName: String): Provenance = {
    lazy val provenance = {
      val app = BeamDb.getApp(moduleName).get
      if (thirdParty.contains(app.name)) ThirdParty
      else if (isChildOfOtpDir(app.ebinDir)) OtpProvenance
      else {
        val erlFile = SourceMap.get(moduleName).sourceFile
        val path = Paths.get(erlFile)
        if (!Files.exists(path)) UnknownProvenance(s"file $erlFile doesn't exist")
        else {
          Using.resource(Source.fromFile(erlFile)) { contents =>
            try {
              if (contents.getLines().take(50).exists(_.contains("@generated"))) Generated(path)
              else FirstParty(path)
            } catch {
              // Some yaws source files have a weird encoding. But we probably don't mean to analyze those anyway
              case error: Throwable => UnknownProvenance(s"error reading $erlFile : $error")
            }
          }
        }
      }
    }
    classified.getOrElseUpdate(moduleName, provenance)
  }

  def isOtp(moduleName: String) =
    toProvenance(moduleName) match {
      case OtpProvenance => true
      case _             => false
    }

  def isFirstParty(moduleName: String) =
    toProvenance(moduleName) match {
      case _: FirstParty => true
      case _             => false
    }

  /**
    * For analyzing generated files,
    * use `toProvenance` instead.
    *
    * For filtering, use `isOtp` or `isFirstParty`
    */
  @deprecated
  def isGenerated(moduleName: String): Boolean = {
    toProvenance(moduleName) match {
      case _: Generated => true
      case _            => false
    }
  }
}
