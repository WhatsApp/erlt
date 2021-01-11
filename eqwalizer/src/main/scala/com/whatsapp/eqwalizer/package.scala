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

package com.whatsapp

import com.typesafe.config.ConfigFactory

package object eqwalizer {
  case class Config(
      otpLibRoot: String,
      srcRoot: String,
      libRoot: String,
      apps: List[String],
      thirdPartyApps: Set[String],
  )

  lazy val config: Config = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val config = ConfigFactory.load().getConfig("eqwalizer")
    Config(
      otpLibRoot = config.getString("otp_lib_root"),
      libRoot = config.getString("lib_root"),
      srcRoot = config.getString("src_root"),
      apps = config.getStringList("apps").asScala.toList,
      thirdPartyApps = config.getStringList("third_party_apps").asScala.toSet,
    )
  }
}
