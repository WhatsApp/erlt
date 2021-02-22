package com.whatsapp

import com.typesafe.config.ConfigFactory
import com.whatsapp.eqwalizer.io.BuildInfo

package object eqwalizer {
  case class Config(
      otpLibRoot: String,
      apps: Map[String, String],
      thirdPartyApps: Set[String],
  )

  lazy val config: Config = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val config = ConfigFactory.load().getConfig("eqwalizer")
    val buildInfoPath = config.getString("build_info")
    val buildInfo = BuildInfo.load(buildInfoPath)
    Config(
      otpLibRoot = buildInfo.otpLibRoot,
      apps = buildInfo.apps,
      thirdPartyApps = config.getStringList("third_party_apps").asScala.toSet,
    )
  }
}
