package com.whatsapp

import com.typesafe.config.ConfigFactory

// adapted from eqwalizer
package object corq {
  case class Config(
      otpLibRoot: String,
      srcRoot: String,
      libRoot: String,
      apps: List[String],
      thirdPartyApps: Set[String],
  )

  lazy val config: Config = {
    import scala.jdk.CollectionConverters.CollectionHasAsScala

    val config = ConfigFactory.load().getConfig("corq")
    Config(
      otpLibRoot = config.getString("otp_lib_root"),
      libRoot = config.getString("lib_root"),
      srcRoot = config.getString("src_root"),
      apps = config.getStringList("apps").asScala.toList,
      thirdPartyApps = config.getStringList("third_party_apps").asScala.toSet,
    )
  }
}
