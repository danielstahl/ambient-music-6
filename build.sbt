name := "ambient-music-6"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11"

libraryDependencies += "net.soundmining" %% "soundmining-tools" % "1.0-SNAPSHOT"

libraryDependencies += "net.soundmining" %% "soundmining-modular" % "1.0-SNAPSHOT"

console / initialCommands := """
    |import net.soundmining._
    |AmbientMusic6.init()
""".trim().stripMargin

console / cleanupCommands += """
    AmbientMusic6.stop()
"""
