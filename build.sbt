val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "typeur-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)
