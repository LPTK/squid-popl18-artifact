name := "squid-popl18-artifact"

version := "1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.11",
  libraryDependencies += "ch.epfl.data" %% "squid" % "0.2-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-language:postfixOps", "-unchecked")
)

// lazy val main = (project in file("."))
//     .settings(commonSettings: _*)
