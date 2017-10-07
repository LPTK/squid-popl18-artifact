name := "squid-popl18-artifact"

version := "1.0"

scalaVersion := "2.11.11"
libraryDependencies += "ch.epfl.data" %% "squid" % "0.2-SNAPSHOT"
scalacOptions ++= Seq("-feature", "-language:postfixOps", "-unchecked")
