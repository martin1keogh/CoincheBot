import AssemblyKeys._

name := "CoincheBot"

version := "0.01"

scalaVersion := "2.10.2"


libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "pircbot" % "pircbot" % "1.5.0"

libraryDependencies += "com.typesafe" % "config" % "1.0.1"

libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor"   % "2.2-M3",
    "com.typesafe.akka" %% "akka-slf4j"   % "2.2-M3",
    "com.typesafe.akka" %% "akka-remote"  % "2.2-M3",
    "com.typesafe.akka" %% "akka-agent"   % "2.2-M3",
    "com.typesafe.akka" %% "akka-testkit" % "2.2-M3" % "test"
    )

parallelExecution in Test := false

assemblySettings
