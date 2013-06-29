name := "Coinche"

version := "0.01"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "pircbot" % "pircbot" % "1.5.0"
            
parallelExecution in Test := false
