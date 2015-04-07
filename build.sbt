name := "reming-json"

version := "0.0.9-SNAPSHOT"

organization := "com.github.jkinkead"

description := "An AST-free Scala library for JSON marshalling, based on spray-json"

homepage := Some(new URL("https://github.com/jkinkead/spray-json"))

startYear := Some(2015)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-language:_", "-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Opts.resolver.sonatypeReleases

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// TODO: Publishing.

// crossScalaVersions := Seq("2.10.4", "2.11.2")

// scalaBinaryVersion <<= scalaVersion(sV => if (CrossVersion.isStable(sV)) CrossVersion.binaryScalaVersion(sV) else sV)

// publishMavenStyle := true
