name := "reming-json"

version := "1.3.1"

organization := "com.github.jkinkead"

description := "An AST-free Scala library for JSON marshalling, based on spray-json"

homepage := Some(new URL("https://github.com/jkinkead/spray-json"))

startYear := Some(2011)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-language:_", "-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Opts.resolver.sonatypeReleases

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.16" % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.4.16" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

// generate boilerplate
Boilerplate.settings

// TODO: Publishing.

// crossScalaVersions := Seq("2.10.4", "2.11.2")

// scalaBinaryVersion <<= scalaVersion(sV => if (CrossVersion.isStable(sV)) CrossVersion.binaryScalaVersion(sV) else sV)

// publishMavenStyle := true
