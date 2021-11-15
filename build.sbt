import Dependencies._

ThisBuild / scalaVersion := "3.0.2"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "windymelt"
ThisBuild / organizationName := "windymelt"

// Spliting project in order to use parboiled2 which only compiles in 2.x.x
lazy val parser = (project in file("parser"))
  .settings(
    name := "webdbpress125-interpreter-in-scala3-parser",
    scalaVersion := "2.13.7",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += parboiled2
  )

lazy val root = (project in file("."))
  .settings(
    name := "webdbpress125-interpreter-in-scala3",
    libraryDependencies += scalaTest % Test
  )
  .dependsOn(parser)
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
