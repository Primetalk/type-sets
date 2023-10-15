val scala3Version = "3.3.1"
val mainVersion = "0.0.1"

ThisBuild / version      := mainVersion
ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")

val commonSettings = Seq(
  scalaVersion := scala3Version,
  organization := "org.primetalk",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
)

lazy val root = (project in file("."))
  .aggregate(
    `type-set`,
  )
  .settings(
    name := "type-sets",
    publish / skip := true,
  )

lazy val `type-set` = (project in file("type-set"))
  .settings(commonSettings :_*)
  .settings(
    name := "type-set",
  )
