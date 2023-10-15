val scala3Version = "3.3.1"
val mainVersion = "0.0.1"

// ThisBuild / organization := "ru.primetalk"
ThisBuild / version      := mainVersion
ThisBuild / scalaVersion := scala3Version
ThisBuild / versionScheme := Some("early-semver")

//lazy val commonSettings = Seq(
//  scalacOptions ++= Seq(
////    "-Ymacro-annotations",// required for simulacrum starting from Scala 2.13+
//    "-deprecation",
//    "-feature",
//    "-language:higherKinds"
////    "-Ytyper-debug"
//  ),
//  publishArtifact in Test := false,
////  libraryDependencies += "org.specs2" %% "specs2-core" % "4.7.0" % Test,
////  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
//  scalacOptions in Test ++= Seq("-Yrangepos")
//)
//

lazy val `type-sets` = (project in file(".")).
  settings(
    organization := "ru.primetalk",
    name := "type-sets",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
