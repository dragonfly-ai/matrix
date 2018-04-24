import sbt.Keys._

scalaVersion in ThisBuild := "2.12.3"

name in ThisBuild := "matrix"

organization in ThisBuild := "ai.dragonfly.code"

version in ThisBuild := "0.1"

publishTo in ThisBuild := Some(Resolver.file("file",  new File( "/var/www/maven" )) )

enablePlugins(ScalaJSBundlerPlugin)

npmDependencies in Compile += "jamajs" -> "1.0.1"

val matrix = crossProject.settings(
  // shared settings
).jsSettings(
  // JS-specific settings here
).jvmSettings(
  // JVM-specific settings here
  libraryDependencies ++= Seq(
    "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    "gov.nist.math" % "jama" % "1.0.2"
  )
)

lazy val js = matrix.js

lazy val jvm = matrix.jvm
