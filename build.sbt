import sbt.Keys._

scalaVersion in ThisBuild := "2.12.3"

name in ThisBuild := "matrix"

organization in ThisBuild := "ai.dragonfly.code"

version in ThisBuild := "0.1"

resolvers in ThisBuild += "dragonfly.ai" at "http://code.dragonfly.ai:8080/"

publishTo in ThisBuild := Some(Resolver.file("file",  new File("/var/www/maven")))

val matrix = crossProject.settings(
  // shared settings
  libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.1"
).jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).jsSettings(
  npmDependencies in Compile += "jama" -> "1.0.4",
  webpackBundlingMode := BundlingMode.LibraryAndApplication("Jama")
).jvmSettings(
  // JVM-specific settings here
  libraryDependencies ++= Seq(
    "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    "gov.nist.math" % "jama" % "1.0.2"
  )
)

lazy val js = matrix.js

lazy val jvm = matrix.jvm