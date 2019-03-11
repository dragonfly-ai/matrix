import sbtcrossproject.CrossPlugin.autoImport.crossProject

val sharedSettings = Seq(
  version in ThisBuild := "0.2",
  scalaVersion := "2.12.6",
  organization in ThisBuild := "ai.dragonfly.code",
  scalacOptions in ThisBuild ++= Seq("-feature"),
  resolvers in ThisBuild += "dragonfly.ai" at "http://code.dragonfly.ai/",
  libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.1",
  publishTo in ThisBuild := Some( Resolver.file ( "file",  new File( "/var/www/maven" ) ) )
)

val matrix = crossProject(JSPlatform, JVMPlatform)
  .settings(sharedSettings)
  // shared settings
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).jsSettings(
    npmDependencies in Compile += "jama" -> "1.0.4",
    webpackBundlingMode := BundlingMode.LibraryAndApplication("Jama")
  ).jvmSettings(
    // JVM-specific settings here
    libraryDependencies ++= Seq(
    "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided",
    "gov.nist.math" % "jama" % "1.0.2"
  )
)
