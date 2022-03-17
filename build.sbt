ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val matrix = crossProject(JSPlatform, JVMPlatform)
  .settings(
    publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
    name := "matrix",
    version := "0.33.4522",
    organization := "ai.dragonfly.code",
    resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
    scalacOptions ++= Seq("-feature","-deprecation"),
    libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.4522",
    Compile / mainClass := Some("ai.dragonfly.math.matrix.demo.Demo")
  )
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin)).jsSettings(
      Compile / npmDependencies += "jama" -> "1.0.4",
      webpackBundlingMode := BundlingMode.LibraryAndApplication("Jama"),
      scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings(
    // JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0",
      "gov.nist.math" % "jama" % "1.0.2"
    )
  )
