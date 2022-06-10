ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val matrix = crossProject(JSPlatform, JVMPlatform)
  .settings(
    publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
    name := "matrix",
    version := "0.331.526",
    organization := "ai.dragonfly.code",
    resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
    scalacOptions ++= Seq("-feature","-deprecation"),
    libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.526",
  ).jsSettings().jvmSettings()
    // JVM-specific settings here
    /*
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0"
    )
     */



lazy val demo = crossProject(JSPlatform, JVMPlatform).dependsOn(matrix).settings(
  name := "demo",
  Compile / mainClass := Some("Demo")
).jsSettings(
  scalaJSUseMainModuleInitializer := true
).jvmSettings(
  libraryDependencies ++= Seq( "gov.nist.math" % "jama" % "1.0.2" )
)
