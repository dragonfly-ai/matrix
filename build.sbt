ThisBuild / scalaVersion := "3.1.0"
ThisBuild / publishTo := Some( Resolver.file( "file",  new File("/var/www/maven") ) )

lazy val matrix = crossProject(JSPlatform, JVMPlatform)
  .settings(
    publishTo := Some(Resolver.file("file",  new File( "/var/www/maven" ))),
    name := "matrix",
    version := "0.4.53",
    organization := "ai.dragonfly.code",
    resolvers += "dragonfly.ai" at "https://code.dragonfly.ai/",
    scalacOptions ++= Seq("-feature","-deprecation"),
    libraryDependencies += "ai.dragonfly.code" %%% "vector" % "0.53",
  ).jsSettings().jvmSettings()
    // JVM-specific settings here
    /*
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-stubs" % "1.1.0"
    )
     */


lazy val verification = project.dependsOn(matrix.projects(JVMPlatform)).settings(
  name := "verification",
  version := "0.01",
  Compile / mainClass := Some("verification.Verify"),
  libraryDependencies ++= Seq( "gov.nist.math" % "jama" % "1.0.3" )
)

lazy val demo = crossProject(JSPlatform, JVMPlatform).dependsOn(matrix).settings(
  name := "demo",
  Compile / mainClass := Some("Demo"),
  libraryDependencies ++= Seq( "ai.dragonfly.code" %%% "cliviz" % "0.0106.53" )
).jsSettings(
  Compile / fastOptJS / artifactPath := file("./demo/public_html/js/main.js"),
  Compile / fullOptJS / artifactPath := file("./demo/public_html/js/main.js"),
  scalaJSUseMainModuleInitializer := true
).jvmSettings(
  libraryDependencies ++= Seq( "gov.nist.math" % "jama" % "1.0.2" )
)
