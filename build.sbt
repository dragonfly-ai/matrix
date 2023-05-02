val appVersion:String = "0.1"
val globalScalaVersion = "3.2.1"

ThisBuild / organization := "ai.dragonfly"
ThisBuild / organizationName := "dragonfly.ai"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List( tlGitHubDev("dragonfly-ai", "dragonfly.ai") )
ThisBuild / scalaVersion := globalScalaVersion

ThisBuild / tlBaseVersion := appVersion
ThisBuild / tlCiReleaseBranches := Seq()
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / nativeConfig ~= {
  _.withLTO(scala.scalanative.build.LTO.thin)
    .withMode(scala.scalanative.build.Mode.releaseFast)
    .withGC(scala.scalanative.build.GC.commix)
}

lazy val matrix = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(
    description := "High performance, low footprint, cross platform, matrix math and machine learning library!",
    libraryDependencies += "ai.dragonfly" %%% "vector" % "0.1",
  )
  .jvmSettings(
    libraryDependencies ++= Seq("org.scala-js" %% "scalajs-stubs" % "1.1.0")
  )
  .jsSettings()
  .nativeSettings()

lazy val verification = project
  .dependsOn(matrix.projects(JVMPlatform))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "verification",
    Compile / mainClass := Some("verification.Verify"),
    libraryDependencies ++= Seq( "gov.nist.math" % "jama" % "1.0.3" )
  )

lazy val demo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .dependsOn(matrix)
  .settings(
    name := "demo",
    Compile / mainClass := Some("Demo"),
    libraryDependencies ++= Seq(
      "ai.dragonfly" %%% "cliviz" % "0.1",
      "ai.dragonfly" %%% "democrossy" % "0.102"
    )
  ).jsSettings(
    Compile / fullOptJS / artifactPath := file("./demo/public_html/js/main.js"),
    scalaJSUseMainModuleInitializer := true
  ).jvmSettings(
    libraryDependencies ++= Seq( "gov.nist.math" % "jama" % "1.0.2" )
  )

lazy val root = tlCrossRootProject.aggregate(matrix).settings(name := "matrix")

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin).settings(
  mdocVariables := Map(
    "VERSION" -> appVersion,
    "SCALA_VERSION" -> globalScalaVersion
  ),
  laikaConfig ~= { _.withRawContent }
)

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin) // also enables the ScalaUnidocPlugin
  .settings(
    name := "matrix-docs",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(matrix.jvm, matrix.js, matrix.native)
  )