val V = _root_.scalafix.sbt.BuildInfo

inThisBuild(
  List(
    ThisBuild / scalaVersion := V.scala212,
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions += "-Yrangepos"
  ))

lazy val rules = project.settings(
  libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafixVersion
)

lazy val scalafixInput = (project in file("scalafix/input"))
  .settings(
    libraryDependencies += "org.gnieh" %% "fs2-data-json-circe" % "1.8.0"
  )
  .disablePlugins(ScalafixPlugin)

lazy val scalafixOutput = (project in file("scalafix/output"))
  .settings(
    libraryDependencies += "org.gnieh" %% "fs2-data-json-circe" % "1.8.0"
  )
  .disablePlugins(ScalafixPlugin)

lazy val tests = (project in file("scalafix/tests"))
  .settings(
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafixVersion % Test cross CrossVersion.full,
    Compile / compile :=
      (Compile / compile).dependsOn(scalafixInput / Compile / compile).value,
    scalafixTestkitOutputSourceDirectories :=
      (scalafixOutput / Compile / sourceDirectories).value,
    scalafixTestkitInputSourceDirectories :=
      (scalafixInput / Compile / sourceDirectories).value,
    scalafixTestkitInputClasspath :=
      (scalafixInput / Compile / fullClasspath).value
  )
  .dependsOn(scalafixInput, rules)
  .enablePlugins(ScalafixTestkitPlugin)
