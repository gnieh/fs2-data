val scala212 = "2.12.12"
val scala213 = "2.13.3"
val fs2Version = "2.5.2"
val circeVersion = "0.13.0"
val shapelessVersion = "2.3.3"

val commonSettings = List(
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala213, scala212),
  organization := "org.gnieh",
  headerLicense := Some(HeaderLicense.ALv2("2021", "Lucas Satabin")),
  licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://github.com/satabin/fs2-data")),
  scalacOptions ++= List("-feature",
                         "-deprecation",
                         "-unchecked",
                         "-Ypatmat-exhaust-depth",
                         "off",
                         "-Ywarn-unused:imports,privates"),
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, n)) if n < 13 =>
        List("-Ypartial-unification", "-language:higherKinds")
    }
    .toList
    .flatten,
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
  addCompilerPlugin("com.olegpy" % "better-monadic-for" % "0.3.1" cross CrossVersion.binary),
  libraryDependencies ++= List(
    "co.fs2" %%% "fs2-core" % fs2Version,
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.4.2",
    "org.scalatest" %%% "scalatest" % "3.2.4" % "test",
    "io.circe" %%% "circe-parser" % circeVersion % "test",
    "co.fs2" %% "fs2-io" % fs2Version % "test",
    "com.github.pathikrit" %% "better-files" % "3.9.1" % "test"
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/satabin/fs2-data"), "scm:git:git@github.com:satabin/fs2-data.git"))
)

val publishSettings = List(
  publishArtifact in Test := false,
  pomIncludeRepository := { x =>
    false
  },
  developers := List(
    Developer(id = "satabin",
              name = "Lucas Satabin",
              email = "lucas.satabin@gnieh.org",
              url = url("https://github.com/satabin"))
  ),
  pomExtra := (
    <ciManagement>
      <system>travis</system>
      <url>https://travis-ci.org/#!/satabin/fs2-data</url>
    </ciManagement>
    <issueManagement>
      <system>github</system>
      <url>https://github.com/satabin/fs2-data/issues</url>
    </issueManagement>
  )
)

val root = (project in file("."))
  .settings(commonSettings)
  .enablePlugins(ScalaUnidocPlugin, SiteScaladocPlugin, NanocPlugin, GhpagesPlugin)
  .settings(
    name := "fs2-data",
    publishArtifact := false,
    skip in publish := true,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(benchmarks,
                                                                               csv.js,
                                                                               csvGeneric.js,
                                                                               json.js,
                                                                               jsonCirce.js,
                                                                               jsonDiffson.js,
                                                                               xml.js,
                                                                               cbor.js),
    siteSubdirName in ScalaUnidoc := "api",
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    Nanoc / sourceDirectory := file("site"),
    git.remoteRepo := scmInfo.value.get.connection.replace("scm:git:", ""),
    ghpagesNoJekyll := true
  )
  .aggregate(
    csv.jvm,
    csv.js,
    csvGeneric.jvm,
    csvGeneric.js,
    json.jvm,
    json.js,
    jsonCirce.jvm,
    jsonCirce.js,
    jsonDiffson.jvm,
    jsonDiffson.js,
    jsonInterpolators,
    xml.jvm,
    xml.js,
    cbor.jvm,
    cbor.js
  )

lazy val csv = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("csv"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(name := "fs2-data-csv", description := "Streaming CSV manipulation library")
  .jsSettings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.1.0" % Test)

lazy val csvGeneric = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("csv/generic"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-csv-generic",
    description := "Generic CSV row decoder generation",
    libraryDependencies ++= List(
      "com.chuusai" %%% "shapeless" % shapelessVersion,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
          )
        case _ =>
          // if scala 2.13.0 or later, macro annotations merged into scala-reflect
          Nil
      }),
    scalacOptions ++= PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
        case Some((2, n)) if n >= 13 =>
          Seq(
            "-Ymacro-annotations"
          )
      }
      .toList
      .flatten
  )
  .jsSettings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.1.0" % Test)
  .dependsOn(csv)

lazy val json = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("json"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(name := "fs2-data-json",
            description := "Streaming JSON manipulation library",
            libraryDependencies ++= List("org.gnieh" %%% "diffson-circe" % "4.0.3" % "test"))

lazy val jsonCirce = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("json/circe"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-json-circe",
    description := "Streaming JSON library with support for circe ASTs",
    libraryDependencies ++= List(
      "io.circe" %%% "circe-core" % circeVersion
    )
  )
  .dependsOn(json % "compile->compile;test->test", jsonDiffson % "test->test")

lazy val jsonDiffson = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("json/diffson"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-json-diffson",
    description := "Streaming JSON library with support for patches",
    libraryDependencies ++= List(
      "org.gnieh" %%% "diffson-core" % "4.0.3"
    )
  )
  .dependsOn(json % "compile->compile;test->test")

lazy val jsonInterpolators = project
  .in(file("json/interpolators"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-json-interpolators",
    description := "Json interpolators support",
    libraryDependencies ++= List(
      "com.propensive" %% "contextual" % "1.2.1",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )
  .dependsOn(json.jvm % "compile->compile;test->test")

lazy val xml = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("xml"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-xml",
    description := "Streaming XML manipulation library"
  )

lazy val cbor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("cbor"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-cbor",
    description := "Streaming CBOR manipulation library",
    scalacOptions ++= List("-opt:l:inline", "-opt-inline-from:fs2.data.cbor.low.internal.ItemParser$")
  )

lazy val documentation = project
  .in(file("documentation"))
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    mdocIn := file("documentation/docs"),
    mdocOut := file("site/content/documentation"),
    libraryDependencies ++= List(
      "com.beachape" %% "enumeratum" % "1.5.15",
      "org.gnieh" %% "diffson-circe" % "4.0.3",
      "io.circe" %% "circe-generic-extras" % circeVersion,
      "co.fs2" %% "fs2-io" % fs2Version
    )
  )
  .dependsOn(csv.jvm, csvGeneric.jvm, json.jvm, jsonDiffson.jvm, jsonCirce.jvm, jsonInterpolators, xml.jvm, cbor.jvm)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= List(
      "com.github.pathikrit" %% "better-files" % "3.9.1"
    )
  )
  .dependsOn(csv.jvm)
