val scala212 = "2.12.13"
val scala213 = "2.13.6"
val scala3 = "3.0.1"
val fs2Version = "3.0.6"
val circeVersion = "0.14.1"
val shapeless2Version = "2.3.7"
val shapeless3Version = "3.0.2"
val scalaJavaTimeVersion = "2.3.0"
val diffsonVersion = "4.1.1"

val commonSettings = List(
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala212, scala3),
  // Copied from circe
  Compile / unmanagedSourceDirectories ++= {
    def extraDirs(suffix: String) =
      CrossType.Full.sharedSrcDir(baseDirectory.value, "main").toList.map(f => file(f.getPath + suffix))

    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) => extraDirs("-2") ++ (if (y >= 13) extraDirs("-2.13+") else Nil)
      case Some((3, _)) => extraDirs("-3") ++ extraDirs("-2.13+")
      case _            => Nil
    }
  },
  Test / unmanagedSourceDirectories ++= {
    def extraDirs(suffix: String) =
      CrossType.Full.sharedSrcDir(baseDirectory.value, "test").toList.map(f => file(f.getPath + suffix))

    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) => extraDirs("-2") ++ (if (y >= 13) extraDirs("-2.13+") else Nil)
      case Some((3, _)) => extraDirs("-3") ++ extraDirs("-2.13+")
      case _            => Nil
    }
  },
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
      case Some((3, _)) =>
        List("-Ykind-projector")
    }
    .toList
    .flatten,
  libraryDependencies ++= List(
    "co.fs2" %%% "fs2-core" % fs2Version,
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.5.0",
    "io.circe" %%% "circe-parser" % circeVersion % "test",
    "co.fs2" %% "fs2-io" % fs2Version % "test",
    "com.disneystreaming" %%% "weaver-cats" % "0.7.4" % "test",
    "com.disneystreaming" %%% "weaver-cats-core" % "0.7.4" % "test",
    "com.disneystreaming" %%% "weaver-core" % "0.7.4" % "test",
    "com.disneystreaming" %%% "weaver-framework" % "0.7.4" % "test",
    "com.eed3si9n.expecty" %%% "expecty" % "0.15.4" % "test",
    "org.portable-scala" %%% "portable-scala-reflect" % "1.1.1" cross CrossVersion.for3Use2_13
  ) ++ PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
      List(
        compilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full),
        compilerPlugin("com.olegpy" % "better-monadic-for" % "0.3.1" cross CrossVersion.binary)
      )
    }
    .toList
    .flatten,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
  scmInfo := Some(ScmInfo(url("https://github.com/satabin/fs2-data"), "scm:git:git@github.com:satabin/fs2-data.git"))
)

val publishSettings = List(
  Test / publishArtifact := false,
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
    publish / skip := true,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(benchmarks,
                                                                             cbor.js,
                                                                             csv.js,
                                                                             csvGeneric.js,
                                                                             json.js,
                                                                             jsonCirce.js,
                                                                             jsonDiffson.js,
                                                                             text.js,
                                                                             xml.js),
    ScalaUnidoc / siteSubdirName := "api",
    addMappingsToSiteDir(ScalaUnidoc / packageDoc / mappings, ScalaUnidoc / siteSubdirName),
    Nanoc / sourceDirectory := file("site"),
    git.remoteRepo := scmInfo.value.get.connection.replace("scm:git:", ""),
    ghpagesNoJekyll := true
  )
  .aggregate(
    text.jvm,
    text.js,
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

lazy val text = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("text"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-text",
    description := "Utilities for textual data format"
  )

lazy val csv = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("csv"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(name := "fs2-data-csv", description := "Streaming CSV manipulation library")
  .jsSettings(
    libraryDependencies ++= List(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion % Test
    ))
  .dependsOn(text)

lazy val csvGeneric = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("csv/generic"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-csv-generic",
    description := "Generic CSV row decoder generation",
    libraryDependencies ++= onScala2(scalaVersion.value)(
      List(
        "com.chuusai" %%% "shapeless" % shapeless2Version,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      )
    ) ++ onScala3(scalaVersion.value)(
      List(
        "org.typelevel" %%% "shapeless3-deriving" % shapeless3Version
      )
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
  .jsSettings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test)
  .dependsOn(csv)

lazy val json = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("json"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(name := "fs2-data-json", description := "Streaming JSON manipulation library")
  .dependsOn(text)

lazy val jsonCirce = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("json/circe"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-json-circe",
    description := "Streaming JSON library with support for circe ASTs",
    libraryDependencies ++= List(
      "io.circe" %%% "circe-core" % circeVersion,
      "org.gnieh" %%% "diffson-circe" % diffsonVersion % "test"
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
      "org.gnieh" %%% "diffson-core" % diffsonVersion
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
      "org.typelevel" %% "literally" % "1.0.2"
    ) ++ PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      }
      .toList
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
  .dependsOn(text)

lazy val cbor = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("cbor"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name := "fs2-data-cbor",
    description := "Streaming CBOR manipulation library",
    scalacOptions ++= PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
        List("-opt:l:inline", "-opt-inline-from:fs2.data.cbor.low.internal.ItemParser$")
      }
      .toList
      .flatten
  )

lazy val documentation = project
  .in(file("documentation"))
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    scalaVersion := scala213,
    crossScalaVersions := List(scala212, scala213),
    mdocIn := file("documentation/docs"),
    mdocOut := file("site/content/documentation"),
    libraryDependencies ++= List(
      "com.beachape" %% "enumeratum" % "1.5.15",
      "org.gnieh" %% "diffson-circe" % diffsonVersion,
      "io.circe" %% "circe-generic-extras" % circeVersion,
      "co.fs2" %% "fs2-io" % fs2Version
    )
  )
  .dependsOn(csv.jvm, csvGeneric.jvm, json.jvm, jsonDiffson.jvm, jsonCirce.jvm, jsonInterpolators, xml.jvm, cbor.jvm)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .dependsOn(csv.jvm)

// Utils

def onScala2[T](version: String)(values: => List[T]): List[T] = PartialFunction
  .condOpt(CrossVersion.partialVersion(version)) { case Some((2, _)) =>
    values
  }
  .toList
  .flatten

def onScala3[T](version: String)(values: => List[T]): List[T] = PartialFunction
  .condOpt(CrossVersion.partialVersion(version)) { case Some((3, _)) =>
    values
  }
  .toList
  .flatten
