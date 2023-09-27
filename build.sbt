import com.typesafe.tools.mima.core._

val scala212 = "2.12.18"
val scala213 = "2.13.12"
val scala3 = "3.3.1"
val fs2Version = "3.9.2"
val circeVersion = "0.14.6"
val circeExtrasVersion = "0.14.2"
val playVersion = "2.10.0-RC7"
val shapeless2Version = "2.3.10"
val shapeless3Version = "3.3.0"
val scalaJavaTimeVersion = "2.5.0"
val diffsonVersion = "4.4.0"
val literallyVersion = "1.1.0"
val weaverVersion = "0.8.3"

val copyrightYears = "2019-2023"

ThisBuild / tlBaseVersion := "1.8"

ThisBuild / organization := "org.gnieh"
ThisBuild / organizationName := "Lucas Satabin"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  tlGitHubDev("satabin", "Lucas Satabin"),
  tlGitHubDev("ybasket", "Yannick Heiber")
)

ThisBuild / crossScalaVersions := Seq(scala212, scala213, scala3)
ThisBuild / scalaVersion := scala213

ThisBuild / tlSitePublishBranch := None
ThisBuild / tlSitePublishTags := true

val commonSettings = List(
  versionScheme := Some("early-semver"),
  libraryDependencies ++= List(
    "co.fs2" %%% "fs2-core" % fs2Version,
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.11.0",
    "io.circe" %%% "circe-parser" % circeVersion % "test",
    "io.circe" %%% "circe-jawn" % circeVersion % "test",
    "io.circe" %%% "circe-generic" % circeVersion % "test",
    "co.fs2" %%% "fs2-io" % fs2Version % "test",
    "com.disneystreaming" %%% "weaver-cats" % weaverVersion % "test",
    "com.disneystreaming" %%% "weaver-scalacheck" % weaverVersion % Test,
    "com.eed3si9n.expecty" %%% "expecty" % "0.16.0" % "test",
    "org.portable-scala" %%% "portable-scala-reflect" % "1.1.2" cross CrossVersion.for3Use2_13
  ) ++ PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
      List(
        compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
        compilerPlugin("com.olegpy" % "better-monadic-for" % "0.3.1" cross CrossVersion.binary)
      )
    }
    .toList
    .flatten,
  scalacOptions := scalacOptions.value.filterNot(_ == "-source:3.0-migration"),
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, _)) => List("-Ypatmat-exhaust-depth", "40")
      case _            => Nil
    }
    .toList
    .flatten,
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, 12)) =>
        List(
          "-Wconf:msg=it is not recommended to define classes/objects inside of package objects:s",
          "-Wconf:msg=type parameter .+ defined in .+ shadows .+:s", // esp.Tag
          "-Wconf:msg=value T is deprecated:s" // jsonpath/xpath literals
        )
      case Some((3, _)) => List("-source:3.2-migration", "-no-indent")
    }
    .toList
    .flatten,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
)

val root = tlCrossRootProject
  .aggregate(
    text,
    csv,
    csvGeneric,
    json,
    jsonCirce,
    jsonPlay,
    jsonDiffson,
    jsonInterpolators,
    xml,
    scalaXml,
    cbor,
    cborJson,
    finiteState,
    unidocs
  )
  .settings(commonSettings)
  .enablePlugins(NoPublishPlugin)

lazy val text = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("text"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-text",
    description := "Utilities for textual data format",
    mimaBinaryIssueFilters ++= List(
      // private class
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.text.CharLikeCharChunks.create"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeCharChunks.needsPull"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeCharChunks.pullNext"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeCharChunks.advance"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeCharChunks.current"),
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.text.CharLikeCharChunks$CharContext"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.text.CharLikeSingleByteChunks.create"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeSingleByteChunks.needsPull"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeSingleByteChunks.pullNext"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeSingleByteChunks.advance"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeSingleByteChunks.current"),
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.text.CharLikeSingleByteChunks$ByteContext"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.text.CharLikeStringChunks.create"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeStringChunks.needsPull"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeStringChunks.pullNext"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeStringChunks.advance"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.text.CharLikeStringChunks.current"),
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.text.CharLikeStringChunks$StringContext")
    )
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )

lazy val csv = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("csv"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-csv",
    description := "Streaming CSV manipulation library",
    mimaBinaryIssueFilters ++= List(
      // Static forwarder, only relevant for Java
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.csv.RowEncoderF.fromNonEmptyMapCsvRowEncoder")
    )
  )
  .jsSettings(
    libraryDependencies ++= List(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test,
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % scalaJavaTimeVersion % Test
    ),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(text)

lazy val csvGeneric = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("csv/generic"))
  .settings(commonSettings)
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
      .flatten,
    // Filter related to DerivedCellDecoder come from removed implicit params. These are only the Java forwarders.
    mimaBinaryIssueFilters ++= List(
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.csv.generic.internal.DerivedCellDecoder.decodeCCons"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.csv.generic.internal.DerivedCellDecoder.decodeCConsObjAnnotated"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.csv.generic.internal.DerivedCellDecoder.decodeCConsObj")
    )
  )
  .jsSettings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion % Test)
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(csv)

lazy val json = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("json"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-json",
    description := "Streaming JSON manipulation library",
    libraryDependencies ++= List(
      "org.typelevel" %%% "literally" % literallyVersion
    ) ++ PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      }
      .toList
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(text, finiteState)

lazy val jsonCirce = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("json/circe"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-json-circe",
    description := "Streaming JSON library with support for circe ASTs",
    libraryDependencies ++= List(
      "io.circe" %%% "circe-core" % circeVersion,
      "org.gnieh" %%% "diffson-circe" % diffsonVersion % "test"
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
        .withSemantics( // jawn parser used for tests is fast-and-loose for performance
          _.withAsInstanceOfs(org.scalajs.linker.interface.CheckedBehavior.Unchecked)
            .withArrayIndexOutOfBounds(org.scalajs.linker.interface.CheckedBehavior.Unchecked))
    }
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(json % "compile->compile;test->test", jsonDiffson % "test->test")

lazy val jsonPlay = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("json/play"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-json-play",
    description := "Streaming JSON library with support for Play! JSON ASTs",
    libraryDependencies ++= List(
      "com.typesafe.play" %%% "play-json" % playVersion,
      "org.gnieh" %%% "diffson-play-json" % diffsonVersion % "test"
    ),
    // 2.x support was actually introduced in 1.3.0, but we forgot to publish the artifacts in later versions
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.3.0", "2.12" -> "1.3.0"),
    tlMimaPreviousVersions ~= { (versions: Set[String]) => versions.diff(Set("1.3.1", "1.4.0", "1.4.1", "1.5.0")) }
  )
  .dependsOn(json % "compile->compile;test->test", jsonDiffson % "test->test")

lazy val jsonDiffson = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("json/diffson"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-json-diffson",
    description := "Streaming JSON library with support for patches",
    libraryDependencies ++= List(
      "org.gnieh" %%% "diffson-core" % diffsonVersion
    )
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(json % "compile->compile;test->test")

lazy val jsonInterpolators = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("json/interpolators"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-json-interpolators",
    description := "Json interpolators support",
    libraryDependencies ++= List(
      "org.typelevel" %%% "literally" % literallyVersion
    ) ++ PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      }
      .toList
  )
  .jsSettings(
    tlVersionIntroduced := Map("3" -> "1.4.0", "2.13" -> "1.4.0", "2.12" -> "1.4.0")
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(json % "compile->compile;test->test")

lazy val xml = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("xml"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-xml",
    description := "Streaming XML manipulation library",
    libraryDependencies ++= List(
      "org.typelevel" %%% "literally" % literallyVersion
    ) ++ PartialFunction
      .condOpt(CrossVersion.partialVersion(scalaVersion.value)) { case Some((2, _)) =>
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      }
      .toList,
    // all filters related to CommentToken come from converting it from case object to case class
    mimaBinaryIssueFilters ++= List(
      ProblemFilters.exclude[MissingTypesProblem]("fs2.data.xml.internals.MarkupToken$CommentToken$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productElementName"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productElementNames"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.xml.internals.MarkupToken#CommentToken.render"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productPrefix"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productArity"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productElement"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "fs2.data.xml.internals.MarkupToken#CommentToken.productIterator"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.xml.internals.MarkupToken#CommentToken.canEqual"),
      ProblemFilters.exclude[FinalMethodProblem]("fs2.data.xml.internals.MarkupToken#CommentToken.toString")
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(text, finiteState)

lazy val scalaXml = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("xml/scala-xml"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-xml-scala",
    description := "Support for Scala XML ASTs",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.2.0",
    tlVersionIntroduced := Map("3" -> "1.4.0", "2.13" -> "1.4.0", "2.12" -> "1.4.0"),
    mimaBinaryIssueFilters ++= List(
      // Changed from implicit object to implicit val, seems impossible to stub. Second is Scala 3 only
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.xml.scalaXml.package$ScalaXmlBuilder$"),
      ProblemFilters.exclude[MissingFieldProblem]("fs2.data.xml.scalaXml.package.ScalaXmlBuilder")
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )
  .dependsOn(xml % "compile->compile;test->test")

lazy val cbor = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("cbor"))
  .settings(commonSettings)
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
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )

lazy val finiteState = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("finite-state"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-finite-state",
    description := "Streaming finite state machines",
    tlVersionIntroduced := Map("3" -> "1.6.0", "2.13" -> "1.6.0", "2.12" -> "1.6.0"),
    mimaBinaryIssueFilters ++= List(
      // all filters related to esp.Rhs.Captured* come from converting it from case class to case object
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.esp.Rhs$CapturedLeaf"),
      ProblemFilters.exclude[MissingTypesProblem]("fs2.data.esp.Rhs$CapturedLeaf$"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedLeaf.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedLeaf.unapply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.esp.Rhs#CapturedTree.copy$default$1"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree.copy$default$2"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.esp.Rhs#CapturedLeaf.fromProduct"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.esp.Rhs#CapturedTree._1"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.esp.Rhs#CapturedTree._2"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
        "fs2.data.mft.MFTBuilder#Guardable.fs2$data$mft$MFTBuilder$Guardable$$$outer"),
      // rules now only have number of parameters
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.mft.Rules.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("fs2.data.mft.Rules.params"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.mft.Rules.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.mft.Rules.copy$default$1"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.mft.Rules.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("fs2.data.mft.Rules.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("fs2.data.mft.Rules._1"),
      // Removal of experimental class
      ProblemFilters.exclude[MissingFieldProblem]("fs2.data.esp.Tag.True"),
      ProblemFilters.exclude[MissingClassProblem]("fs2.data.esp.Tag$True$")
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )

lazy val cborJson = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("cbor-json"))
  .settings(commonSettings)
  .settings(
    name := "fs2-data-cbor-json",
    description := "Streaming CBOR/JSON interoperability library",
    tlVersionIntroduced := Map("3" -> "1.5.0", "2.13" -> "1.5.0", "2.12" -> "1.5.0")
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .dependsOn(cbor, json, jsonCirce % "test")
  .nativeSettings(
    tlVersionIntroduced := Map("3" -> "1.5.1", "2.13" -> "1.5.1", "2.12" -> "1.5.1")
  )

lazy val benchmarks = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-io" % fs2Version,
      "io.circe" %%% "circe-jawn" % circeVersion,
      "io.circe" %%% "circe-fs2" % "0.14.1"
    )
  )
  .dependsOn(csv, scalaXml, jsonCirce)

lazy val scalafixInput = (project in file("scalafix/input"))
  .disablePlugins(ScalafixPlugin)
  .dependsOn(jsonCirce.jvm)

lazy val scalafixOutput = (project in file("scalafix/output"))
  .disablePlugins(ScalafixPlugin)
  .dependsOn(jsonCirce.jvm)

lazy val scalafixRules = (project in file("scalafix/rules"))
  .disablePlugins(ScalafixPlugin)
  .settings(
    libraryDependencies +=
      "ch.epfl.scala" %%
        "scalafix-core" %
        _root_.scalafix.sbt.BuildInfo.scalafixVersion
  )

lazy val scalafixTests = (project in file("scalafix/tests"))
  .settings(
    scalafixTestkitOutputSourceDirectories :=
      (scalafixOutput / Compile / sourceDirectories).value,
    scalafixTestkitInputSourceDirectories :=
      (scalafixInput / Compile / sourceDirectories).value,
    scalafixTestkitInputClasspath :=
      (scalafixInput / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions :=
      (scalafixInput / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion :=
      (scalafixInput / Compile / scalaVersion).value
  )
  .dependsOn(scalafixInput, scalafixRules)
  .enablePlugins(ScalafixTestkitPlugin)

lazy val site = project
  .in(file("msite"))
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    tlSiteIsTypelevelProject := Some(TypelevelProject.Affiliate),
    tlSiteApiPackage := Some("fs2.data"),
    tlJdkRelease := None,
    tlFatalWarnings := false,
    libraryDependencies ++= List(
      "com.beachape" %% "enumeratum" % "1.7.0",
      "org.gnieh" %% "diffson-circe" % diffsonVersion,
      "io.circe" %% "circe-generic-extras" % circeExtrasVersion,
      "co.fs2" %% "fs2-io" % fs2Version,
      "io.circe" %% "circe-fs2" % "0.14.1"
    ),
    scalacOptions += "-Ymacro-annotations",
    mdocIn := file("site"),
    laikaSite := {
      sbt.IO.copyDirectory(mdocOut.value, (laikaSite / target).value)
      Set.empty
    }
  )
  .dependsOn(csv.jvm,
             csvGeneric.jvm,
             json.jvm,
             jsonDiffson.jvm,
             jsonCirce.jvm,
             jsonInterpolators.jvm,
             xml.jvm,
             scalaXml.jvm,
             cbor.jvm,
             cborJson.jvm)

lazy val unidocs = project
  .in(file("unidocs"))
  .enablePlugins(TypelevelUnidocPlugin)
  .settings(
    name := "fs2-data-docs",
    tlJdkRelease := None,
    tlFatalWarnings := false,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      cbor.jvm,
      cborJson.jvm,
      csv.jvm,
      csvGeneric.jvm,
      json.jvm,
      jsonCirce.jvm,
      jsonDiffson.jvm,
      jsonPlay.jvm,
      jsonInterpolators.jvm,
      text.jvm,
      xml.jvm,
      scalaXml.jvm
    )
  )

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
