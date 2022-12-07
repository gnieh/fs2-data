// Remove me as soon as all sbt plugins use scala-xml 2 and we got rid of the annoying errors
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)

addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.4.17")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.6")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.4.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.11.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.6")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.9")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")
