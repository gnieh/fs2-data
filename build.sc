import mill._
import scalalib._
import scalafmt._
import publish._

val millVersion = System.getProperty("MILL_VERSION")
interp.load.ivy("com.lihaoyi" %% "mill-contrib-bloop" % millVersion)

@

val scala212 = "2.12.8"
val scala213 = "2.13.0"

val fs2Version = "1.1.0-M1"
val circeVersion = "0.12.0-M4"

val fs2DataVersion = "0.3.0-SNAPSHOT"

val fs2DataLicense = License.`Apache-2.0`

val fs2DataUrl = "https://github.com/satabin/fs2-data"

val fs2DataDeveloper = Developer("satabin", "Lucas Satabin", "https://github.com/satabin")

trait Fs2DataModule extends ScalaModule with ScalafmtModule {

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked", "-Ypatmat-exhaust-depth", "off", "-Ywarn-unused:imports")

  def ivyDeps =
    Agg(
      ivy"co.fs2::fs2-core:$fs2Version",
      ivy"org.scala-lang.modules::scala-collection-compat:2.1.1")

  def scalacPluginIvyDeps = Agg(ivy"com.olegpy::better-monadic-for:0.3.1")

  trait Fs2DataTests extends Tests {
    def ivyDeps =
      Agg(
        ivy"org.scalatest::scalatest:3.0.8",
        ivy"com.github.pathikrit::better-files:3.8.0",
        ivy"io.circe::circe-parser:$circeVersion",
        ivy"co.fs2::fs2-io:$fs2Version"
      )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}

object csv extends Cross[CsvModule](scala212, scala213)

class CsvModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule with PublishModule {

  def publishVersion = fs2DataVersion

  def artifactName = "fs2-data-csv"

  def pomSettings =
    PomSettings(
      description = "Streaming CSV manipulation library",
      organization = "org.gnieh",
      url = fs2DataUrl,
      licenses = Seq(fs2DataLicense),
      versionControl = VersionControl.github("satabin", "fs2-data"),
      developers = Seq(fs2DataDeveloper)
    )

  object test extends Fs2DataTests

}

object json extends Cross[JsonModule](scala212, scala213)

class JsonModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule with PublishModule {
  outer =>

  def publishVersion = fs2DataVersion

  def artifactName = "fs2-data-json"

  def pomSettings =
    PomSettings(
      description = "Streaming JSON manipulation library",
      organization = "org.gnieh",
      url = fs2DataUrl,
      licenses = Seq(fs2DataLicense),
      versionControl = VersionControl.github("satabin", "fs2-data"),
      developers = Seq(fs2DataDeveloper)
    )

    object test extends Fs2DataTests {
      def moduleDeps = Seq(circe)
    }

  object circe extends Fs2DataModule with PublishModule {
    def scalaVersion = outer.scalaVersion
    def moduleDeps = Seq(outer)
    def ivyDeps = Agg(ivy"io.circe::circe-core:$circeVersion")

    def publishVersion = fs2DataVersion

    def artifactName = "fs2-data-json-circe"

    def pomSettings =
      PomSettings(
        description = "Streaming JSON library with support for circe ASTs",
        organization = "org.gnieh",
        url = fs2DataUrl,
        licenses = Seq(fs2DataLicense),
        versionControl = VersionControl.github("satabin", "fs2-data"),
        developers = Seq(fs2DataDeveloper)
      )

  }

}

object xml extends Cross[XmlModule](scala212, scala213)

class XmlModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule with PublishModule {
  outer =>

  def publishVersion = fs2DataVersion

  def artifactName = "fs2-data-xml"

  def pomSettings =
    PomSettings(
      description = "Streaming XML manipulation library",
      organization = "org.gnieh",
      url = fs2DataUrl,
      licenses = Seq(fs2DataLicense),
      versionControl = VersionControl.github("satabin", "fs2-data"),
      developers = Seq(fs2DataDeveloper)
    )

  object test extends Fs2DataTests
}
