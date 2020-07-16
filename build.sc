import mill._
import eval._
import scalalib._
import scalafmt._
import publish._
import $file.jmh
import jmh.Jmh
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import mill.define.BaseModule
import $file.mdoc
import mdoc.MdocModule
import ammonite.ops._
import mill.modules.Jvm.runSubprocess

val scala212 = "2.12.11"
val scala213 = "2.13.2"

val fs2Version = "2.4.1"
val circeVersion = "0.13.0"
val shapelessVersion = "2.3.3"

val fs2DataVersion = "0.7.0"

val fs2DataLicense = License.`Apache-2.0`

val fs2DataUrl = "https://github.com/satabin/fs2-data"

val fs2DataDeveloper = Developer("satabin", "Lucas Satabin", "https://github.com/satabin")

trait Fs2DataModule extends ScalaModule with ScalafmtModule {

  def scalacOptions =
    Seq("-feature", "-deprecation", "-unchecked", "-Ypatmat-exhaust-depth", "off", "-Ywarn-unused:imports") ++
      (if (scalaVersion().startsWith("2.13"))
         Seq()
       else
         Seq("-Ypartial-unification", "-language:higherKinds"))

  def ivyDeps =
    Agg(ivy"co.fs2::fs2-core:$fs2Version", ivy"org.scala-lang.modules::scala-collection-compat:2.1.6")

  def scalacPluginIvyDeps = Agg(ivy"org.typelevel::kind-projector:0.10.3", ivy"com.olegpy::better-monadic-for:0.3.1")

  trait Fs2DataTests extends Tests {
    def ivyDeps =
      Agg(
        ivy"org.scalatest::scalatest:3.1.2",
        ivy"com.github.pathikrit::better-files:3.8.0",
        ivy"io.circe::circe-parser:$circeVersion",
        ivy"co.fs2::fs2-io:$fs2Version"
      )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}

object csv extends Cross[CsvModule](scala212, scala213)

class CsvModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule with PublishModule {
  outer =>

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

  object generic extends Fs2DataModule with PublishModule {
    def scalaVersion = outer.scalaVersion
    def moduleDeps = Seq(outer)
    def ivyDeps = Agg(
      ivy"com.chuusai::shapeless:$shapelessVersion",
      ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
    )

    override def scalacOptions = T {
      if (scalaVersion().startsWith("2.13"))
        Seq("-Ymacro-annotations")
      else
        Seq()
    }

    override def scalacPluginIvyDeps = T {
      if (scalaVersion().startsWith("2.13"))
        super.scalacPluginIvyDeps()
      else
        super.scalacPluginIvyDeps() ++ Agg(ivy"org.scalamacros:::paradise:2.1.1")
    }

    def publishVersion = fs2DataVersion

    def artifactName = "fs2-data-csv-generic"

    def pomSettings =
      PomSettings(
        description = "Generic CSV row decoder generation",
        organization = "org.gnieh",
        url = fs2DataUrl,
        licenses = Seq(fs2DataLicense),
        versionControl = VersionControl.github("satabin", "fs2-data"),
        developers = Seq(fs2DataDeveloper)
      )

    object test extends Fs2DataTests

  }

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
    def moduleDeps = Seq(circe, diffson)
    def ivyDeps = super.ivyDeps() ++ Seq(ivy"org.gnieh::diffson-circe:4.0.2")
  }

  object interpolators extends Fs2DataModule with PublishModule {
    def scalaVersion = outer.scalaVersion
    def moduleDeps = Seq(outer)
    def ivyDeps =
      Agg(ivy"com.propensive::contextual:1.2.1", ivy"org.scala-lang:scala-reflect:${scalaVersion()}")

    def publishVersion = fs2DataVersion

    def artifactName = "fs2-data-json-interpolators"

    def pomSettings =
      PomSettings(
        description = "Json interpolators support",
        organization = "org.gnieh",
        url = fs2DataUrl,
        licenses = Seq(fs2DataLicense),
        versionControl = VersionControl.github("satabin", "fs2-data"),
        developers = Seq(fs2DataDeveloper)
      )

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

  object diffson extends Fs2DataModule with PublishModule {
    def scalaVersion = outer.scalaVersion
    def moduleDeps = Seq(outer)
    def ivyDeps = Agg(ivy"org.gnieh::diffson-core:4.0.2")

    def publishVersion = fs2DataVersion

    def artifactName = "fs2-data-json-diffson"

    def pomSettings =
      PomSettings(
        description = "Streaming JSON library with support for patches",
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

  def scalacPluginIvyDeps = Agg(ivy"com.olegpy::better-monadic-for:0.3.1")

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

object benchmarks extends Cross[Benchmarks](scala212, scala213)
class Benchmarks(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule with Jmh {
  def moduleDeps = Seq(csv(crossScalaVersion))

  def ivyDeps = T { super.ivyDeps() ++ Agg(ivy"com.github.pathikrit::better-files:3.8.0") }

  def millSourcePath = os.pwd / "benchmarks"
}

object documentation extends Fs2DataModule with MdocModule {

  def scalaVersion = scala212

  def moduleDeps =
    Seq(
      csv(scala212),
      csv(scala212).generic,
      json(scala212),
      json(scala212).diffson,
      json(scala212).circe,
      json(scala212).interpolators,
      xml(scala212)
    )

  def mdocVersion = "2.2.0"

  def mdocTargetDirectory = os.pwd / 'site / 'content / 'documentation

  def ivyDeps =
    Agg(ivy"com.beachape::enumeratum:1.5.15", ivy"org.gnieh::diffson-circe:4.0.2", ivy"co.fs2::fs2-io:$fs2Version")

}

def unidoc(ev: Evaluator) = T.command {

  def isInfra(x: ScalaModule): Boolean =
    x match {
      case x: ScalaModule#Tests => true
      case _ =>
        val segments = x.millModuleBasePath.value.segments.toVector
        segments.contains("documentation") || segments.contains("benchmarks")
    }

  val modules =
    ev.rootModule.millInternal.segmentsToModules.values
      .collect { case x: ScalaModule if !isInfra(x) => x }
      .filter(mod => ev.evaluate(Agg(mod.scalaVersion)).values.head.asInstanceOf[String] == scala213)
      .toSeq
  val base = ev.rootModule.millModuleBasePath.value.toNIO.toString

  val sources = ev
    .evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.allSources): _*))
    .values
    .collect {
      case paths: Seq[PathRef] => paths
    }
    .flatten

  val javadocDir = os.pwd / 'site / 'content / 'api
  mkdir(javadocDir)

  val files = for {
    ref <- sources
    if exists(ref.path)
    p <- (if (ref.path.isDir) ls.rec(ref.path) else Seq(ref.path))
    if (p.isFile && ((p.ext == "scala") || (p.ext == "java")))
  } yield p.toNIO.toString

  val pluginOptions = ev
    .evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalacPluginClasspath): _*))
    .values
    .collect {
      case a: Agg[_] =>
        a.items.collect {
          case p: PathRef => s"-Xplugin:${p.path}"
        }
    }
    .flatten
    .distinct

  val scalacOptions = ev
    .evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalacOptions): _*))
    .values
    .collect {
      case l: List[_] =>
        l.collect {
          case s: String => s
        }
    }
    .flatten
    .distinct

  def url(v: String): String = {
    val branch = if (v.endsWith("SNAPSHOT")) "master" else v
    "http://github.com/satabin/fs2-data/tree/" + branch
  }

  val urlString = s"${url(fs2DataVersion)}/€{FILE_PATH}.scala#L1"

  val options = Seq(
    "-d",
    javadocDir.toNIO.toString,
    "-usejavacp",
    "-doc-title",
    "fs2-data API Documentation",
    "-doc-version",
    fs2DataVersion,
    "-doc-source-url",
    urlString,
    "-skip-packages",
    "better",
    "-groups",
    "-implicits",
    "-sourcepath",
    base
  ) ++ pluginOptions ++ scalacOptions

  val scalaCompilerClasspath = ev
    .evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.scalaCompilerClasspath): _*))
    .values
    .collect {
      case a: Agg[_] =>
        a.items.collect {
          case p: PathRef => p.path
        }
    }
    .flatten

  val compileClasspath = ev
    .evaluate(mill.api.Strict.Agg[define.Task[_]](modules.map(_.compileClasspath): _*))
    .values
    .collect {
      case a: Agg[_] =>
        a.items.collect {
          case p: PathRef => p
        }
    }
    .flatten

  if (files.nonEmpty)
    runSubprocess(
      "scala.tools.nsc.ScalaDoc",
      scalaCompilerClasspath ++ compileClasspath.filter(_.path.ext != "pom").map(_.path),
      mainArgs = (files ++ options).toSeq
    )

  ()
}
