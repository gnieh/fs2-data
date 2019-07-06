import mill._
import scalalib._
import scalafmt._

val millVersion = System.getProperty("MILL_VERSION")
interp.load.ivy("com.lihaoyi" %% "mill-contrib-bloop" % millVersion)

@

val scala212 = "2.12.8"
val scala213 = "2.13.0"

val fs2Version = "1.1.0-M1"

trait Fs2DataModule extends ScalaModule with ScalafmtModule {

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked")

  val scala212Deps = T {
    val v = scalaVersion()
    if (v.startsWith("2.13"))
      Nil
    else
      List(ivy"org.scala-lang.modules::scala-collection-compat:2.1.1")
  }

  def ivyDeps =
    Agg(ivy"co.fs2::fs2-core:$fs2Version") ++ scala212Deps()

  trait Fs2DataTests extends Tests {
    def ivyDeps =
      Agg(
        ivy"org.scalatest::scalatest:3.0.8",
        ivy"com.github.pathikrit::better-files:3.8.0",
        ivy"io.circe::circe-parser:0.12.0-M4",
        ivy"co.fs2::fs2-io:$fs2Version"
      )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}

object csv extends Cross[CsvModule](scala212, scala213)

class CsvModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule {

  object test extends Fs2DataTests

}
