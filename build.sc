import mill._
import scalalib._
import scalafmt._

import $ivy.`com.lihaoyi::mill-contrib-bloop:0.4.1`

val scala212 = "2.12.8"
val scala213 = "2.13.0"

trait Fs2DataModule extends ScalaModule with ScalafmtModule {

  def scalacOptions = Seq("-feature", "-deprecation", "-unchecked")

  def ivyDeps = Agg(ivy"co.fs2::fs2-core:1.1.0-M1")

  trait Fs2DataTests extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.8")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}

object csv extends Cross[CsvModule](scala212, scala213)

class CsvModule(val crossScalaVersion: String) extends Fs2DataModule with CrossScalaModule {

  object test extends Fs2DataTests

}
