import mill._
import mill.scalalib._

import scala.util.matching.Regex

/**
  * By default the resulting documents are simply placed in the Mill build output folder but they can be placed elsewhere by overriding the [[mill.contrib.mdoc.MdocModule#mdocTargetDirectory]] task.
  *
  * For example:
  *
  * {{{
  * // build.sc
  * import mill._, scalalib._, contrib.mdoc.__
  *
  * object example extends MdocModule {
  *   def scalaVersion = "2.12.8"
  *   def mdocVersion = "1.2.10"
  * }
  * }}}
  *
  * This defines a project with the following layout:
  *
  * {{{
  * build.sc
  * example/
  *     src/
  *     mdoc/
  *     resources/
  * }}}
  *
  * In order to compile documentation we can execute the `mdoc` task in the module:
  *
  * {{{
  * sh> mill example.mdoc
  * }}}
  */
trait MdocModule extends ScalaModule {
  /**
    * This task determines where documentation files must be placed in order to be compiled with Mdoc. By default this is the `docs` folder at the root of the module.
    */
  def mdocSourceDirectory = T.sources { millSourcePath / 'docs }

  /**
    * A task which determines where the compiled documentation files will be placed. By default this is simply the Mill build's output folder for this task,
    * but this can be reconfigured so that documentation goes to the root of the module (e.g. `millSourcePath`) or to a dedicated folder (e.g. `millSourcePath / 'docs`)
    */
  def mdocTargetDirectory: T[os.Path] = T { T.ctx().dest }

  /**
    * A task defining the site mapping. By default it is empty.
    */
  def mdocSite: T[Map[String, String]] = T { Map.empty[String, String] }

  /**
    * A task which determines what classpath is used when compiling documentation. By default this is configured to use the same inputs as the [[mill.contrib.mdoc.MdocModule#runClasspath]],
    * except for using [[mill.contrib.mdoc.MdocModule#mdocIvyDeps]] rather than the module's [[mill.contrib.mdoc.MdocModule#runIvyDeps]].
    */
  def mdocClasspath: T[Agg[PathRef]] = T {
    // Same as runClasspath but with mdoc added to ivyDeps from the start
    // This prevents duplicate, differently versioned copies of scala-library ending up on the classpath which can happen when resolving separately
    transitiveLocalClasspath() ++
    resources() ++
    localClasspath() ++
    unmanagedClasspath() ++
    mdocIvyDeps()
  }

  /**
    * A task which determines the scalac plugins which will be used when compiling code examples with Mdoc. The default is to use the [[mill.contrib.mdoc.MdocModule#scalacPluginIvyDeps]] for the module.
    */
  def mdocScalacPluginIvyDeps: T[Agg[Dep]] = scalacPluginIvyDeps()

  /**
    * A [[scala.util.matching.Regex]] task which will be used to determine which files should be compiled with mdoc. The default glob is `None`.
    */
  def mdocNameFilter: T[Option[String]] = T { None }

  /**
    * The scalac options which will be used when compiling code examples with Mdoc. The default is to use the [[mill.contrib.mdoc.MdocModule#scalacOptions]] for the module,
    * but filtering out options which are problematic in the REPL, e.g. `-Xfatal-warnings`, `-Ywarn-unused-imports`.
    */
  def mdocScalacOptions: T[Seq[String]] =
    scalacOptions().filterNot(Set(
      "-Ywarn-unused:imports",
      "-Ywarn-unused-import",
      "-Ywarn-dead-code",
      "-Xfatal-warnings"
    ))

  /**
    * The version of Mdoc to use.
    */
  def mdocVersion: T[String]

  /**
    * A task which determines how to fetch the Mdoc jar file and all of the dependencies required to compile documentation for the module and returns the resulting files.
    */
  def mdocIvyDeps: T[Agg[PathRef]] = T {
    Lib.resolveDependencies(
      repositories,
      Lib.depToDependency(_, scalaVersion()),
      compileIvyDeps() ++ transitiveIvyDeps() ++ Seq(
        ivy"org.scalameta::mdoc:${mdocVersion()}"
      )
    )
  }

  /**
    * A task which performs the dependency resolution for the scalac plugins to be used with Mdoc.
    */
  def mdocPluginJars: T[Agg[PathRef]] = resolveDeps(mdocScalacPluginIvyDeps)()

  /**
    * Run Mdoc using the configuration specified in this module. The working directory used is the [[mill.contrib.mdoc.MdocModule#millSourcePath]].
    */
  def mdoc: T[os.CommandResult] = T {
    val in = mdocSourceDirectory().head.path.toIO.getAbsolutePath
    val out = mdocTargetDirectory().toIO.getAbsolutePath
    val re = mdocNameFilter()
    val site = mdocSite().flatMap { case (k, v) => Seq(s"site.$k", v) }
    val opts = mdocScalacOptions()
    val pOpts = mdocPluginJars().map(pathRef => "-Xplugin:" + pathRef.path.toIO.getAbsolutePath)
    val mdocArgs = List("--in", in, "--out", out, "--scalac-options", (opts ++ pOpts).mkString(" "), "--no-link-hygiene") ++ re.toSeq.flatMap(Seq("--include", _)) ++ site
    os.proc(
      'java,
      "-cp", mdocClasspath().map(_.path.toIO.getAbsolutePath).mkString(java.io.File.pathSeparator),
      "mdoc.Main",
      mdocArgs
    ).call(os.Path(in))
  }
}
