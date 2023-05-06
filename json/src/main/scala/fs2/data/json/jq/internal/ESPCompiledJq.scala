package fs2
package data
package json
package jq
package internal

import cats.data.NonEmptyChain

import tagged._

private[jq] class ESPCompiledJq[F[_]: RaiseThrowable](val esp: JqESP[F]) extends CompiledJq[F] {

  def apply(in: Stream[F, Token]): Stream[F, Token] =
    in.through(JsonTagger.pipe).through(esp.pipe).map(untag(_)).unNone

  def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      case that: ESPCompiledJq[F] =>
        // fuse them to avoid tagging/untagging between each stage
        new FusedESPCompiledJq[F](NonEmptyChain(this.esp, that.esp))
      case that: FusedESPCompiledJq[F] =>
        new FusedESPCompiledJq[F](this.esp +: that.esps)
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this +: that.jqs)
      case _ =>
        // no idea how to fuse them, be naive
        new PipedCompiledJq[F](NonEmptyChain(this, that))
    }

}
