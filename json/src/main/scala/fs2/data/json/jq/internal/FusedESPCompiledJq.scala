package fs2
package data
package json
package jq
package internal

import cats.data.NonEmptyChain
import cats.syntax.all._

import tagged._

private[jq] class FusedESPCompiledJq[F[_]: RaiseThrowable](val esps: NonEmptyChain[JqESP[F]]) extends CompiledJq[F] {

  override def apply(in: Stream[F, Token]): Stream[F, Token] =
    esps
      .foldLeft(in.through(JsonTagger.pipe))((base, esp) => base.through(esp.pipe))
      .map(untag(_))
      .unNone

  override def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      case that: ESPCompiledJq[F] =>
        new FusedESPCompiledJq[F](this.esps :+ that.esp)
      case that: FusedESPCompiledJq[F] =>
        new FusedESPCompiledJq[F](this.esps ++ that.esps)
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this +: that.jqs)
      case _ =>
        new PipedCompiledJq[F](NonEmptyChain(this, that))
    }

}

