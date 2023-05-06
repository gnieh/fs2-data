package fs2
package data
package json
package jq

import fs2.data.json.Token
import cats.data.NonEmptyChain

/** Represents a sequence of piped compiled jq queries, applying every query to the result of the previous one. */
class PipedCompiledJq[F[_]](val jqs: NonEmptyChain[CompiledJq[F]]) extends CompiledJq[F] {

  override def apply(in: fs2.Stream[F, Token]): fs2.Stream[F, Token] =
    jqs.foldLeft(in)((base, jq) => base.through(jq))

  override def andThen(that: CompiledJq[F]): CompiledJq[F] =
    that match {
      case that: PipedCompiledJq[F] =>
        new PipedCompiledJq[F](this.jqs ++ that.jqs)
      case _ =>
        new PipedCompiledJq[F](this.jqs :+ that)
    }

}
