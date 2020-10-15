package fs2
package data
package cbor

import low.internal._

package object low {

  /** Parses the input byte stream into a sequence of low-level CBOR items.
    * This allows for parsing arbitrary long and deep CBOR data. No AST
    * is built.
    */
  def items[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, Byte, CborItem] =
    ItemParser.pipe[F]

}
