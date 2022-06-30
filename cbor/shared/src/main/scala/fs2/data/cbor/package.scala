package fs2
package data

package object cbor {

  /** Useful for debugging, generates a stream of diagnostic strings
  * representing the CBOR values in the input stream as defined in
  * [section 8 of RFC8949](https://www.rfc-editor.org/rfc/rfc8949.html#name-diagnostic-notation).
  */
  def diagnostic[F[_]](implicit F: RaiseThrowable[F]): Pipe[F, low.CborItem, String] =
    Diagnostic[F](_)

  /** A debugging `Pipe`, useful to use in conjunction with `observe`.
    * {{{
    * bytes.through(items).observe(debugDiagnostic()).compile.toList
    * }}}
    */
  def debugDiagnostic[F[_]](logger: String => Unit = println(_))(implicit
      F: RaiseThrowable[F]): Pipe[F, low.CborItem, Nothing] =
    s => Diagnostic[F](s).debug(logger = logger).drain

}
