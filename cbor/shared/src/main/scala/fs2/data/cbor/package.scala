/*
 * Copyright 2023 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
