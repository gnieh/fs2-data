package fs2
package data

import html.internals._
import fs2.data.text.CharLikeChunks

package object html {

  /** Transform the input stream into a stream of HTML tokens
    * according to [[https://html.spec.whatwg.org/multipage/parsing.html#tokenization the specification]].
    *
    * This is a low-level pipe, giving access to emitted tokens.
    * It can be used to build the DOM or another representation of the page.
    * This also means that every character is emitted as a single token, including new lines.
    * Depending on the mode, new lines may have different semantics, and this pipe allows for
    * the full flexibility to build higher-level tools.
    */
  def tokens[F[_], T](implicit F: RaiseThrowable[F], T: CharLikeChunks[F, T]): Pipe[F, T, HtmlToken] =
    new HtmlTokenizer[F, T].pipe

}
