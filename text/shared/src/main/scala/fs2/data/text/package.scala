/*
 * Copyright 2019-2022 Lucas Satabin
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

import java.nio.charset.{Charset, StandardCharsets}

package object text {

  /** Import this if your byte stream is encoded in UTF-8 */
  object utf8 {

    implicit def byteStreamCharLike[F[_]]: CharLikeChunks[F, Byte] = {
      val stringsCharLike = CharLikeChunks.stringStreamCharLike[F]
      new CharLikeChunks[F, Byte] {
        override type Context = stringsCharLike.Context
        override def create(s: Stream[F, Byte]): Context = stringsCharLike.create(s.through(fs2.text.utf8.decode))
        override def needsPull(ctx: Context): Boolean = stringsCharLike.needsPull(ctx)
        override def pullNext(ctx: Context): Pull[F, Nothing, Option[Context]] = stringsCharLike.pullNext(ctx)
        override def advance(ctx: Context): Context = stringsCharLike.advance(ctx)
        override def current(ctx: Context): Char = stringsCharLike.current(ctx)
      }
    }

  }

  /** Import this if your byte stream is encoded in ISO-8859-1 (aka latin1) */
  object latin1 {

    implicit def byteStreamCharLike[F[_]]: CharLikeChunks[F, Byte] =
      new CharLikeSingleByteChunks[F](StandardCharsets.ISO_8859_1)

  }

  /** Import this if your byte stream is encoded in ISO-8859-15 (aka latin9) */
  object latin9 {

    implicit def byteStreamCharLike[F[_]]: CharLikeChunks[F, Byte] =
      new CharLikeSingleByteChunks[F](Charset.forName("ISO-8859-15"))

  }

  /** Import this if your byte stream is encoded in ASCII */
  object ascii {

    implicit def byteStreamCharLike[F[_]]: CharLikeChunks[F, Byte] =
      new CharLikeSingleByteChunks[F](StandardCharsets.US_ASCII)

  }

}
