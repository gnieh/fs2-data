/*
 * Copyright 2019 Lucas Satabin
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

import java.nio.{CharBuffer => JCharBuffer}

import csv.internals._
import cats._
import cats.data._
import cats.implicits._

import scala.language.higherKinds

package object csv {

  type DecoderResult[T] = Either[DecoderError, T]

  type HeaderResult[T] = Either[HeaderError, T]

  type CsvNelRow[HeadElem] = CsvRow[NonEmptyList[HeadElem]]

  type CsvNelRowDecoder[T, HeadElem] = CsvRowDecoder[T, NonEmptyList[HeadElem]]

  object CsvNelRowDecoder {
    def apply[T, HeadElem](implicit dec: CsvNelRowDecoder[T, HeadElem]): CsvNelRowDecoder[T, HeadElem] = dec
  }

  type ParseableNelHeader[HeadElem] = ParseableHeader[NonEmptyList[HeadElem]]

  object ParseableNelHeader {
    def apply[HeadElem](implicit dec: ParseableNelHeader[HeadElem]): ParseableNelHeader[HeadElem] = dec
  }

  /** Transforms a stream of characters into a stream of CSV rows.
    */
  def rows[F[_]](separator: Char = ',')(
      implicit F: ApplicativeError[F, Throwable]): Pipe[F, Char, NonEmptyList[String]] =
    RowParser.pipe[F](separator)

  /** Transforms a stream of strings into a stream of CSV rows. Helper for streams which are not initially char-based,
   *  prefer to use [[rows]] otherwise.
   */
  def rowsFromStrings[F[_]](separator: Char = ',')(
    implicit F: ApplicativeError[F, Throwable]): Pipe[F, String, NonEmptyList[String]] =
    _.flatMap(s => Stream.chunk(Chunk.charBuffer(JCharBuffer.wrap(s)))) through RowParser.pipe[F](separator)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def headers[F[_], Header](implicit F: ApplicativeError[F, Throwable],
                            Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    CsvRowParser.pipe[F, Header](true)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with a NonEmptyList of independent headers. */
  def nelHeaders[F[_], HeadElem](implicit F: ApplicativeError[F, Throwable],
                                 Header: ParseableNelHeader[HeadElem]): Pipe[F, NonEmptyList[String], CsvNelRow[HeadElem]] =
    CsvRowParser.pipe[F, NonEmptyList[HeadElem]](true)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def noHeaders[F[_]](implicit F: ApplicativeError[F, Throwable]): Pipe[F, NonEmptyList[String], CsvRow[Nothing]] =
    CsvRowParser.pipe[F, Nothing](false)

  def decode[F[_], R](implicit F: ApplicativeError[F, Throwable], R: RowDecoder[R]): Pipe[F, NonEmptyList[String], R] =
    _.evalMap(R(_).liftTo[F])

  def attemptDecode[F[_], R](implicit F: Applicative[F],
                             R: RowDecoder[R]): Pipe[F, NonEmptyList[String], DecoderResult[R]] =
    _.map(R(_))

  def decodeRow[F[_], Header, R](implicit F: ApplicativeError[F, Throwable],
                                 R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], R] =
    _.evalMap(R(_).liftTo[F])

  def attemptDecodeRow[F[_], Header, R](implicit F: Applicative[F],
                                        R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], DecoderResult[R]] =
    _.map(R(_))

}
