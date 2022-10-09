/*
 * Copyright 2022 Lucas Satabin
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

package fs2.data.cbor

import cats.syntax.all._

import scodec.bits.ByteVector

import io.circe.{Decoder, Json}
import io.circe.generic.semiauto._

case class AppendixTestCase(cbor: ByteVector,
                            hex: String,
                            roundtrip: Boolean,
                            decoded: Option[Json],
                            decodedAlt: Option[Json],
                            diagnostic: Option[String])

object AppendixTestCase {

  implicit val bvDecoder: Decoder[ByteVector] = Decoder.decodeString.emap(s =>
    ByteVector.fromBase64(s).liftTo[Either[String, *]](s"'$s' is not a valid base64 string"))

  implicit val decoder: Decoder[AppendixTestCase] = deriveDecoder

}
