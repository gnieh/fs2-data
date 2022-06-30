package fs2.data.cbor

import cats.syntax.all._

import scodec.bits.ByteVector

import io.circe.{Decoder, Json}
import io.circe.generic.semiauto._

case class AppendixTestCase(cbor: ByteVector,
                            hex: String,
                            roundtrip: Boolean,
                            decoded: Option[Json],
                            diagnostic: Option[String],
                            diagnosticAlt: Option[String])

object AppendixTestCase {

  implicit val bvDecoder: Decoder[ByteVector] = Decoder.decodeString.emap(s =>
    ByteVector.fromBase64(s).liftTo[Either[String, *]](s"'$s' is not a valid base64 string"))

  implicit val decoder: Decoder[AppendixTestCase] = deriveDecoder

}
