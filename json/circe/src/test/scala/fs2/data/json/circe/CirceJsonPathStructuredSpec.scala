package fs2.data.json.circe

import fs2.data.json.ast.Builder
import fs2.data.json.codec.Deserializer
import fs2.data.json.jsonpath.{Data, JsonPathStructuredSpec}
import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.deriveDecoder

object CirceJsonPathStructuredSpec extends JsonPathStructuredSpec[Json] {

  override implicit val builder: Builder[Json] = CirceBuilder

  implicit val dataDecoder: Decoder[Data] =
    deriveDecoder[Data.Number]
      .either(deriveDecoder[Data.Bool])
      .map(_.merge)

  override implicit def deserializer: Deserializer[Data] = deserializerForDecoder[Data]

}
