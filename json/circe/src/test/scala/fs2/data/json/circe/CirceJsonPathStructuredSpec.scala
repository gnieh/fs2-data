/*
 * Copyright 2024 fs2-data Project
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
