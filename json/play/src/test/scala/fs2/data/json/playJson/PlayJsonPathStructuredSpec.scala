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

package fs2.data.json.playJson

import fs2.data.json.ast.Builder
import fs2.data.json.codec.Deserializer
import fs2.data.json.jsonpath.{Data, JsonPathStructuredSpec}
import play.api.libs.json.{JsResult, JsValue, Reads}

object PlayJsonPathStructuredSpec extends JsonPathStructuredSpec[JsValue] {

  override implicit def builder: Builder[JsValue] = PlayBuilder

  implicit val dataReads: Reads[Data] = new Reads[Data] {
    override def reads(json: JsValue): JsResult[Data] = {
      val value = (json \ "value")
      value
        .validate[Int]
        .map(Data.Number(_))
        .orElse(value.validate[Boolean].map(Data.Bool(_)))
    }
  }

  override implicit def deserializer: Deserializer[Data] = deserializerForReads[Data]

}
