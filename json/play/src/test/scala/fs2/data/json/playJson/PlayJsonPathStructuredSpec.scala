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
