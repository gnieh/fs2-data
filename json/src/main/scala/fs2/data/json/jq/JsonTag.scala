package fs2.data.json.jq

private sealed trait JsonTag
private object JsonTag {
  case object Object extends JsonTag
  case object Array extends JsonTag
  case class ObjectKey(key: Option[String]) extends JsonTag
  case class ArrayElement(idx: Option[Int]) extends JsonTag
  case class Bool(b: Option[Boolean]) extends JsonTag
  case class Str(s: Option[String]) extends JsonTag
  case class Num(n: Option[String]) extends JsonTag
  case object Null extends JsonTag
}
