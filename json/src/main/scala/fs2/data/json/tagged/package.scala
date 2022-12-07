package fs2.data.json

package object tagged {

  private[json] def untag(tj: TaggedJson): Option[Token] =
    tj match {
      case TaggedJson.Raw(t)                 => Some(t)
      case TaggedJson.StartArrayElement(_)   => None
      case TaggedJson.EndArrayElement        => None
      case TaggedJson.StartObjectValue(name) => Some(Token.Key(name))
      case TaggedJson.EndObjectValue         => None
    }

}
