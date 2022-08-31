package fs2.data.esp

/** A typeclass that describes how to transform an input tag into an output tag. */
trait Tag2Tag[InTag, OutTag] {
  def convert(tag: InTag): OutTag
}

object Tag2Tag {
  implicit def same[Tag]: Tag2Tag[Tag, Tag] = identity
}
