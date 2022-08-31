package fs2.data.esp

/** A typeclass to create events out of tags. */
trait Conversion[Tag, Evt] {
  def makeOpen(t: Tag): Evt
  def makeClose(t: Tag): Evt
  def makeLeaf(t: Tag): Evt
}
