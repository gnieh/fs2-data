package fs2.data.pattern

/** A type class that describes how the pattern type `Pat`
  * can be decomposed into [[Skeleton]]s.
  *
  * Skeletons represent `or` patterns, and are matched left to right.
  */
trait Pattern[Pat, Tag] {

  def decompose(pat: Pat): List[Skeleton[Tag]]

}
