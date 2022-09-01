package fs2.data.esp

/** This is thre right-hand side of an ESP rule processor.
  * It can access the context, meaning:
  *  - the rule parameters
  *  - the current state depth
  *  - the captured inputs
  */
sealed trait Rhs[+OutTag]
object Rhs {

  /** Calls a new rule at a given depth with the given parameters. */
  case class Call[Tag](q: Int, depth: Depth, params: List[Rhs[Tag]]) extends Rhs[Tag]

  /** Reads the rule parameter. */
  case class Param(n: Int) extends Rhs[Nothing]

  /** Empty RHS. */
  case object Epsilon extends Rhs[Nothing]

  /** Builds a tree. */
  case class Tree[OutTag](tag: OutTag, inner: Rhs[OutTag]) extends Rhs[OutTag]

  /** Builds a tree with the captured node tag in pattern. */
  case class CapturedTree[OutTag](name: String, inner: Rhs[OutTag]) extends Rhs[OutTag]

  /** Emits a leaf value. */
  case class Leaf[OutTag](value: OutTag) extends Rhs[OutTag]

  /** Emits the captured input value. */
  case class CapturedLeaf(name: String) extends Rhs[Nothing]

  /** Concatenates two RHS. */
  case class Concat[OutTag](fst: Rhs[OutTag], snd: Rhs[OutTag]) extends Rhs[OutTag]

  def epsilon[OutTag]: Rhs[OutTag] = Epsilon

}

sealed trait Depth {
  def apply(d: Int): Int =
    this match {
      case Depth.Value(d)  => d
      case Depth.Copy      => d
      case Depth.Increment => d + 1
      case Depth.Decrement => d - 1
    }
}
object Depth {
  case class Value(d: Int) extends Depth
  case object Copy extends Depth
  case object Increment extends Depth
  case object Decrement extends Depth
}
