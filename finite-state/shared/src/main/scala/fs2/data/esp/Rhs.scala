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
  case class Call[Tag](q: Int, depth: Depth2, params: List[Rhs[Tag]]) extends Rhs[Tag]

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

sealed trait Depth2 {
  def apply(d: Int): Int =
    this match {
      case Depth2.Value(d)  => d
      case Depth2.Copy      => d
      case Depth2.Increment => d + 1
      case Depth2.Decrement => d - 1
    }
}
object Depth2 {
  case class Value(d: Int) extends Depth2
  case object Copy extends Depth2
  case object Increment extends Depth2
  case object Decrement extends Depth2
}
