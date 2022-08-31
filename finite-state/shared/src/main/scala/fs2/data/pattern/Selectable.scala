package fs2.data.pattern

/** Describes the structure of an expression in term of constructor
  * trees that can be selected.
  */
trait Selectable[Expr, Tag] {
  def tree(e: Expr): ConstructorTree[Tag]
}

case class ConstructorTree[Tag](tag: Tag, args: List[ConstructorTree[Tag]]) {
  def select(sel: Selector[Tag]): Option[ConstructorTree[Tag]] =
    sel match {
      case Selector.Root => Some(this)
      case Selector.Sel(parent, tag, n) =>
        select(parent).flatMap { const =>
          if (const.tag == tag)
            const.args.lift(n)
          else
            None
        }
    }
}
object ConstructorTree {
  def noArgConstructor[Tag](tag: Tag): ConstructorTree[Tag] =
    ConstructorTree(tag, Nil)
}
