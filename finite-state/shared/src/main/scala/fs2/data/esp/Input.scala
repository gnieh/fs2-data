package fs2.data
package esp

import pattern._

case class Input[Evt](state: Int, depth: Int, evt: Option[Evt])

object Input {

  implicit def InputSelectable[Evt, T](implicit Evt: Selectable[Evt, Tag[T]]): Selectable[Input[Evt], Tag[T]] =
    new Selectable[Input[Evt], Tag[T]] {

      override def tree(e: Input[Evt]): ConstructorTree[Tag[T]] =
        ConstructorTree(
          Tag.Input,
          List(
            ConstructorTree.noArgConstructor(Tag.State(e.state)),
            ConstructorTree.noArgConstructor(Tag.Depth(e.depth)),
            e.evt.fold[ConstructorTree[Tag[T]]](ConstructorTree.noArgConstructor(Tag.End))(Evt.tree(_))
          )
        )

    }

}
