package fs2.data.matching

/** Represents a lookup table. This is useful to implement
  * finite state automata or transducers.
  */
trait Table[T] {
  type In
  type Out

  def get(table: T)(in: In): Option[Out]
}

object Table extends LowPriorityImplicits {
  type Aux[T, I, O] = Table[T] {
    type In = I
    type Out = O
  }

  implicit def MapTable[K, V]: Table.Aux[Map[K, V], K, V] = new Table[Map[K, V]] {
    type In = K
    type Out = V

    override def get(table: Map[K, V])(in: K): Option[V] = table.get(in)

  }
}

sealed trait LowPriorityImplicits {

  implicit def FunctionTable[I, O]: Table.Aux[Function[I, O], I, O] = new Table[Function[I, O]] {
    type In = I
    type Out = O

    override def get(table: Function[I, O])(in: I): Option[O] = Some(table(in))

  }

  implicit def PartialFunctionTable[I, O]: Table.Aux[PartialFunction[I, O], I, O] = new Table[PartialFunction[I, O]] {
    type In = I
    type Out = O

    override def get(table: PartialFunction[I, O])(in: I): Option[O] = table.lift(in)

  }

}
