package fs2.data.utils

import cats._
import cats.syntax.all._

final class CharMap[+V] private (val underlying: IntMap[V]) extends AnyVal {
  import CharMap._

  @inline
  def get(key: Char): Option[V] =
    underlying.get(wrap(key))

  @inline
  def contains(key: Char): Boolean =
    underlying.contains(wrap(key))

  @inline
  def updated[V1 >: V](key: Char, value: V1): CharMap[V1] =
    new CharMap(underlying.updated(wrap(key), value))

  @inline
  def updated[V1 >: V](key: Char, value: V1, combine: (V1, V) => V1): CharMap[V1] =
    new CharMap(underlying.updated(wrap(key), value, combine))

  @inline
  final def mapValues[V1](f: V => V1): CharMap[V1] =
    new CharMap(underlying.mapValues(f))

  @inline
  final def mapValuesWithKey[V1](f: (Char, V) => V1): CharMap[V1] =
    new CharMap(underlying.mapValuesWithKey((k, v) => f(unwrap(k), v)))

  @inline
  def removed(key: Char): CharMap[V] =
    new CharMap(underlying.removed(wrap(key)))

  @inline
  def mergeWithKey[V1, V2](combine: (Char, V, V1) => Option[V2],
                           mapV: CharMap[V] => CharMap[V2],
                           mapV1: CharMap[V1] => CharMap[V2])(that: CharMap[V1]): CharMap[V2] =
    new CharMap(
      underlying.mergeWithKey[V1, V2]((k, v, v1) => combine(unwrap(k), v, v1),
                                      m => mapV(new CharMap(m)).underlying,
                                      m => mapV1(new CharMap(m)).underlying)(that.underlying))

  @inline
  def union[V1 >: V](that: CharMap[V1]): CharMap[V1] =
    new CharMap(underlying.union(that.underlying))

  @inline
  final def unionWith[V1 >: V](that: CharMap[V1], f: (V1, V1) => V1): CharMap[V1] =
    new CharMap(underlying.unionWith(that.underlying, f))

  @inline
  def diff[V1 >: V](that: CharMap[V1]): CharMap[V1] =
    new CharMap(underlying.diff(that.underlying))

  @inline
  def intersect[V1 >: V](that: CharMap[V1]): CharMap[V1] =
    new CharMap(underlying.intersect(that.underlying))

  @inline
  def isEmpty: Boolean =
    underlying.isEmpty

  @inline
  def size: Int =
    underlying.size

  @inline
  def foldLeft[Res](acc: Res)(f: (Res, V) => Res): Res =
    underlying.foldLeft(acc)(f)

  @inline
  def foldLeftWithKey[Res](acc: Res)(f: (Res, Char, V) => Res): Res =
    underlying.foldLeftWithKey(acc)((acc, k, v) => f(acc, unwrap(k), v))

  @inline
  def foldRight[Res](acc: Eval[Res])(f: (V, Eval[Res]) => Eval[Res]): Eval[Res] =
    underlying.foldRight(acc)(f)

  @inline
  def foldRightWithKey[Res](acc: Eval[Res])(f: (Char, V, Eval[Res]) => Eval[Res]): Eval[Res] =
    underlying.foldRightWithKey(acc)((k, v, acc) => f(unwrap(k), v, acc))

  def toList: List[(Char, V)] =
    foldRightWithKey(Eval.now(List.empty[(Char, V)])) { (k, v, acc) =>
      acc.map((k, v) :: _)
    }.value

}

object CharMap {

  @inline
  private def wrap(c: Char): Int =
    0xffff & c

  @inline
  private def unwrap(i: Int): Char =
    (0xffff & i).toChar

  @inline
  def empty[V]: CharMap[V] =
    new CharMap(IntMap.empty)

  @inline
  def one[V](key: Char, value: V): CharMap[V] =
    new CharMap(IntMap.one(wrap(key), value))

  @inline
  def apply[V](bindings: (Char, V)*): CharMap[V] =
    bindings.toList.foldLeft(empty[V]) { case (acc, (k, v)) => acc.updated(k, v) }

  implicit def CharMapMonoidValueMonoid[V: Semigroup]: Monoid[CharMap[V]] =
    new Monoid[CharMap[V]] {

      override def combine(x: CharMap[V], y: CharMap[V]): CharMap[V] =
        x.unionWith(y, _ combine _)

      override def empty: CharMap[V] =
        CharMap.empty

    }

  implicit object CharMapTraverse extends Traverse[CharMap] {

    override def foldLeft[A, B](fa: CharMap[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: CharMap[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldRight(lb)(f)

    override def traverse[G[_]: Applicative, A, B](fa: CharMap[A])(f: A => G[B]): G[CharMap[B]] =
      fa.underlying.traverse(f).map(new CharMap(_))

  }

  implicit def CharMapEq[V: Eq]: Eq[CharMap[V]] = new Eq[CharMap[V]] {

    override def eqv(x: CharMap[V], y: CharMap[V]): Boolean =
      x.underlying === y.underlying

    override def neqv(x: CharMap[V], y: CharMap[V]): Boolean =
      x.underlying =!= y.underlying

  }

  implicit def CharMapShow[V: Show]: Show[CharMap[V]] =
    Show.show(_.mapValuesWithKey((k, v) => show"$k -> $v").mkString_("CharMap(", ", ", ")"))

}
