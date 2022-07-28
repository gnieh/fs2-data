package fs2.data.utils

import cats._
import cats.syntax.all._

import scala.annotation.{tailrec, nowarn}
import cats.data.NonEmptyList

sealed trait IntMap[+V] {

  import IntMap._

  @tailrec
  final def get(key: Int): Option[V] =
    this match {
      case Empty              => none
      case Leaf(k, v)         => if (k === key) v.some else none
      case Branch(_, m, l, r) => if (zero(key, m)) l.get(key) else r.get(key)
    }

  @tailrec
  final def contains(key: Int): Boolean =
    this match {
      case Empty      => false
      case Leaf(k, _) => key === k
      case Branch(p, m, l, r) =>
        if (nomatch(key, p, m))
          false
        else if (zero(key, m))
          l.contains(key)
        else
          r.contains(key)
    }

  final def updated[V1 >: V](key: Int, value: V1): IntMap[V1] =
    this match {
      case Empty =>
        Leaf(key, value)
      case Leaf(k, _) =>
        if (k == key)
          Leaf(key, value)
        else
          link(key, Leaf(key, value), k, this)
      case Branch(p, m, l, r) =>
        if (nomatch(key, p, m))
          link(key, Leaf(key, value), p, this)
        else if (zero(key, m))
          Branch(p, m, l.updated(key, value), r)
        else
          Branch(p, m, l, r.updated(key, value))
    }

  final def updated[V1 >: V](key: Int, value: V1, combine: (V1, V) => V1): IntMap[V1] =
    this match {
      case Empty =>
        Leaf(key, value)
      case Leaf(k, v) =>
        if (k == key)
          Leaf(key, combine(value, v))
        else
          link(key, Leaf(key, value), k, this)
      case Branch(p, m, l, r) =>
        if (nomatch(key, p, m))
          link(key, Leaf(key, value), p, this)
        else if (zero(key, m))
          Branch(p, m, l.updated(key, value, combine), r)
        else
          Branch(p, m, l, r.updated(key, value, combine))
    }

  final def mapValues[V1](f: V => V1): IntMap[V1] =
    mapValuesWithKey((_, v) => f(v))

  final def mapValuesWithKey[V1](f: (Int, V) => V1): IntMap[V1] = {

    def go(stack: NonEmptyList[Frame[V, V1]]): IntMap[V1] =
      stack match {
        case NonEmptyList(Frame.New(p, m, l, r), stack1) =>
          l match {
            case Empty                  => go(NonEmptyList(Frame.LeftDone[V, V1](p, m, Empty, r), stack1))
            case Leaf(k, v)             => go(NonEmptyList(Frame.LeftDone[V, V1](p, m, Leaf(k, f(k, v)), r), stack1))
            case Branch(p1, m1, l1, r1) => go(Frame.New[V, V1](p1, m1, l1, r1) :: stack)
          }
        case NonEmptyList(Frame.LeftDone(p, m, l, r), stack1) =>
          r match {
            case Empty                  => go(NonEmptyList(Frame.Done[V, V1](p, m, l, Empty), stack1))
            case Leaf(k, v)             => go(NonEmptyList(Frame.Done[V, V1](p, m, l, Leaf(k, f(k, v))), stack1))
            case Branch(p1, m1, l1, r1) => go(Frame.New[V, V1](p1, m1, l1, r1) :: stack)
          }
        case NonEmptyList(Frame.Done(p, m, l, r), stack) =>
          val res = Branch(p, m, l, r)
          stack match {
            case Nil => res
            case Frame.New(p1, m1, _, r1) :: stack =>
              go(NonEmptyList(Frame.LeftDone(p1, m1, res, r1), stack))
            case Frame.LeftDone(p1, m1, l1, _) :: stack =>
              go(NonEmptyList(Frame.Done[V, V1](p1, m1, l1, res), stack))
            case Frame.Done(p1, m1, l1, r1) :: _ =>
              throw new Exception("THIS IS A BUG")
          }
      }

    this match {
      case Empty              => Empty
      case Leaf(k, v)         => Leaf(k, f(k, v))
      case Branch(p, m, l, r) => go(NonEmptyList.one(Frame.New(p, m, l, r)))
    }
  }

  final def removed(key: Int): IntMap[V] =
    this match {
      case Empty =>
        Empty
      case Leaf(k, _) =>
        if (k == key)
          Empty
        else
          this
      case Branch(p, m, l, r) =>
        if (nomatch(key, p, m))
          this
        else if (zero(key, m))
          branchLeft(p, m, l.removed(key), r)
        else
          branchRight(p, m, l, r.removed(key))
    }

  final def mergeWithKey[V1, V2](combine: (Int, V, V1) => Option[V2],
                                 mapV: IntMap[V] => IntMap[V2],
                                 mapV1: IntMap[V1] => IntMap[V2])(that: IntMap[V1]): IntMap[V2] = {
    def comb: PartialFunction[(IntMap[V], IntMap[V1]), IntMap[V2]] = { case (Leaf(k1, v1), Leaf(_, v2)) =>
      combine(k1, v1, v2) match {
        case None    => Empty
        case Some(v) => Leaf(k1, v)
      }
    }
    mergeWithKey1[V, V1, V2](branch, comb(_, _), mapV, mapV1, this, that)
  }

  final def union[V1 >: V](that: IntMap[V1]): IntMap[V1] =
    mergeWithKey1[V1, V1, V1](Branch(_, _, _, _), (t, _) => t, identity, identity, this, that)

  final def unionWith[V1 >: V](that: IntMap[V1], f: (V1, V1) => V1): IntMap[V1] =
    mergeWithKey1[V1, V1, V1](Branch(_, _, _, _),
                              { case (Leaf(k1, v1), Leaf(_, v2)) => Leaf(k1, f(v1, v2)) }: @nowarn,
                              identity,
                              identity,
                              this,
                              that)

  final def diff[V1](that: IntMap[V1]): IntMap[V] =
    mergeWithKey[V1, V]((_, _, _) => none, identity, _ => Empty)(that)

  final def intersect[V1](that: IntMap[V1]): IntMap[V] =
    mergeWithKey1[V, V1, V](branch, (t, _) => t, _ => Empty, _ => Empty, this, that)

  def isEmpty: Boolean =
    this == Empty

  def size: Int = {
    def loop(t: IntMap[V], acc: Int): Int =
      t match {
        case Empty              => acc
        case Leaf(_, _)         => acc + 1
        case Branch(_, _, l, r) => loop(r, loop(l, acc))
      }
    loop(this, 0)
  }

  def foldLeft[Res](acc: Res)(f: (Res, V) => Res): Res =
    foldLeftWithKey(acc)((acc, _, v) => f(acc, v))

  def foldLeftWithKey[Res](acc: Res)(f: (Res, Int, V) => Res): Res = {
    @tailrec
    def go(acc: Res, stack: List[IntMap[V]]): Res =
      stack match {
        case Nil => acc
        case t :: stack =>
          t match {
            case Empty              => go(acc, stack)
            case Leaf(k, v)         => go(f(acc, k, v), stack)
            case Branch(_, _, l, r) => go(acc, l :: r :: stack)
          }
      }
    this match {
      case Branch(_, m, l, r) =>
        if (m < 0)
          go(acc, List(r, l))
        else
          go(acc, List(l, r))
      case _ =>
        go(acc, List(this))
    }
  }

  def foldRight[Res](acc: Eval[Res])(f: (V, Eval[Res]) => Eval[Res]): Eval[Res] =
    foldRightWithKey(acc)((_, v, acc) => f(v, acc))

  def foldRightWithKey[Res](acc: Eval[Res])(f: (Int, V, Eval[Res]) => Eval[Res]): Eval[Res] = {
    @tailrec
    def go(acc: Res, stack: List[IntMap[V]]): Res =
      stack match {
        case Nil =>
          acc
        case t :: stack =>
          t match {
            case Empty              => go(acc, stack)
            case Leaf(k, v)         => go(f(k, v, Eval.now(acc)).value, stack)
            case Branch(_, _, l, r) => go(acc, r :: l :: stack)
          }
      }
    this match {
      case Branch(_, m, l, r) =>
        if (m < 0)
          acc.map(go(_, List(l, r)))
        else
          acc.map(go(_, List(r, l)))
      case _ =>
        acc.map(go(_, List(this)))
    }
  }

  def toList: List[(Int, V)] =
    foldRightWithKey(Eval.now(List.empty[(Int, V)])) { (k, v, acc) =>
      acc.map((k, v) :: _)
    }.value

}

object IntMap {

  private case class Branch[V](prefix: Int, mask: Int, left: IntMap[V], right: IntMap[V]) extends IntMap[V]
  private case class Leaf[V](key: Int, value: V) extends IntMap[V]
  private case object Empty extends IntMap[Nothing]

  def empty[V]: IntMap[V] = Empty

  def one[V](key: Int, value: V): IntMap[V] =
    Leaf(key, value)

  def apply[V](bindings: (Int, V)*): IntMap[V] =
    bindings.toList.foldLeft(empty[V]) { case (acc, (k, v)) => acc.updated(k, v) }

  // utilities

  @inline
  private def zero(i: Int, m: Int): Boolean =
    (i & m) == 0

  @inline
  private def nomatch(i: Int, p: Int, m: Int): Boolean =
    mask(i, m) != p

  @inline
  private def mask(i: Int, m: Int): Int =
    (i & ((-m) ^ m))

  @inline
  def shorter(m1: Int, m2: Int): Boolean =
    Integer.compareUnsigned(m1, m2) > 0

  @inline
  private def branchMask(p1: Int, p2: Int): Int =
    Integer.highestOneBit(p1 ^ p2)

  @inline
  private def link[V](p1: Int, t1: IntMap[V], p2: Int, t2: IntMap[V]): IntMap[V] =
    linkWithMask(branchMask(p1, p2), p1, t1, t2)

  @inline
  private def linkWithMask[V](m: Int, p1: Int, t1: IntMap[V], t2: IntMap[V]): IntMap[V] = {
    val p = mask(p1, m)
    if (zero(p1, m))
      Branch(p, m, t1, t2)
    else
      Branch(p, m, t2, t1)
  }

  // ensure branches do not have empty child
  private def branch[V](p: Int, m: Int, l: IntMap[V], r: IntMap[V]): IntMap[V] =
    (l, r) match {
      case (_, Empty) => l
      case (Empty, _) => r
      case (_, _)     => Branch(p, m, l, r)
    }

  private def branchLeft[V](p: Int, m: Int, l: IntMap[V], r: IntMap[V]): IntMap[V] =
    l match {
      case Empty => r
      case _     => Branch(p, m, l, r)
    }

  private def branchRight[V](p: Int, m: Int, l: IntMap[V], r: IntMap[V]): IntMap[V] =
    r match {
      case Empty => l
      case _     => Branch(p, m, l, r)
    }

  private def mergeWithKey1[V1, V2, V](br: (Int, Int, IntMap[V], IntMap[V]) => IntMap[V],
                                       combine: (IntMap[V1], IntMap[V2]) => IntMap[V],
                                       mapV1: IntMap[V1] => IntMap[V],
                                       mapV2: IntMap[V2] => IntMap[V],
                                       t1: IntMap[V1],
                                       t2: IntMap[V2]): IntMap[V] = {
    def maybeLink(p1: Int, t1: IntMap[V], p2: Int, t2: IntMap[V]): IntMap[V] =
      (t1, t2) match {
        case (Empty, _) => t2
        case (_, Empty) => t1
        case (_, _)     => link(p1, t1, p2, t2)
      }
    def go(t1: IntMap[V1], t2: IntMap[V2]): IntMap[V] =
      (t1, t2) match {
        case (Branch(p1, m1, l1, r1), Branch(p2, m2, l2, r2)) =>
          if (shorter(m1, m2)) {
            if (nomatch(p2, p1, m1))
              maybeLink(p1, mapV1(t1), p2, mapV2(t2))
            else if (zero(p2, m1))
              br(p1, m1, go(l1, t2), mapV1(r1))
            else
              br(p1, m1, mapV1(l1), go(r1, t2))
          } else if (shorter(m2, m1)) {
            if (nomatch(p1, p2, m2))
              maybeLink(p1, mapV1(t1), p2, mapV2(t2))
            else if (zero(p1, m2))
              br(p2, m2, go(t1, l2), mapV2(r2))
            else
              br(p2, m2, mapV2(l2), go(t1, r2))
          } else if (p1 === p2) {
            br(p1, m1, go(l1, l2), go(r1, r2))
          } else {
            maybeLink(p1, mapV1(t1), p2, mapV2(t2))
          }
        case (Branch(_, _, _, _), Leaf(k2, _)) =>
          def merge0(t2: IntMap[V2], k2: Int, t1: IntMap[V1]): IntMap[V] =
            t1 match {
              case Branch(p1, m1, l1, r1) =>
                if (nomatch(k2, p1, m1))
                  maybeLink(p1, mapV1(t1), k2, mapV2(t2))
                else if (zero(k2, m1))
                  br(p1, m1, merge0(t2, k2, l1), mapV1(r1))
                else
                  br(p1, m1, mapV1(l1), merge0(t2, k2, r1))
              case Leaf(k1, _) =>
                if (k1 === k2)
                  combine(t1, t2)
                else
                  maybeLink(k1, mapV1(t1), k2, mapV2(t2))
              case Empty =>
                mapV2(t2)
            }
          merge0(t2, k2, t1)
        case (Branch(_, _, _, _), Empty) =>
          mapV1(t1)
        case (Leaf(k1, _), _) =>
          def merge0(t1: IntMap[V1], k1: Int, t2: IntMap[V2]): IntMap[V] =
            t2 match {
              case Branch(p2, m2, l2, r2) =>
                if (nomatch(k1, p2, m2))
                  maybeLink(k1, mapV1(t1), p2, mapV2(t2))
                else if (zero(k1, m2))
                  br(p2, m2, merge0(t1, k1, l2), mapV2(r2))
                else
                  br(p2, m2, mapV2(l2), merge0(t1, k1, r2))
              case Leaf(k2, _) =>
                if (k1 === k2)
                  combine(t1, t2)
                else
                  maybeLink(k1, mapV1(t1), k2, mapV2(t2))
              case Empty =>
                mapV1(t1)
            }
          merge0(t1, k1, t2)
        case (Empty, _) => mapV2(t2)
      }
    go(t1, t2)
  }

  implicit def IntMapMonoidValueMonoid[V: Semigroup]: Monoid[IntMap[V]] =
    new Monoid[IntMap[V]] {

      override def combine(x: IntMap[V], y: IntMap[V]): IntMap[V] =
        x.unionWith(y, _ combine _)

      override def empty: IntMap[V] =
        IntMap.empty

    }

  implicit object IntMapTraverse extends Traverse[IntMap] {

    override def traverse[G[_]: Applicative, A, B](t: IntMap[A])(f: A => G[B]): G[IntMap[B]] =
      t match {
        case Empty      => empty[B].pure[G]
        case Leaf(k, v) => f(v).map(Leaf(k, _))
        case Branch(p, m, l, r) =>
          if (m < 0)
            (traverse(r)(f), traverse(l)(f)).mapN((r, l) => Branch(p, m, l, r))
          else
            (traverse(l)(f), traverse(r)(f)).mapN(Branch(p, m, _, _))
      }

    override def map[A, B](fa: IntMap[A])(f: A => B): IntMap[B] =
      fa.mapValues(f)

    override def foldLeft[A, B](fa: IntMap[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: IntMap[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldRight(lb)(f)

  }

  implicit def IntMapEq[V: Eq]: Eq[IntMap[V]] = new Eq[IntMap[V]] {

    override def eqv(x: IntMap[V], y: IntMap[V]): Boolean =
      (x, y) match {
        case (Branch(p1, m1, l1, r1), Branch(p2, m2, l2, r2)) =>
          (p1 === p2) && (m1 === m2) && (l1 === l2) && (r1 === r2)
        case (Leaf(k1, v1), Leaf(k2, v2)) =>
          (k1 === k2) && (v1 === v2)
        case (Empty, Empty) =>
          true
        case (_, _) =>
          false
      }

    override def neqv(x: IntMap[V], y: IntMap[V]): Boolean =
      (x, y) match {
        case (Branch(p1, m1, l1, r1), Branch(p2, m2, l2, r2)) =>
          (p1 =!= p2) || (m1 =!= m2) || (l1 =!= l2) || (r1 =!= r2)
        case (Leaf(k1, v1), Leaf(k2, v2)) =>
          (k1 =!= k2) || (v1 =!= v2)
        case (Empty, Empty) =>
          false
        case (_, _) =>
          true
      }

  }

  implicit def IntMapShow[V: Show]: Show[IntMap[V]] =
    Show.show(_.mapValuesWithKey((k, v) => show"$k -> $v").mkString_("IntMap(", ", ", ")"))

}

private sealed trait Frame[V, V1]
private object Frame {
  case class Done[V, V1](p: Int, m: Int, l: IntMap[V1], r: IntMap[V1]) extends Frame[V, V1]
  case class LeftDone[V, V1](p: Int, m: Int, l: IntMap[V1], r: IntMap[V]) extends Frame[V, V1]
  case class New[V, V1](p: Int, m: Int, l: IntMap[V], r: IntMap[V]) extends Frame[V, V1]
}
