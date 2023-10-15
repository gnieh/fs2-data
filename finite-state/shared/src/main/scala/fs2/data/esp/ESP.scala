/*
 * Copyright 2023 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fs2
package data
package esp

import cats.syntax.all._
import cats.{Order, Show}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import pattern._

/** An Event Stream Processor is a generalization of the one defined in _Streamlining Functional XML Processing_.
  * It encodes its recognized patterns into a [[fs2.data.matching.DecisionTree Decision Tree]], including the state and depth. This flexibility allows for easily implementing
  * catch all rules, no matter what the state or depth is.
  */
private[data] class ESP[F[_], Guard, InTag, OutTag](val init: Int,
                                                    val params: Map[Int, Int],
                                                    val rules: DecisionTree[Guard, Tag[InTag], Rhs[OutTag]])(implicit
    F: RaiseThrowable[F]) {

  def call[In, Out](env: Vector[Expr[Out]], q: Int, d: Int, args: List[Expr[Out]], in: Option[In], inline: Boolean)(
      implicit
      In: Selectable[In, Tag[InTag]],
      Out: Conversion[OutTag, Out],
      TT: Tag2Tag[InTag, OutTag],
      G: Evaluator[Guard, Tag[InTag]]) =
    params.get(q).liftTo[Pull[F, Nothing, *]](new ESPException(s"unknown state $q")).flatMap { params =>
      if (params === args.size) {
        args
          .traverse { arg =>
            if (inline)
              Pull.pure(arg)
            else
              step(env, arg, in)
          }
          .flatMap { env =>
            rules
              .get(Input(q, d, in))
              .liftTo[Pull[F, Nothing, *]](new ESPException(s"no rule found for state $q at depth $d for input $in"))
              .flatMap(eval(env.toVector, d, in, _))
          }
      } else {
        Pull.raiseError(new ESPException(
          s"wrong number of arguments given in state $q reading input $in (expected $params but got ${args.size})"))
      }
    }

  private def select[In](in: In, selector: Selector[Guard, Tag[InTag]])(implicit
      In: Selectable[In, Tag[InTag]],
      G: Evaluator[Guard, Tag[InTag]]) =
    In.select(in, selector).collect {
      case Tag.Name(tag)  => tag
      case Tag.Value(tag) => tag
    }

  def step[In, Out](env: Vector[Expr[Out]], e: Expr[Out], in: Option[In])(implicit
      In: Selectable[In, Tag[InTag]],
      Out: Conversion[OutTag, Out],
      TT: Tag2Tag[InTag, OutTag],
      G: Evaluator[Guard, Tag[InTag]]): Pull[F, Nothing, Expr[Out]] =
    e match {
      case Expr.Call(q, d, args, next) =>
        (call(env, q, d, args, in, false), step(env, next, in)).mapN(Expr.concat(_, _))
      case Expr.Epsilon =>
        Pull.pure(Expr.Epsilon)
      case Expr.Open(o, next) =>
        step(env, next, in).map(Expr.Open(o, _))
      case Expr.Close(c, next) =>
        step(env, next, in).map(Expr.Close(c, _))
      case Expr.Leaf(v, next) =>
        step(env, next, in).map(Expr.Leaf(v, _))
      case Expr.Default(v, next) =>
        step(env, next, in).map {
          case Expr.Epsilon              => Expr.Leaf(v, Expr.Epsilon)
          case e @ Expr.Close(_, _)      => Expr.Leaf(v, e)
          case q @ Expr.Call(_, _, _, _) => Expr.Default(v, q)
          case e                         => e
        }
    }

  def eval[In, Out](env: Vector[Expr[Out]], depth: Int, in: Option[In], rhs: Rhs[OutTag])(implicit
      In: Selectable[In, Tag[InTag]],
      Out: Conversion[OutTag, Out],
      TT: Tag2Tag[InTag, OutTag],
      G: Evaluator[Guard, Tag[InTag]]): Pull[F, Nothing, Expr[Out]] =
    rhs match {
      case Rhs.Call(q, d, params) =>
        params
          .traverse(eval(env, depth, in, _))
          .map(Expr.Call(q, d(depth), _, Expr.Epsilon))
      case Rhs.SelfCall(q, params) =>
        params
          .traverse(eval(env, depth, in, _))
          .flatMap(call(env, q, 0, _, in, true))
      case Rhs.Param(i) =>
        env
          .lift(i)
          .liftTo[Pull[F, Nothing, *]](new ESPException(s"unknown parameter $i"))
      case Rhs.Epsilon =>
        Pull.pure(Expr.Epsilon)
      case Rhs.Default(v) =>
        Pull.pure(Expr.Default(Out.makeLeaf(v), Expr.Epsilon))
      case Rhs.Tree(tag, inner) =>
        eval(env, depth, in, inner)
          .map(inner => Expr.Open(Out.makeOpen(tag), Expr.concat(inner, Expr.Close(Out.makeClose(tag), Expr.Epsilon))))
      case Rhs.CapturedTree(inner) =>
        eval(env, depth, in, inner).flatMap { inner =>
          in.flatMap(select(_, Selector.Cons(Selector.Root(), Tag.Open, 0)))
            .liftTo[Pull[F, Nothing, *]](new ESPException("cannot capture eos"))
            .map { v =>
              val tag = TT.convert(v)
              Expr.Open(Out.makeOpen(tag), Expr.concat(inner, Expr.Close(Out.makeClose(tag), Expr.Epsilon)))
            }
        }
      case Rhs.Leaf(v) =>
        Pull.pure(Expr.Leaf(Out.makeLeaf(v), Expr.Epsilon))
      case Rhs.CapturedLeaf =>
        in.flatMap(select(_, Selector.Cons(Selector.Root(), Tag.Leaf, 0)))
          .liftTo[Pull[F, Nothing, *]](new ESPException("cannot capture eos"))
          .map(v => Expr.Leaf(Out.makeLeaf(TT.convert(v)), Expr.Epsilon))
      case Rhs.ApplyToLeaf(f: (OutTag => Either[String, OutTag]) @unchecked) =>
        in.flatMap(select(_, Selector.Cons(Selector.Root(), Tag.Leaf, 0)))
          .liftTo[Pull[F, Nothing, *]](new ESPException("cannot capture eos"))
          .flatMap(v =>
            f(TT.convert(v))
              .leftMap(new ESPException(_))
              .liftTo[Pull[F, Nothing, *]]
              .map(t => Expr.Leaf(Out.makeLeaf(t), Expr.Epsilon)))
      case Rhs.Concat(rhs1, rhs2) =>
        (eval(env, depth, in, rhs1), eval(env, depth, in, rhs2)).mapN(Expr.concat(_, _))
    }

  private def squeeze[Out](e: Expr[Out]): (Expr[Out], List[Out]) =
    e match {
      case Expr.Call(_, _, _, _)                                    => (e, Nil)
      case Expr.Epsilon                                             => (Expr.Epsilon, Nil)
      case Expr.Default(v, Expr.Epsilon)                            => (Expr.Leaf(v, Expr.Epsilon), Nil)
      case Expr.Default(v, e @ Expr.Close(_, _))                    => squeeze(Expr.Leaf(v, e))
      case Expr.Default(_, e @ (Expr.Open(_, _) | Expr.Leaf(_, _))) => squeeze(e)
      case Expr.Default(_, _)                                       => (e, Nil)
      case Expr.Open(o, e) =>
        val (e1, s1) = squeeze(e)
        (e1, o :: s1)
      case Expr.Close(c, e) =>
        val (e1, s1) = squeeze(e)
        (e1, c :: s1)
      case Expr.Leaf(v, e) =>
        val (e1, s1) = squeeze(e)
        (e1, v :: s1)
    }

  def squeezeAll[Out](e: Expr[Out]): (Expr[Out], List[Out]) = {
    @tailrec
    def loop(e: Expr[Out], acc: ListBuffer[Out]): (Expr[Out], List[Out]) =
      e match {
        case Expr.Epsilon | Expr.Call(_, _, _, _) => (e, acc.result())
        case _ =>
          val (e1, s) = squeeze(e)
          loop(e1, acc ++= s)
      }
    loop(e, new ListBuffer)
  }

  def transform[In, Out](chunk: Chunk[In], idx: Int, rest: Stream[F, In], e: Expr[Out], chunkAcc: ListBuffer[Out])(
      implicit
      In: Selectable[In, Tag[InTag]],
      Out: Conversion[OutTag, Out],
      TT: Tag2Tag[InTag, OutTag],
      G: Evaluator[Guard, Tag[InTag]]): Pull[F, Out, Unit] =
    if (idx >= chunk.size) {
      Pull.output(Chunk.from(chunkAcc.result())) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          chunkAcc.clear()
          transform(hd, 0, tl, e, chunkAcc)
        case None =>
          step(Vector.empty, e, none).map(squeezeAll(_)).flatMap { case (e, s) =>
            e match {
              case Expr.Epsilon =>
                Pull.output(Chunk.from(s))
              case _ => Pull.raiseError(new ESPException(s"unexpected end of input $e"))
            }
          }
      }
    } else {
      step(Vector.empty, e, chunk(idx).some).map(squeeze(_)).flatMap { case (e, s) =>
        transform(chunk, idx + 1, rest, e, chunkAcc ++= s)
      }
    }

  def pipe[In, Out](implicit
      In: Selectable[In, Tag[InTag]],
      Out: Conversion[OutTag, Out],
      TT: Tag2Tag[InTag, OutTag],
      G: Evaluator[Guard, Tag[InTag]]): Pipe[F, In, Out] =
    transform[In, Out](Chunk.empty, 0, _, Expr.Call(init, 0, Nil, Expr.Epsilon), new ListBuffer).stream

}

object ESP {

  private case class Case[I](q: Option[Int], depth: Option[Int], pat: Option[Tag[I]], rhs: String)

  private object Case {
    // None is bigger than anything else (wildcard case)
    implicit def optionOrder[T: Order]: Order[Option[T]] = new Order[Option[T]] {
      def compare(x: Option[T], y: Option[T]): Int =
        x match {
          case None =>
            if (y.isEmpty) 0 else 1
          case Some(a) =>
            y match {
              case None    => -1
              case Some(b) => a.compare(b)
            }
        }
    }

    implicit def ord[I: Order]: Order[Case[I]] = Order.by { case Case(q, d, p, _) =>
      (q, d, p)
    }
  }

  implicit def show[F[_], G, I: Show: Order, O: Show]: Show[ESP[F, G, I, O]] = { esp =>
    type TreeStack = (Option[Int], Option[Int], Option[Tag[I]], DecisionTree[G, Tag[I], Rhs[O]])
    def makeCases(trees: List[TreeStack], acc: List[Case[I]]): List[Case[I]] =
      trees match {
        case Nil                                          => acc.sortWith(_ < _)
        case (q, d, pat, DecisionTree.Fail()) :: trees    => makeCases(trees, Case(q, d, pat, "FAIL") :: acc)
        case (q, d, pat, DecisionTree.Leaf(out)) :: trees => makeCases(trees, Case(q, d, pat, out.show) :: acc)
        case (q, d, pat, DecisionTree.Switch(_, branches, default)) :: trees =>
          val cases = branches.toList.map {
            case (Tag.State(q), t) => (Some(q), d, pat, t)
            case (Tag.Depth(d), t) => (q, Some(d), pat, t)
            case (pat, t)          => (q, d, Some(pat), t)
          }
          makeCases(cases ++ default.fold(List.empty[TreeStack])((q, d, None, _) :: Nil) ++ trees, acc)
      }

    Stream
      .emits(makeCases((None, None, None, esp.rules) :: Nil, Nil))
      .groupAdjacentBy(_.q)
      .map { case (_, qcases) =>
        qcases
          .map { case Case(q, d, p, r) =>
            val params = List.tabulate(esp.params.getOrElse(q.getOrElse(-1), 0))(i => show"y$i").mkString_(", ")
            show"${q.fold("_")(q => show"q$q")}[${d.fold(raw"$d")(_.show)}]($params) --[ ${p.fold("_")(_.show)} ]--> $r"
          }
          .mkString_("\n")
      }
      .intersperse("\n\n")
      .compile
      .string

  }

}
