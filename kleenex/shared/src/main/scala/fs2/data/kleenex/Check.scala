/*
 * Copyright 2021 Lucas Satabin
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

package fs2.data.kleenex

import cats.data.NonEmptyList
import cats.parse.Caret
import cats.syntax.all._
import cats.data.StateT
import cats.MonadError
import fs2.data.kleenex.core.KleenexCompilerException
import scala.annotation.tailrec

class Checker[F[_]](implicit F: MonadError[F, Throwable]) {

  def check(prog: Program): F[Unit] = {
    val declMap = prog.productions.toList.map { case p @ Production(name, t) => (name, (p.pos, t)) }.toMap
    scc(declMap).flatMap { components =>
      components.traverse_ { component =>
        val allStrictDeps =
          component.flatMap(id => declMap.get(id).map { case (pos, t) => (id, pos, strictDependencies(t)) })
        val localStrictDeps = allStrictDeps.toList.mapFilter { case (id, pos, deps) =>
          // remove strict dependencies not in SCC
          val deps1 = deps.view.filterKeys(component.contains(_)).toMap
          if (deps1.nonEmpty)
            (id, pos).some
          else
            None
        }
        if (localStrictDeps.nonEmpty)
          F.raiseError[Unit](KleenexCompilerException(s"""Following productions contain non tail recursive calls:
                                                         |${localStrictDeps
            .map { case (id, pos) =>
              s"$id (at line ${pos.line + 1})"
            }
            .mkString("\n")}""".stripMargin))
        else
          F.unit

      }
    }
  }

  private def successors(id: String, term: Term): List[String] = {
    def go(t: Term, acc: Set[String]): Set[String] =
      t match {
        case Term.Var(s)          => acc + s
        case Term.Concat(ts)      => ts.foldLeft(acc)((acc, t) => go(t, acc))
        case Term.Alternative(ts) => ts.foldLeft(acc)((acc, t) => go(t, acc))
        case Term.Star(t)         => go(t, acc)
        case Term.Plus(t)         => go(t, acc)
        case Term.Question(t)     => go(t, acc)
        case Term.Range(t, _, _)  => go(t, acc)
        case Term.Suppress(t)     => go(t, acc)
        case Term.Capture(_, t)   => go(t, acc)
        case _                    => acc
      }

    go(term, Set.empty).toList
  }

  private def termIdents(t: Term): Map[String, Set[Caret]] =
    t match {
      case Term.Var(name)       => Map(name -> Set(t.pos))
      case Term.Concat(ts)      => ts.toList.map(termIdents(_)).combineAll
      case Term.Alternative(ts) => ts.toList.map(termIdents(_)).combineAll
      case Term.Star(t)         => termIdents(t)
      case Term.Plus(t)         => termIdents(t)
      case Term.Question(t)     => termIdents(t)
      case Term.Suppress(t)     => termIdents(t)
      case Term.Capture(_, t)   => termIdents(t)
      case _                    => Map.empty
    }

  // strict dependencies are the variables occurring not in tail positions in sequences
  def strictDependencies(t: Term): Map[String, Set[Caret]] =
    t match {
      case Term.Concat(NonEmptyList(t1, t2 :: ts)) =>
        strictDependencies(Term.Concat(NonEmptyList(t2, ts))).combine(termIdents(t1))
      case Term.Concat(NonEmptyList(t, Nil)) => strictDependencies(t)
      case Term.Alternative(ts)              => ts.toList.map(strictDependencies(_)).combineAll
      case Term.Star(t)                      => strictDependencies(t)
      case Term.Plus(t)                      => strictDependencies(t)
      case Term.Question(t)                  => strictDependencies(t)
      case Term.Suppress(t)                  => strictDependencies(t)
      case Term.Capture(_, t)                => strictDependencies(t)
      case _                                 => Map.empty
    }

  private type State[Res] = StateT[F, SCCState, Res]

  private def gets[Res](f: SCCState => Res): State[Res] =
    StateT.inspect(f)

  private def getProps(id: String): State[Option[SCCProps]] =
    StateT.inspect(_.props.get(id))

  private def nop: State[Unit] =
    StateT.empty

  private def modify(f: SCCState => SCCState): State[Unit] =
    StateT.modify(f)

  private def update[Res](f: SCCState => (SCCState, Res)): State[Res] =
    StateT.inspect(f).flatMap { case (st, res) => StateT.set(st).as(res) }

  private def raiseError[Res](t: Throwable): State[Res] =
    nop.flatMapF(_ => t.raiseError)

  private def scc(declMap: Map[String, (Caret, Term)]): F[List[Set[String]]] = {
    val state = SCCState(0, Nil, Map.empty, Nil)

    def process(v: String, t: Term): State[Unit] =
      for {
        // first push v on the stack and assign an index
        vProps <- update { st =>
          val props = SCCProps(true, st.index, st.index)
          (st.copy(index = st.index + 1, stack = v :: st.stack, props = st.props.updated(v, props)), props)
        }
        // then for each successor compute recursively
        () <- successors(v, t).traverse_ { w =>
          getProps(w).flatMap {
            case Some(wProps) =>
              // successor already processed
              if (wProps.onStack)
                // it is on stack, hence in the current SCC
                modify(st =>
                  st.copy(props = st.props.updated(v, vProps.copy(lowlink = vProps.lowlink.min(wProps.index)))))
              else
                // not on the stack, not in SCC
                nop
            case None =>
              // not processed yet, do it
              declMap.get(w) match {
                case Some((_, wt)) =>
                  for {
                    () <- process(w, wt)
                    wProps <- gets(_.props(w))
                    vProps <- gets(_.props(v))
                    () <- modify(st =>
                      st.copy(props = st.props.updated(v, vProps.copy(lowlink = vProps.lowlink.min(wProps.lowlink)))))
                  } yield ()
                case None =>
                  raiseError[Unit](
                    KleenexCompilerException(s"Unknown identifier $w in definition of $v at line ${t.pos.line + 1}"))
              }
          }
        }
        vProps <- gets(_.props(v))
        () <-
          if (vProps.lowlink == vProps.index)
            for {
              stack <- gets(_.stack)
              (component, stack1) = spanUntilIncluding(stack, v)
              () <- modify { st =>
                st.copy(
                  // pop from stack
                  stack = stack1,
                  // update the components
                  components = component.toSet :: st.components,
                  // remove vertices in component from stack
                  props = component.foldLeft(st.props) { (props, w) =>
                    props.updatedWith(w)(_.map(_.copy(onStack = false)))
                  }
                )
              }
            } yield ()
          else
            nop
      } yield ()

    declMap.toList
      // traverse each node (aka production identifier)
      .traverse_ { case (id, (_, t)) =>
        getProps(id)
          .flatMap {
            case None =>
              // if no index has been assigned yet, process it
              process(id, t)
            case Some(_) =>
              // otherwise, just continue
              nop
          }
      }
      .runS(state)
      .map(_.components)
  }

  private def spanUntilIncluding(l: List[String], v: String): (List[String], List[String]) = {
    @tailrec
    def loop(l: List[String], acc: List[String]): (List[String], List[String]) =
      l match {
        case Nil         => (l, Nil)
        case `v` :: rest => ((v :: acc).reverse, rest)
        case e :: rest   => loop(rest, e :: acc)
      }
    loop(l, Nil)
  }

}

case class SCCProps(onStack: Boolean, index: Int, lowlink: Int)
case class SCCState(index: Int, stack: List[String], props: Map[String, SCCProps], components: List[Set[String]])
