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

package fs2.data.kleenex.core

import fs2.data.kleenex.{Action, Production, Program => KProgram, Regex, RegOrStr, Term => KTerm}
import fs2.data.transducer.CharRanges

import cats.MonadError
import cats.data.StateT
import cats.syntax.all._
import cats.data.NonEmptyList

case class KleenexCompilerException(msg: String) extends Exception(msg)

case class CompilerState(idents: Map[(String, Boolean), Int],
                         decls: Map[Int, Term],
                         revDecls: Map[Term, Int],
                         fresh: Int)

class Compiler[F[_]](implicit F: MonadError[F, Throwable]) {

  private type State[Res] = StateT[F, CompilerState, Res]

  /** Compiles a kleenex program into the core language representation. */
  def compile(prog: KProgram): F[Program] = {
    // associate each production to 2 ids:
    //  - one when it outputs element
    //  - one when it outputs *no* elements
    val idents = prog.productions
      .flatMap { case Production(name, term) =>
        NonEmptyList.of((name, true), (name, false))
      }
      .zipWithIndex
      .toList
      .toMap
    val fresh = idents.size
    val state = CompilerState(idents, Map.empty, Map.empty, fresh)

    val checkPipeline =
      prog.pipeline.traverse(name =>
        idents.get(name -> true) match {
          case Some(id) => id.pure[F]
          case None     => new KleenexCompilerException(s"Unknown production $name in pipeline").raiseError[F, Int]
        })

    val compiledProductions =
      prog.productions
        .traverse_ { case Production(name, term) =>
          for {
            idout <- lookup(name, true)
            idnoout <- lookup(name, false)
            compiledout <- compile(true, term)
            compilednoout <- compile(false, term)
            _ <- insertDecl(idout, Term.Seq(List(compiledout)))
            _ <- insertDecl(idnoout, Term.Seq(List(compilednoout)))
          } yield ()
        }

    (checkPipeline, compiledProductions.runS(state))
      .mapN { (pipeline, state) =>
        val reached = reachable(pipeline.toList, state.decls)
        compress(Program(pipeline, state.decls.view.filterKeys(reached.contains(_)).toMap))
      }
  }

  def compile(re: Regex): F[Program] = {
    compile(true, re)
      .run(CompilerState(Map.empty, Map.empty, Map.empty, 0))
      .map { case (st, id) => Program(NonEmptyList.one(id), st.decls) }
  }

  private def compile(output: Boolean, re: Regex): State[Int] =
    re match {
      case Regex.Any =>
        declare(Term.Read(CharRanges.all, output))
      case Regex.Str(str) =>
        str.toList
          .traverse(c => declare(Term.Read(CharRanges.char(c), output)))
          .flatMap(ids => declare(Term.Seq(ids)))
      case Regex.Concat(res) =>
        res.traverse(compile(output, _)).flatMap(ids => declare(Term.Seq(ids.toList)))
      case Regex.Or(alts) =>
        alts.traverse(compile(output, _)).flatMap(ids => declare(Term.Alternative(ids)))
      case Regex.Plus(re, greedy) =>
        compile(output, re).flatMap(plus(_, greedy))
      case Regex.Star(re, greedy) =>
        compile(output, re).flatMap(star(_, greedy))
      case Regex.Question(re, greedy) =>
        compile(output, re).flatMap(question(_, greedy))
      case Regex.Set(chars) =>
        declare(Term.Read(chars, output))
      case Regex.Range(re, min, max) =>
        compile(output, re).flatMap(range(_, min, max))
    }

  private def compile(output: Boolean, term: KTerm): State[Int] =
    term match {
      case KTerm.One() =>
        declare(Term.epsilon)
      case KTerm.Str(s) =>
        val toOuptut = if (output) s else ""
        declare(Term.Const(Left(toOuptut)))
      case KTerm.Var(v) =>
        lookup(v, output)
      case KTerm.Capture(reg, t) =>
        for {
          idt <- compile(output, t)
          idpush <- declare(Term.Const(Right(Action.Push)))
          idpop <- declare(Term.Const(Right(Action.Pop(reg))))
          id <- declare(Term.Seq(List(idpush, idt, idpop)))
        } yield id
      case KTerm.Output(reg) =>
        declare(Term.Const(Right(Action.Write(reg))))
      case KTerm.UpdateReg(reg, value) =>
        for {
          idpush <- declare(Term.Const(Right(Action.Push)))
          idsval <- value.map(updateSym(_)).traverse(c => declare(Term.Const(c)))
          idpop <- declare(Term.Const(Right(Action.Pop(reg))))
          id <- declare(Term.Seq((idpush :: idsval).toList :+ idpop))
        } yield id
      case KTerm.Alternative(cases) =>
        flattenAlternatives(cases)
          .traverse(compile(output, _))
          .flatMap(ids => declare(Term.Alternative(ids)))
      case KTerm.Concat(ts) =>
        flattenSequences(ts)
          .traverse(compile(output, _))
          .flatMap(ids => declare(Term.Seq(ids)))
      case KTerm.RE(re) =>
        compile(output, re)
      case KTerm.Suppress(t) =>
        compile(false, t)
      case KTerm.Star(t) =>
        compile(output, t).flatMap(star(_, true))
      case KTerm.Plus(t) =>
        compile(output, t).flatMap(plus(_, true))
      case KTerm.Question(t) =>
        compile(output, t).flatMap(question(_, true))
      case KTerm.Range(t, min, max) =>
        compile(output, t).flatMap(range(_, min, max))
    }

  // r* = r1 | 1
  // r1 = r r*
  // r*? = 1 | r2
  // r2 = r r*?
  private def star(idt: Int, greedy: Boolean): State[Int] =
    for {
      ideps <- declare(Term.epsilon)
      id <- freshId
      idloop <- declare(Term.Seq(List(idt, id)))
      id <- insertDecl(id,
                       Term.Alternative(
                         // favor more over less
                         if (greedy) NonEmptyList.of(idloop, ideps)
                         // favor less over more
                         else NonEmptyList.of(ideps, idloop)
                       ))
    } yield id

  // r+ = r r*
  // r+? = r r*?
  private def plus(idt: Int, greedy: Boolean): State[Int] =
    for {
      idstar <- star(idt, greedy)
      id <- declare(Term.Seq(List(idt, idstar)))
    } yield id

  // r? = r | 1
  // r?? = 1 | r
  private def question(idt: Int, greedy: Boolean): State[Int] =
    for {
      ideps <- declare(Term.epsilon)
      id <- declare(
        Term.Alternative(
          // favor one over zero
          if (greedy) NonEmptyList.of(idt, ideps)
          // favor zero over one
          else NonEmptyList.of(ideps, idt)))
    } yield id

  private def range(idt: Int, min: Int, max: Option[Int]): State[Int] =
    max match {
      case Some(max) if min == max =>
        declare(Term.Seq(List.fill(min)(idt)))
      case Some(max) =>
        question(idt, true).flatMap(idq => declare(Term.Seq(List.fill(min)(idt) ++ List.fill(max - min)(idq))))
      case None =>
        star(idt, true).flatMap(idstar => declare(Term.Seq(List.fill(min)(idt) ++ List(idstar))))
    }

  private def updateSym(sym: RegOrStr): Either[String, Action] =
    sym match {
      case RegOrStr.Reg(reg) => Right(Action.Write(reg))
      case RegOrStr.Str(s)   => Left(s)
    }

  private def flattenAlternatives(alts: NonEmptyList[KTerm]): NonEmptyList[KTerm] =
    alts match {
      case NonEmptyList(KTerm.Alternative(alts), a :: rest) =>
        flattenAlternatives(alts).concatNel(flattenAlternatives(NonEmptyList(a, rest)))
      case NonEmptyList(KTerm.Alternative(alts), Nil) =>
        flattenAlternatives(alts)
      case NonEmptyList(t, a :: rest) =>
        t :: flattenAlternatives(NonEmptyList(a, rest))
      case NonEmptyList(t, Nil) =>
        NonEmptyList.one(t)
    }

  private def flattenSequences(ts: NonEmptyList[KTerm]): List[KTerm] =
    ts match {
      case NonEmptyList(KTerm.Concat(ts), t :: rest) => flattenSequences(ts) ++ flattenSequences(NonEmptyList(t, rest))
      case NonEmptyList(t, h :: rest)                => t :: flattenSequences(NonEmptyList(h, rest))
      case NonEmptyList(KTerm.Concat(ts), Nil)       => flattenSequences(ts)
      case NonEmptyList(t, Nil)                      => List(t)
    }

  private def get: State[CompilerState] =
    StateT.get

  private def modify(f: CompilerState => CompilerState): State[Unit] =
    StateT.modify(f)

  private def freshId: State[Int] =
    get.map(_.fresh) <* modify(s => s.copy(fresh = s.fresh + 1))

  private def insertDecl(id: Int, term: Term): State[Int] =
    modify(st => st.copy(decls = st.decls.updated(id, term), revDecls = st.revDecls.updated(term, id))).as(id)

  private def lookup(id: String, output: Boolean): State[Int] =
    get.map(_.idents.get((id, output))).flatMapF {
      case Some(id) => id.pure[F]
      case None     => KleenexCompilerException(s"Unknown non terminal identifier $id").raiseError[F, Int]
    }

  private def declare(term: Term): State[Int] =
    get.map(_.revDecls.get(term)).flatMap {
      case Some(id) => id.pure[State]
      case None     => freshId.flatMap(insertDecl(_, term))
    }

  private def reachable(from: List[Int], decls: Map[Int, Term]): Set[Int] = {
    def referenced(t: Term): List[Int] =
      t match {
        case Term.Seq(ids)         => ids
        case Term.Alternative(ids) => ids.toList
        case _                     => Nil
      }

    def loop(from: List[Int], acc: Set[Int]): Set[Int] =
      from match {
        case id :: from =>
          if (acc.contains(id))
            loop(from, acc)
          else
            loop(decls.get(id).map(referenced(_)).getOrElse(Nil) reverse_::: from, acc + id)
        case Nil =>
          acc
      }
    loop(from, Set.empty)
  }

  private def compress(prog: Program): Program = {
    def alias(aliases: Map[Int, Int], id: Int): Map[Int, Int] =
      prog.decls.get(id) match {
        case Some(Term.Seq(List(idt))) =>
          val aliases1 = alias(aliases, idt)
          aliases1.updated(id, aliases1.getOrElse(idt, idt))
        case _ =>
          aliases
      }
    val aliases = prog.decls.keys.foldLeft(Map.empty[Int, Int])(alias(_, _))

    def replace(t: Term): Term =
      t match {
        case Term.Alternative(ts) => Term.Alternative(ts.map(id => aliases.getOrElse(id, id)))
        case Term.Seq(ts)         => Term.Seq(ts.map(id => aliases.getOrElse(id, id)))
        case _                    => t
      }

    if (aliases.isEmpty)
      prog
    else
      Program(prog.pipeline.map(id => aliases.getOrElse(id, id)),
              prog.decls.view.filterKeys(!aliases.contains(_)).mapValues(replace(_)).toMap)
  }

}
