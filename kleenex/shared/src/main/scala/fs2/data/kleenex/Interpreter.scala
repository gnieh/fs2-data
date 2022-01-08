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

package fs2
package data
package kleenex

import cats.MonadError
import cats.syntax.all._

case class KleenexException(msg: String) extends Exception(msg)

object Interpreter {

  def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Either[String, Action], String] = {
    (s: Stream[F, Either[String, Action]]) =>
      s
        .evalScan(new Environment("" :: Nil, Map.empty)) {
          case (env, Left(c)) =>
            env
              .append(c)
              .liftTo[F](KleenexException(s"cannot append on top of stack"))
          case (env, Right(act)) =>
            act match {
              case Action.Push => (env.push).pure[F]
              case Action.Pop(reg) =>
                env
                  .pop(reg)
                  .liftTo[F](KleenexException(s"cannot pop to register $reg"))
              case Action.Write(reg) =>
                env
                  .write(reg)
                  .liftTo[F](KleenexException(s"cannot write register $reg"))
            }
        }
        .last
        .evalMap {
          case Some(Environment(s :: _, _)) => s.pure[F]
          case _                            => F.raiseError[String](KleenexException("cannot pop from empty stack"))
        }
  }

}
