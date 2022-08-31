/*
 * Copyright 2019-2022 Lucas Satabin
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
