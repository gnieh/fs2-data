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

case class Input[In](state: Int, depth: Int, evt: Option[In])

object Input {

  implicit def InputSelectable[In, InTag](implicit In: Selectable[In, Tag[InTag]]): Selectable[Input[In], Tag[InTag]] =
    new Selectable[Input[In], Tag[InTag]] {

      override def tree(e: Input[In]): ConstructorTree[Tag[InTag]] =
        ConstructorTree(
          Tag.Input,
          List(
            ConstructorTree.noArgConstructor(Tag.State(e.state)),
            ConstructorTree.noArgConstructor(Tag.Depth(e.depth)),
            e.evt.fold[ConstructorTree[Tag[InTag]]](ConstructorTree.noArgConstructor(Tag.End))(In.tree(_))
          )
        )

    }

}
