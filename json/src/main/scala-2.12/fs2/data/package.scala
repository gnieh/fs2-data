/*
 * Copyright 2024 fs2-data Project
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

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer

package object data {

  implicit class VectorBuilderOps[T](val builder: VectorBuilder[T]) extends AnyVal {
    def addOne(t: T) = builder += t
  }

  implicit class ListBufferOps[T](val buffer: ListBuffer[T]) extends AnyVal {
    def addOne(t: T) = buffer += t
  }

}
