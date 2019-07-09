/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.json

private sealed trait NumberState {
  val isFinal: Boolean
}
private object NumberState {
  case object NumberStart extends NumberState {
    val isFinal: Boolean = false
  }
  case object IntegerStart extends NumberState {
    val isFinal: Boolean = false
  }
  case object IntegerBody extends NumberState {
    val isFinal: Boolean = true
  }
  case object FractionStart extends NumberState {
    val isFinal: Boolean = true
  }
  case object FractionOne extends NumberState {
    val isFinal: Boolean = false
  }
  case object FractionBody extends NumberState {
    val isFinal: Boolean = true
  }
  case object ExponentSign extends NumberState {
    val isFinal: Boolean = false
  }
  case object ExponentOne extends NumberState {
    val isFinal: Boolean = false
  }
  case object ExponentBody extends NumberState {
    val isFinal: Boolean = true
  }
  case object Invalid extends NumberState {
    val isFinal: Boolean = false
  }
}
