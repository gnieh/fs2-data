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

private sealed trait State

private object State {
  case object BeforeValue extends State
  case object BeforeObjectKey extends State
  case object ExpectObjectKey extends State
  case object AfterObjectKey extends State
  case object BeforeObjectValue extends State
  case object AfterObjectValue extends State
  case object BeforeArrayValue extends State
  case object ExpectArrayValue extends State
  case object AfterArrayValue extends State
}
