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

package fs2.data.json.internals

private[json] object State {
  final val BeforeValue = 0
  final val BeforeObjectKey = 1
  final val ExpectObjectKey = 2
  final val AfterObjectKey = 3
  final val BeforeObjectValue = 4
  final val AfterObjectValue = 5
  final val BeforeArrayValue = 6
  final val ExpectArrayValue = 7
  final val AfterArrayValue = 8
}
