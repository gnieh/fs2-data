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

package fs2.data.text.render

sealed trait DocEvent

object DocEvent {

  /** Renders the text _as is_. */
  case class Text(text: String) extends DocEvent

  /** Adds a new line and indent, or a space if undone by a group */
  case object Line extends DocEvent

  /** Adds a new line and indent, or a empty if undone by a group */
  case object LineBreak extends DocEvent

  /** Begins a new group */
  case object GroupBegin extends DocEvent

  /** Ends a group */
  case object GroupEnd extends DocEvent

  /** Begins a new indent, incrementing the current indentation level */
  case object IndentBegin extends DocEvent

  /** Ends an indent , decrementing the current indentation level */
  case object IndentEnd extends DocEvent

  /** Begins a new alignment, setting the current indentation level to the current column */
  case object AlignBegin extends DocEvent

  /** Ends an alignment, setting the current indentation level back to what it was before this alignment */
  case object AlignEnd extends DocEvent
}
