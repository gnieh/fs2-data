package fs2.data

import cats.data.NonEmptyList

case class MiniXPath(steps: NonEmptyList[Step])

sealed trait Step
object Step {
  case class Child(name: String) extends Step
  case class Descendant(name: String) extends Step
}
