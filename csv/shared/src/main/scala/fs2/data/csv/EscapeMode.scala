package fs2.data.csv

/**
  * Controls the escaping when outputting CSV.
  */
sealed trait EscapeMode
object EscapeMode {

  /**
    * Escape only when necessary.
    */
  object Auto extends EscapeMode

  /**
    * Escape always, even if not needed.
    */
  object Always extends EscapeMode

  /**
    * Never escape, even if that would potentially lead to invalid CSV output.
    */
  object Never extends EscapeMode
}
