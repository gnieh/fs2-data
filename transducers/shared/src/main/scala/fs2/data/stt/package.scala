package fs2.data

package object stt {

  implicit class MappableOps[M[_, _], From, To](val m: M[From, To]) extends AnyVal {
    def get(from: From)(implicit M: Table[M]): Option[To] =
      M.get(m)(from)
  }

}
