package fs2.data.mft

case class Rules[InTag, OutTag](params: List[Int], tree: List[(EventSelector[InTag], Rhs[OutTag])])
