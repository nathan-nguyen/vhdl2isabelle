val listOpt = Some(List(1, 2, 3))
val ss = List("a", "b")


def foo: Map[Int, String] = {
  val res: Seq[(Int, String)] = for {
    str <- ss
    i <- listOpt.getOrElse(List.empty)
  } yield {
    i -> str
  }
  res.toMap
}