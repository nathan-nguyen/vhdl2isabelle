val l = List(1,2,3)
l.slice(1, 1)
l.slice(1,2)


val sss = List("a", "b", "c")

val eee = List[String]()

eee.scanLeft("0")((acc, cur)=>s"${acc}_${cur}")