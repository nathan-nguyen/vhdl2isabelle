val sss = List("a", "a_b", "a_b_c")

val ttt = List("a")

ttt.tail.foldLeft(sss.head)((acc, cur)=> s"(sp_of_spl ${acc} (s.''${cur}''))")