
package object parsing {

  type IdTy = String
  type DefIdPair = (Option[IDef], IdTy)

  // TODO define a list repr function

  implicit class IsarList[A](l: List[A]) {
    def ISAR: String = l.mkString(",")
  }

}
