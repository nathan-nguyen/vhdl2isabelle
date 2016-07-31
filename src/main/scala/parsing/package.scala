
package object parsing {

  type IdTy = String

  // TODO define a list repr function

  implicit class IsarList[A](l: List[A]) {
    def ISAR: String = l.mkString(",")
  }

}
