
package object parsing {

  object VIError extends Throwable

  case class VIErrorMsg(msg:String) extends Exception(msg)

  type IdTy = String
  type DefIdPair = (Option[IDef], IdTy)
  val unknownString = "???"

  // TODO define a list repr function

  implicit class IsarList[A](l: List[A]) {
    def ISAR: String = l.mkString("[", ",", "]")
  }

}
