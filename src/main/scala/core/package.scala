import org.slf4j.LoggerFactory

package object core {

  def VHDLize(vhdlType: VBaseType) = s"vhdl_${vhdlType.s}"

  val logger = LoggerFactory.getLogger(getClass)

  object VIError extends Throwable

  case class VIErrorMsg(msg: String) extends Exception(msg)

  type TDTy = scala.collection.mutable.Map[VCustomizedType, RecordInfoTy]
  type RecordInfoTy = Seq[(String, VSubtypeInd)]
  type IdTy = String
  type DefIdPair = (Option[IDef], IdTy)

  def defaultId = ""
  val unknownString = "???"

  def handler(msg: String) = throw VIErrorMsg(msg)

  object RangeD extends Enumeration {
    type Ty = Value
    val to = Value("TO")
    val downto = Value("DOWNTO")
    val unkown = Value("XXX")
  }

  case class VRangeTy(l: String, rangeD: RangeD.Ty, r: String)

  def defaultRange(msg: String): VRangeTy = {
    logger.warn(s"defaultRange: ${msg}")
    VRangeTy(unknownString, RangeD.unkown, unknownString)
  }

  // TODO define a list repr function

  implicit class IsarList[A](l: List[A]) {
    def ISAR: String = l.mkString("[", ",\n", "]")

    def ISAR_r: String = l.mkString("[", ",", "]")

    def ISAR_conc: String = l.mkString("[\n\t", ",\n\t", "\n]")
  }

}
