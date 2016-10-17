import org.slf4j.LoggerFactory

/**
  * Created by Hongxu Chen.
  */
package object core {

  val logger = LoggerFactory.getLogger(getClass)

  def VHDLize(vhdlType: VBaseType) = s"vhdl_${vhdlType.s}"

  object VIError extends Throwable

  case class VIErrorMsg(msg: String) extends Exception(msg)

  type TDTy = scala.collection.mutable.Map[VCustomizedType, RecordInfoTy]
  type RecordInfoTy = Seq[(String, VSubtypeInd)]
  type IdTy = String
  type DefIdPair = (Option[IDef], IdTy)

  val defaultId = ""

  val unknownString = "???"

  val vectorFlag = "_vector"
  //  val defaultCharType = "std_xxxxxx"
  val defaultCharType = "std_logic"

  def handler(msg: String) = throw VIErrorMsg(msg)

  object RangeD extends Enumeration {
    type Ty = Value
    val to = Value("TO")
    val downto = Value("DOWNTO")
    val unkown = Value("XXX")
  }

  case class VRangeV(l: String, rangeD: RangeD.Ty, r: String)

  def defaultRangeV(msg: String): VRangeV = {
    logger.warn(s"defaultRange: ${msg}")
    VRangeV(unknownString, RangeD.unkown, unknownString)
  }

  def handleExpKindMismatch(e1: IsabelleExpression, e2: IsabelleExpression, msg: String) = {
    logger.error(s"""expKindMismatch: ${msg}\n${e1.expKind}\t"${e1}"\n${e2.expKind}\t"${e2}"""")
  }

  // TODO define a list repr function

  implicit class IsabelleList[A](l: List[A]) {
    def ISABELLE: String = l.mkString("[", ",\n", "]")

    def ISABELLE_r: String = l.mkString("[", ",", "]")

    def ISABELLE_conc: String = l.mkString("[\n\t", ",\n\t", "\n]")
  }

}
