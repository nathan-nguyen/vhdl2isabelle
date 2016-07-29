package parsing

import org.slf4j.LoggerFactory

import scala.language.implicitConversions

case class VInfo(typeInfo: TypeInfo, defInfo: DefInfo)

object V2IUtils {

  val logger = LoggerFactory.getLogger(this.getClass)

  type RangeTy = (String, String, String)

  def defaultTargetName(msg: String): String = {
    logger.warn(s"defaultTargetName: ${msg}")
    "===???==="
  }

  def defaultRange(msg: String): RangeTy = {
    logger.warn(s"defaultRange: ${msg}")
    ("???", "???", "???")
  }

  def defaultScalarValue(msg: String): IVariable = {
    logger.warn(s"defaultScalarValue: ${msg}")
    IVariable("???", "???")
  }

  def VHDLize(vhdlType: String) = s"vhdl_${vhdlType}"

}
