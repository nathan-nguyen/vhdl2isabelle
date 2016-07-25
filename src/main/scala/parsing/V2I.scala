package parsing

import org.slf4j.LoggerFactory

object V2I {

  val logger = LoggerFactory.getLogger(this.getClass)

  def VHDLize(vhdlType: String) = s"vhdl_${vhdlType}"

  abstract class IType

  case class IScalarType(id: String)

  case class IListType(id: String)

  val knownListType = Set("div32_in_type", "div32_out_type")

  def isListType(valType: String) = knownListType.contains(valType)

  def decoratedType(rawIdType: String, valType: String) = {
    if (knownListType.contains(valType)) s"${rawIdType} list"
    else rawIdType
  }

  def guessInitValImpl(valType: String): String = valType match {
    case "integer" => "(val_i 0)"
    case "real" => "(val_r 0.0)"
    case "character" => "(val_c (CHR ''0''))"
    case "boolean" => "(val_b True)"
    case "std_ulogic" => "(val_c (CHR ''0''))"
    case "std_logic" => "(val_c (CHR ''0''))"
    case _ => s"[[TODO ${valType}]]"
  }

  def getInitVal(valType: String, expOption: Option[VExp]): String = expOption match {
    case Some(exp) => {
      val expRepr = exp.repr
      if (expRepr.contains("???")) {
        logger.warn(s"unknown exp: ${exp}")
        guessInitValImpl(valType)
      } else expRepr
    }
    case None => guessInitValImpl(valType)
  }

  case class TVRecordItem(id: String, valType: String, range: Seq[VExplicitRange])

  case class TVRecord(id: String, items: Seq[TVRecordItem])

}

