package parsing

object VIType {

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

  case class TVRecordItem(id: String, valType: String, range: Seq[VExplicitRange])

  case class TVRecord(id: String, items: Seq[TVRecordItem])

}


