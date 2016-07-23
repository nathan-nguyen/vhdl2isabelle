package parsing

object VVRange {

  case class VVExplicitRange(direction: String, rangeVal: Seq[String])

  case class VRecordItem(id: String, valType: String, range: Seq[VVExplicitRange])

  case class VRecord(id: String, items: Seq[VRecordItem])

}


