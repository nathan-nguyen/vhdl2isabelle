package parsing

object TVExp {

  case class TVExplicitRange(direction: String, rangeVal: Seq[String])

  case class TVRecordItem(id: String, valType: String, range: Seq[TVExplicitRange])

  case class TVRecord(id: String, items: Seq[TVRecordItem])

}


