package parsing

import scala.collection.mutable

class TypeInfo {
  import V2IUtils._

  ///////////////////////////////////////////////////////////////
  //  for type rather than value
  case class TVRecordItem(id: String, valType: String, range: Seq[VExplicitRange])

  case class TVRecord(id: String, items: Seq[TVRecordItem])

  ///////////////////////////////////////////////////////////////

  type RecordInfoTy = Seq[(String, VSubtypeIndication)]
  val typeDeclTbl = mutable.Map.empty[String, RecordInfoTy]

  def addNewType(id: String, items: RecordInfoTy): Unit = {
    typeDeclTbl += (id -> items)
  }

  val knownListType = Set("div32_in_type", "div32_out_type")

  def isListType(valType: String) = {
    if (knownListType.contains(valType)) true
    else if (typeDeclTbl.contains(valType)) true
    else false
  }

  def isVectorType(valType: String) = valType.contains("_vector")

  def decorate(rawIdType: String, valType: String) = {
    if (isListType(valType)) s"${rawIdType} list" else rawIdType
  }

  def _guessScalarInitVal(valType: String): IExp_con = {
    val initValue = valType match {
      case "integer" => IVariable("val_i", "0")
      case "real" => IVariable("val_r", "0.0")
      case "character" => IVariable("val_c", "(CHR ''0'')")
      case "boolean" => IVariable("val_b", "True")
      case "std_ulogic" => IVariable("val_c", "(CHR ''0'')")
      case "std_logic" => IVariable("val_c", "(CHR ''0'')")
      case _ => defaultScalarValue(s"scalar unknown ${valType}")
    }
    IExp_con(s"${valType}", initValue)
  }

  def getScalarInitVal(valType: String, expOption: Option[VExp]): IExp = expOption match {
    case Some(exp) => {
      val expRepr = exp.toIExp.toString
      if (expRepr.contains("???")) {
        logger.warn(s"unknown exp === ${expRepr}")
        _guessScalarInitVal(valType)
      } else {
        //        repr -> IExpr
        exp.toIExp match {
          case iVar: IVariable => IExp_con(valType, iVar)
          case e => e
        }
      }
    }
    case None => _guessScalarInitVal(valType)
  }

  def _guessListInitVals(rawType: String): List[IScalarOrVecIval] = {
    val recordInfo = typeDeclTbl(rawType)
    val iVals = for {
      (itemId, subtypeIndication) <- recordInfo
    } yield {
      val valType = subtypeIndication.selectedName
      if (isListType(valType)) {
        val initVals = _guessListInitVals(valType)
        //      TODO    IValue shoud have other forms
        IScalarOrVecIval(itemId, valType, defaultScalarValue(s"list-list ${initVals}"))
      } else {
        val initVal = if (isVectorType(valType)) {
          val range = subtypeIndication.getRange.getOrElse(defaultRange(s"guessListInit vector ${subtypeIndication}"))
          _guessVectorInitVal(valType, range)
        } else {
          _guessScalarInitVal(valType)
        }
        IScalarOrVecIval(itemId, valType, initVal)
      }
    }
    iVals.toList
  }

  def _guessVectorInitVal(valType: String, r: RangeTy, numericVal: String = "'0'"): IExp_con = {
    require(valType.endsWith("_vector"), "vector")
    val genCmd = valType.substring(0, valType.length - "_vector".length) + "_vec_gen"
    val valListType = if (r._2 == "to") "val_list" else if (r._2 == "downto") "val_rlist" else "???"
    val initValue = if (List("val_lsit", "val_rlist").contains(valListType)) {
      val iVarChar = IVariable("val_c", s"(CHR '${numericVal}')")
      IVariable(valListType, s"(${genCmd} ${Math.abs(r._1.toInt - r._3.toInt) + 1} ${iVarChar})")
    } else {
      IVariable(valListType, s"???")
    }
    IExp_con(valType, initValue)
  }

  //    expOption  is taken from definition; but type information should be record type declaration
  def getListInitVals(valType: String, expOption: Option[VExp]): List[IScalarOrVecIval] = {
    require(isListType(valType), s"${valType} should be composite")
    val iVals: Seq[IScalarOrVecIval] = expOption match {
      case Some(vExp) => {
        vExp.getPrimary match {
          case Some(VPrimaryAggregate(aggregate)) => {
            val recordInfo = typeDeclTbl(valType)
            val aggregateIdExpMap = aggregate.getAssoc
            for {
              (itemId, subtypeIndication) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = subtypeIndication.selectedName
              val initValue: IExp = if (isListType(itemValType)) {
                defaultScalarValue(s"list-list: ${itemValType}")
              } else if (isVectorType(itemValType)) {
                itemExp.getAggregate match {
                  case Some(itemAggregate) => {
                    val (fieldName, innerExp) = itemAggregate.getFirstMap
                    if (fieldName == "others") {
                      val range = subtypeIndication.getRange.getOrElse(defaultRange(s"getListInit vector ${subtypeIndication}"))
                      val numericVal = innerExp.getPrimary
                      numericVal match {
                        case Some(p) => {
                          _guessVectorInitVal(itemValType, range, p.asVal)
                        }
                        case None => defaultScalarValue(s"${numericVal}")
                      }
                    } else {
                      logger.info(s"${itemAggregate.getFirstMap}")
                      defaultScalarValue(s"${itemValType}")
                    }
                  }
                  case None => defaultScalarValue(s"${itemExp}")
                }
              } else {
                getScalarInitVal(itemValType, Option(itemExp))
              }
              IScalarOrVecIval(itemId, itemValType, initValue)
            } // end of yield
          }
          case _ => {
            logger.warn(s"unknown record exp, guessing")
            _guessListInitVals(valType)
          }
        }
      }
      case None => _guessListInitVals(valType)
    }
    iVals.toList
  }
}