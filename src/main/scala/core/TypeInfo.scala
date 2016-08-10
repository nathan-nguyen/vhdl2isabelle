package core

import scala.collection.mutable

///////////////////////////////////////////////////////////////
/**
  * it's about the type information for variables defined in isar,
  * which has nothing to do with entity generation
  */
///////////////////////////////////////////////////////////////

class TypeInfo(private[this] val typeInfo: Option[TypeInfo]) {

  import V2IUtils._

  type RecordInfoTy = Seq[(String, VSubtypeInd)]

  val typeDeclTbl: mutable.Map[String, RecordInfoTy] = typeInfo match {
    case Some(ti) => ti.typeDeclTbl
    case None => mutable.Map.empty
  }

  def +=(id: String, items: RecordInfoTy): Unit = typeDeclTbl += (id -> items)

//  val knownListType = Set("div32_in_type", "div32_out_type")

  def isListType(valType: String) = {
//    if (knownListType.contains(valType)) true
    if (typeDeclTbl.contains(valType)) true
    else false
  }

  def isVectorType(valType: String) = valType.contains("_vector")

  def fillValType(valType: String, exp_con: IExp_con) = IExp_con(valType, exp_con.const)

  def getScalarInitVal(valType: String, expOption: Option[VExp])(defInfo: DefInfo): IExp = expOption match {
    case Some(exp) => {
      val refined = exp.toIExp(defInfo) match {
        case e:IExp_con => fillValType(valType, e)
        case o => o
      }
      val expRepr = refined.toString
      if (expRepr.contains(unknownString)) {
        logger.warn(s"unknown exp === ${expRepr}")
        fillInDefaultScalaVal(valType)
      } else {
        refined
      }
    }
    case None => fillInDefaultScalaVal(valType)
  }

  def _guessListInitVals(rawType: String): List[MetaData] = {
    val recordInfo = typeDeclTbl(rawType)
    val iVals = for {
      (itemId, sti) <- recordInfo
    } yield {
      val valType = sti.getSimpleName
      if (isListType(valType)) {
        val initVals = _guessListInitVals(valType)
        //      TODO    IValue shoud have other forms
        MetaData(itemId, valType, defaultExpCon(s"list-list ${initVals}"))
      } else {
        val initVal = if (isVectorType(valType)) {
          val range = sti.getRange.getOrElse(defaultRange(s"guessListInit vector ${sti}"))
          _guessVectorInitVal(valType, range)
        } else {
          fillInDefaultScalaVal(valType)
        }
        MetaData(itemId, valType, initVal)
      }
    }
    iVals.toList
  }

  def _guessVectorInitVal(valType: String, r: RangeTy, numericVal: String = "'0'"): IExp_con = {
    require(valType.endsWith("_vector"), "vector")
    val genCmd = valType.substring(0, valType.length - "_vector".length) + "_vec_gen"
    val valListType = if (r._2 == "to") "val_list" else if (r._2 == "downto") "val_rlist" else unknownString
    val initValue = if (List("val_lsit", "val_rlist").contains(valListType)) {
      val iVarChar = IConstS("val_c", s"(CHR '${numericVal}')")
      IConstS(valListType, s"(${genCmd} ${Math.abs(r._1.toInt - r._3.toInt) + 1} ${iVarChar})")
    } else {
      IConstS(valListType, unknownString)
    }
    IExp_con(valType, initValue)
  }

  //    expOption  is taken from definition; but type information should be record type declaration
  def getListInitVals(valType: String, expOption: Option[VExp])(defInfo: DefInfo): List[MetaData] = {
    require(isListType(valType), s"${valType} should be composite")
    val iVals: Seq[MetaData] = expOption match {
      case Some(vExp) => {
        vExp.getPrimary match {
          case Some(VPrimaryAggregate(aggregate)) => {
            val recordInfo = typeDeclTbl(valType)
            val aggregateIdExpMap = aggregate.getAssoc
            for {
              (itemId, sti) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = sti.getSimpleName
              val initValue: IExp = if (isListType(itemValType)) {
                defaultExpCon(s"list-list: ${itemValType}")
              } else if (isVectorType(itemValType)) {
                itemExp.getAggregate match {
                  case Some(itemAggregate) => {
                    val (fieldName, innerExp) = itemAggregate.getFirstMap
                    if (fieldName == "others") {
                      val range = sti.getRange.getOrElse(defaultRange(s"getListInit vector ${sti}"))
                      val numericVal = innerExp.getPrimary
                      numericVal match {
                        case Some(p) => {
                          _guessVectorInitVal(itemValType, range, p.asVal)
                        }
                        case None => defaultExpCon(s"${numericVal}")
                      }
                    } else {
                      logger.info(s"${itemAggregate.getFirstMap}")
                      defaultExpCon(s"${itemValType}")
                    }
                  }
                  case None => defaultExpCon(s"${itemExp}")
                }
              } else {
                getScalarInitVal(itemValType, Option(itemExp))(defInfo)
              }
              MetaData(itemId, itemValType, initValue)
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