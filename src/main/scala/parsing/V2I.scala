package parsing

import org.slf4j.LoggerFactory

import scala.collection.mutable

object V2I {

  //  consider adding a type of types to denotes "scalar"/"list" --> vector

  val logger = LoggerFactory.getLogger(this.getClass)

  def VHDLize(vhdlType: String) = s"vhdl_${vhdlType}"

  class TypeInfo {
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

    def _guessScalarInitVal(valType: String): IVariable = valType match {
      case "integer" => IVariable("val_i", "0")
      case "real" => IVariable("val_r", "0.0")
      case "character" => IVariable("val_c", "(CHR ''0'')")
      case "boolean" => IVariable("val_b", "True)")
      case "std_ulogic" => IVariable("val_c", "(CHR ''0'')")
      case "std_logic" => IVariable("val_c", "(CHR ''0'')")
      case _ => IVariable("TODO", "Scalar unknown")
    }

    def getScalarInitVal(valType: String, expOption: Option[VExp]): IExp = expOption match {
      case Some(exp) => {
        val expRepr = exp.repr
        if (expRepr.contains("???")) {
          logger.warn(s"unknown exp, guessing")
          IExp_con(valType, _guessScalarInitVal(valType))
        } else {
          //        repr -> IExpr
          IVariable("TODO", "Scalar repr")
        }
      }
      case None => IExp_con(valType, _guessScalarInitVal(valType))
    }

    def _guessListInitVals(rawType: String): List[IValue] = {
      val recordInfo = typeDeclTbl(rawType)
      //    TODO not exactly scalar type
      val iVals = for {
        (itemId, itemTyInfo) <- recordInfo
        valType = itemTyInfo.selectedName
        iVar = _guessScalarInitVal(valType)
      } yield IValue(itemId, valType, iVar)
      iVals.toList
    }

    def _guessVectorInitVal(valType: String, r: (Int, Int)): IExp = {
      require(valType.endsWith("_vector"), "vector")
      val genCmd = valType.substring(0, valType.length - "_vector".length) + "_vec_gen"
      val valListType = if (r._1 <= r._2) "val_list" else "val_rlist"
      val iVarChar = IVariable("val_c", "(CHR ''0'')")
      IVariable(valListType, s"${valListType} (${genCmd} ${Math.abs(r._1 - r._2) + 1} ${iVarChar})")
    }

    def getListInitVals(valType: String, expOption: Option[VExp]): List[IValue] = {
      require(isListType(valType), s"${valType} should be composite")
      expOption match {
        case Some(vExp) => {
          val expRepr = vExp.repr
          if (expRepr.contains("???")) {
            logger.warn("unknown composite exp, guessing")
            List.empty
          } else {
            logger.info("comoposite exp, TODO")
            List.empty
          }
        }
        case None => _guessListInitVals(valType)
      }
    }
  }

  //  for type rather than  value
  case class TVRecordItem(id: String, valType: String, range: Seq[VExplicitRange])

  case class TVRecord(id: String, items: Seq[TVRecordItem])

}

