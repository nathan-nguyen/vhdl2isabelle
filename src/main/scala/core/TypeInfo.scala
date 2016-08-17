package core

import scala.collection.mutable

///////////////////////////////////////////////////////////////
/**
  * it's about the type information for variables defined in isar,
  * which has nothing to do with entity generation
  */
///////////////////////////////////////////////////////////////

/**
  * 3 kinds of types <---
  * - * scalar: integer, std_logic
  * - * vector: std_ulogic_vector
  * - + customized: div32_in_type
  */
sealed abstract class VValType {
  val s: String
}

object VValType {
  def apply(s: String): VValType = {
    if (VBaseType.scalars(s)) {
      VScalarType(s)
    } else if (VBaseType.vectors(s)) {
      VVectorType(s)
    } else {
      // must be defined before; however should be checked outside
      VCustomizedType(s)
    }
  }
}

abstract class VBaseType extends VValType {
  val s: String
}

object VBaseType {
  val scalars = Set("character", "integer", "real", "boolean", "std_logic", "std_ulogic", "natural")
  val vectors = Set("std_logic_vector", "std_ulogic_vector")
}

case class VScalarType(s: String) extends VBaseType {

  def guessInitVal: IExp_con = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", "0")
      case "real" => IConstS("val_r", "0.0")
      case "character" => IConstS("val_c", "(CHR ''0'')")
      case "boolean" => IConstS("val_b", "True")
      case "std_ulogic" => IConstS("val_c", "(CHR ''0'')")
      case "std_logic" => IConstS("val_c", "(CHR ''0'')")
      case _ => handler(s"scalar unknown ${s}")
    }
    IExp_con(this, iconstS, ExpScalarKind)
  }

  def getInitValFromLiteral(v: String): IExp_con = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", v)
      case "real" => IConstS("val_r", v)
      case "character" => IConstS("val_c", s"(CHR '${v}')")
      case "boolean" => IConstS("val_b", v.capitalize)
      case "std_ulogic" => IConstS("val_c", s"(CHR '${v}')")
      case "std_logic" => IConstS("val_c", s"(CHR '${v}')")
      // NOT in guess
      case "natural" => IConstS("val_i", v)
      case _ => handler(s"scalar unknown ${toString} (${v})")
    }
    IExp_con(this, iconstS, ExpScalarKind)
  }


  def getInitVal(expOption: Option[VExp])(defInfo: DefInfo): IExp = expOption match {
    case Some(exp) => {
      val refined = exp.toIExp(defInfo) match {
        case e: IExp_con => IExp_con(this, e.const, ExpScalarKind)
        case o => o
      }
      val expRepr = refined.toString
      if (expRepr.contains(unknownString)) {
        logger.warn(s"unknown exp === ${expRepr}")
        guessInitVal
      } else {
        refined
      }
    }
    case None => guessInitVal
  }
}

case class VVectorType(s: String) extends VBaseType {

  // FIXME
  def getInitVal(r: VRangeTy, expOption: Option[VExp]): IExp_con = {
    expOption match {
      case Some(vExp) => {
        logger.info(s"${vExp}")
        val literalS = vExp.getLiteralS
        literalS.num2Exp(VScalarType("character"), r)
      }
      case None => guessInitVal(r)
    }
  }

  // this has something to do with TO/DOWNTO but not about "character"/"std_logic"/"std_ulogic"
  def guessInitVal(r: VRangeTy, numericVal: String = "'0'"): IExp_con = {
    val genCmd = s.substring(0, s.length - "_vector".length) + "_vec_gen"
    val valListType = if (r.rangeD == RangeD.to) "val_list" else if (r.rangeD == RangeD.downto) "val_rlist" else unknownString
    val initValue = if (List("val_lsit", "val_rlist").contains(valListType)) {
      val iVarChar = IConstS("val_c", s"(CHR '${numericVal}')")
      IConstS(valListType, s"(${genCmd} ${Math.abs(r.l.toInt - r.r.toInt) + 1} ${iVarChar})")
    } else {
      IConstS(valListType, unknownString)
    }
    IExp_con(this, initValue, ExpVectorKind)
  }
}

case class VCustomizedType(s: String) extends VValType {

  def guessInitVals(typeDeclTbl: TDTy): List[MetaData] = {
    val recordInfo = typeDeclTbl(this)
    val iVals = for {
      (itemId, sti) <- recordInfo
    } yield {
      val valType = VValType(sti.getSimpleName)
      valType match {
        case bt: VBaseType => {
          val initVal = bt match {
            case st: VScalarType => st.guessInitVal
            case vt: VVectorType => {
              val range = sti.getRange.getOrElse(defaultRange(s"guessListInit vector ${sti}"))
              vt.guessInitVal(range)
            }
          }
          MetaData(itemId, bt, initVal)
        }
        case ct: VCustomizedType => {
          val initVals = ct.guessInitVals(typeDeclTbl)
          //      TODO    IValue shoud have other forms
          MetaData(itemId, valType, handler(s"list-list ${initVals}"))
        }
      }
    }
    iVals.toList
  }

  // expOption is taken from definition; but type information should be record type declaration
  def getInitVals(typeDeclTbl: TDTy, expOption: Option[VExp])(defInfo: DefInfo): List[MetaData] = {
    val iVals: Seq[MetaData] = expOption match {
      case Some(vExp) => {
        vExp.getPrimary match {
          case Some(VPrimaryAggregate(aggregate)) => {
            val recordInfo = typeDeclTbl(this)
            val aggregateIdExpMap = aggregate.getAssoc
            for {
              (itemId, sti) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = VValType(sti.getSimpleName)
              val initVal: IExp = itemValType match {
                case bt: VBaseType => bt match {
                  case st: VScalarType => {
                    st.getInitVal(Option(itemExp))(defInfo)
                  }
                  case vt: VVectorType => {
                    itemExp.getAggregate match {
                      case Some(itemAggregate) => {
                        val (fieldName, innerExp) = itemAggregate.getFirstMap
                        if (fieldName == "others") {
                          val range = sti.getRange.getOrElse(defaultRange(s"getListInit vector ${sti}"))
                          val numericVal = innerExp.getPrimary
                          numericVal match {
                            case Some(p) => {
                              vt.guessInitVal(range, p.asVal)
                            }
                            case None => handler(s"${numericVal}")
                          }
                        } else {
                          logger.info(s"${itemAggregate.getFirstMap}")
                          handler(s"${itemValType}")
                        }
                      }
                      case None => handler(s"${itemExp}")
                    }
                  }
                }
                case ct: VCustomizedType => {
                  handler(s"list-list: ${itemValType}")
                }
              }
              MetaData(itemId, itemValType, initVal)
            } // end of yield
          }
          case _ => {
            logger.warn(s"unknown record exp, guessing")
            guessInitVals(typeDeclTbl)
          }
        }
      }
      case None => guessInitVals(typeDeclTbl)
    }
    iVals.toList
  }

}


class TypeInfo(private[this] val typeInfo: Option[TypeInfo]) {

  val typeDeclTbl: TDTy = typeInfo match {
    case Some(ti) => ti.typeDeclTbl
    case None => mutable.Map.empty
  }

  def +=(id: VCustomizedType, items: RecordInfoTy): Unit = typeDeclTbl += (id -> items)
}