package core

import scala.collection.mutable

/**
  * Created by Hongxu Chen.
  */

/**
  * It's about the type information for variables defined in isabelle,
  * which has nothing to do with entity generation
  */

/**
  * 3 kinds of types <---
  * - * scalar: integer, std_logic, std_ulogic
  * - * vector: std_logic_vector, std_ulogic_vector
  * - * customized:
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

  def vectorize: VVectorType = {
    require(s.startsWith("std_"))
    //    require(s == "std_logic" || s == "std_ulogic")
    VVectorType(s + vectorFlag)
  }

  def guessInitVal: IExp_constant = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", "0")
      case "real" => IConstS("val_r", "0.0")
      case "character" => IConstS("val_c", "(CHR ''0'')")
      case "boolean" => IConstS("val_b", "True")
      case "std_ulogic" => IConstS("val_c", "(CHR ''0'')")
      case "std_logic" => IConstS("val_c", "(CHR ''0'')")
      case _ => handler(s"scalar unknown ${s}")
    }
    IExp_constant(this, iconstS, ExpScalarKind)
  }

  def getInitValFromLiteral(v: String): IExp_constant = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", v)
      case "real" => IConstS("val_r", v)
      case "character" => IConstS("val_c", s"(CHR '${v}')")
      case "boolean" => IConstS("val_b", v.capitalize)
      case "std_ulogic" => IConstS("val_c", s"(CHR '${v}')")
      case "std_logic" => IConstS("val_c", s"(CHR '${v}')")
      // NOT in guess
      case "natural" => IConstS("val_i", v)
      case `defaultCharType` => IConstS("val_c", s"(CHR '${v}')")
      case _ => handler(s"scalar unknown ${toString} (${v})")
    }
    IExp_constant(this, iconstS, ExpScalarKind)
  }

  def getInitVal(expOption: Option[VExp])(defInfo: DefInfo): IsabelleExpression = expOption match {
    case Some(exp) => {
      val refined = exp.toIExp(defInfo) match {
        case e: IExp_constant => IExp_constant(this, e.const, ExpScalarKind)
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
  require(s.endsWith(vectorFlag))

  def scalarize: VScalarType = VScalarType(s.substring(0, s.length - vectorFlag.length))

  def getInitVal(r: VRangeV, expOption: Option[VExp]): IExp_constant = {
    expOption match {
      case Some(vExp) => {
        val literalS = vExp.getLiteralS
        val scalarType = scalarize
        literalS.num2Exp(scalarType, r)
      }
      case None => guessInitVal(r)
    }
  }

  // This has something to do with TO/DOWNTO but not about "character"/"std_logic"/"std_ulogic"
  def guessInitVal(range: VRangeV, rawVal: Char = '0'): IExp_constant = {
    try {
      range.rangeD match {
        case RangeD.`to` => {
          val length = range.r.toInt - range.l.toInt + 1;
          IExp_constant(this, IConstL_gen(this, length.toString, rawVal), ExpVectorKindTo)
        }
        case RangeD.`downto` => {
          val length = range.l.toInt - range.r.toInt + 1
          IExp_constant(this, IConstRL_gen(this, length.toString, rawVal), ExpVectorKindDownTo)
        }
        case RangeD.`unkown` => handler(s"${range}")
      }
    } catch {
      case nfe : NumberFormatException => {
        range.rangeD match {
          case RangeD.`to` => {
            IExp_constant(this, IConstL_gen(this, s"(${range.r} - ${range.l} + 1)", rawVal), ExpVectorKindTo)
          }
          case RangeD.`downto` => {
            IExp_constant(this, IConstRL_gen(this, s"(${range.l} - ${range.r} + 1)", rawVal), ExpVectorKindDownTo)
          }
          case RangeD.`unkown` => handler(s"${range}")
        }
      }
      case e : Exception => ???
    }
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
              val range = sti.getRange.getOrElse(defaultRangeV(s"guessListInit vector ${sti}"))
              vt.guessInitVal(range)
            }
          }
          MetaData(itemId, bt, initVal)
        }
        case ct: VCustomizedType =>
          MetaData(itemId, valType, IExp_customizedConstant(ct, IConstCustomized_gen(ct), ExpCustomizedKind))
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
              val initVal: IsabelleExpression = itemValType match {
                case bt: VBaseType => bt match {
                  case st: VScalarType => {
                    st.getInitVal(Option(itemExp))(defInfo)
                  }
                  case vt: VVectorType => {
                    itemExp.getAggregate match {
                      case Some(itemAggregate) => {
                        val (fieldName, innerExp) = itemAggregate.getFirstMap
                        if (fieldName == "others") {
                          val range = sti.getRange.getOrElse(defaultRangeV(s"getListInit vector ${sti}"))
                          val numericVal = innerExp.getPrimary
                          numericVal match {
                            case Some(p) => {
                              val rawVal = p.asVal
                              require(rawVal.length == 3 && rawVal.head == '\'' && rawVal.last == '\'')
                              vt.guessInitVal(range, rawVal(1))
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
                  val printVal = println("\n\nprintVal " + ct + "\n\n")
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