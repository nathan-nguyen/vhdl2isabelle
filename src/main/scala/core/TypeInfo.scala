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
  * - * customized: composite, subtype
  * - * - * composite: record, array
  */
sealed abstract class VTypeDefinition {
  val s: String
}

object VTypeDefinition {
  def apply(s: String): VTypeDefinition = {
    if (VBaseType.scalars(s)) {
      VScalarType(s)
    } else if (VBaseType.vectors(s)) {
      VVectorType(s)
    } else if (VCustomizedType.subTypes(s)){
      VSubtype(s)
    } else if (VCompositeType.recordTypes(s)) {
      VRecordType(s)
    } else if (VCompositeType.arrayTypes(s)) {
      VArrayType(s)
    } else handler(s"${s}")
  }
}

sealed abstract class VBaseType extends VTypeDefinition {
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

  def guessInitVal: IExp_baseTypeConstant = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", "0")
      case "real" => IConstS("val_r", "0.0")
      case "character" => IConstS("val_c", "(CHR ''0'')")
      case "boolean" => IConstS("val_b", "True")
      case "std_ulogic" => IConstS("val_c", "(CHR ''0'')")
      case "std_logic" => IConstS("val_c", "(CHR ''0'')")
      case _ => handler(s"scalar unknown ${s}")
    }
    IExp_baseTypeConstant(this, iconstS, ExpScalarKind)
  }

  def getInitValFromLiteral(v: String): IExp_baseTypeConstant = {
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
    IExp_baseTypeConstant(this, iconstS, ExpScalarKind)
  }

  def getInitVal(expOption: Option[VExp])(defInfo: DefInfo): IsabelleExpression = expOption match {
    case Some(exp) => {
      val refined = exp.toIExp(defInfo) match {
        case e: IExp_baseTypeConstant => IExp_baseTypeConstant(this, e.const, ExpScalarKind)
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

  def getInitVal(r: VRangeV, expOption: Option[VExp]): IExp_baseTypeConstant = {
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
  def guessInitVal(range: VRangeV, rawVal: Char = '0'): IExp_baseTypeConstant = {
    try {
      range.rangeD match {
        case RangeD.`to` => {
          val length = range.r.toInt - range.l.toInt + 1;
          IExp_baseTypeConstant(this, IConstL_gen(this, length.toString, rawVal), ExpVectorKindTo)
        }
        case RangeD.`downto` => {
          val length = range.l.toInt - range.r.toInt + 1
          IExp_baseTypeConstant(this, IConstRL_gen(this, length.toString, rawVal), ExpVectorKindDownTo)
        }
        case RangeD.`unkown` => handler(s"${range}")
      }
    } catch {
      case nfe : NumberFormatException => {
        range.rangeD match {
          case RangeD.`to` => {
            IExp_baseTypeConstant(this, IConstL_gen(this, s"(${range.r} - ${range.l} + 1)", rawVal), ExpVectorKindTo)
          }
          case RangeD.`downto` => {
            IExp_baseTypeConstant(this, IConstRL_gen(this, s"(${range.l} - ${range.r} + 1)", rawVal), ExpVectorKindDownTo)
          }
          case RangeD.`unkown` => handler(s"${range}")
        }
      }
      case e : Exception => ???
    }
  }
}

// CustomizedType does not exist in VHDL Parser
sealed abstract class VCustomizedType extends VTypeDefinition

object VCustomizedType {
  var subTypes = Set("");
}

// VSubType should be separate with VTypeDefinition
case class VSubtype(s: String) extends VCustomizedType {

  def guessInitVals(itemId: String, typeInfo : TypeInfo): MetaData = {
    val subtypeIndication = typeInfo.subtypeDeclarationMap(this)
    val valType = VTypeDefinition(subtypeIndication.getSimpleName)
    valType match {
      case baseType: VBaseType => {
        val initVal = baseType match {
          case scalarType: VScalarType => scalarType.guessInitVal
          case vectorType: VVectorType => {
            val range = subtypeIndication.getRange.getOrElse(defaultRangeV(s"guessListInit vector ${subtypeIndication}"))
            vectorType.guessInitVal(range)
          }
        }
        MetaData(itemId, baseType, initVal)
      }
      case customizedType : VCustomizedType => customizedType match{
        case subtype : VSubtype => {
          subtype.guessInitVals(itemId, typeInfo)
        }
        case recordType : VRecordType => handler(s"${recordType}")  // Subtype of a record is not implemented
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }

}

sealed abstract class VCompositeType extends VCustomizedType

object VCompositeType {
  var recordTypes = Set("");
  var arrayTypes = Set("");
}

case class VRecordType(s: String) extends VCompositeType {

  def guessInitVals(typeInfo : TypeInfo): List[MetaData] = {
    val recordInfo = typeInfo.recordTypeDeclarationMap(this)
    val iVals = for {
      (itemId, sti) <- recordInfo
    } yield {
      val valType = VTypeDefinition(sti.getSimpleName)
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
        case ct: VCustomizedType => ct match {
          case subtype : VSubtype => subtype.guessInitVals(itemId, typeInfo)
          case recordType : VRecordType => MetaData(itemId, recordType, IExp_recordTypeConstant(recordType, IConstRecord_gen(recordType), ExpRecordKind))
          case arrayType : VArrayType => MetaData(itemId, arrayType, arrayType.guessInitVals(itemId, typeInfo))
        }
      }
    }
    iVals.toList
  }

  // expOption is taken from definition; but type information should be record type declaration
  def getInitVals(typeInfo: TypeInfo, expOption: Option[VExp])(defInfo: DefInfo): List[MetaData] = {
    val iVals: Seq[MetaData] = expOption match {
      case Some(vExp) => {
        vExp.getPrimary match {
          case Some(VPrimaryAggregate(aggregate)) => {
            val recordInfo = typeInfo.recordTypeDeclarationMap(this)
            val aggregateIdExpMap = aggregate.getAssoc
            for {
              (itemId, sti) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = VTypeDefinition(sti.getSimpleName)
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
                  handler(s"${ct}")
                }
              }
              MetaData(itemId, itemValType, initVal)
            } // End of yield
          }
          case _ => {
            logger.warn(s"unknown record exp, guessing")
            guessInitVals(typeInfo)
          }
        }
      }
      case None => guessInitVals(typeInfo)
    }
    iVals.toList
  }

}

case class VArrayType(s: String) extends VCompositeType {

  // TODO : This implementation only consider 1 dimensional array - Need to implement multidimensional array
  def guessInitVals(itemId: String, typeInfo : TypeInfo): IsabelleExpression = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val subtypeIndication = constrainedArrayDefinition.subtypeIndication
    // FIXME getRange doesn't seem right - constrainedArrayDefinition.indexConstraint is list of range corresponding to multidimensional array

    val constOriginal = VTypeDefinition(subtypeIndication.getSimpleName) match {
      case scalarType : VScalarType => scalarType.guessInitVal.const
      case vectorType : VVectorType => {
        val vectorRange = subtypeIndication.getRange.getOrElse(defaultRangeV(s"${subtypeIndication}"))
        vectorType.guessInitVal(vectorRange).const
      }
      case subType : VSubtype => subType.guessInitVals(itemId, typeInfo).initVal.asInstanceOf[IExp_constant].const
      case recordType : VRecordType => handler(s"${recordType}")
      case arrayType : VArrayType => handler(s"${arrayType}")
    }

    val range = constrainedArrayDefinition.indexConstraint.getRange
    range.rangeD match {
      case RangeD.`to` => {
        val length = range.r.toInt - range.l.toInt + 1;
        IExp_arrayTypeConstant(this, IConstArrayL_gen(this, length, constOriginal), ExpArrayKindTo)
      }
      case RangeD.`downto` => {
        val length = range.l.toInt - range.r.toInt + 1
        IExp_arrayTypeConstant(this, IConstArrayRL_gen(this, length, constOriginal), ExpArrayKindDownTo)
      }
      case RangeD.`unkown` => handler(s"${range}")
    }
  }
}

class TypeInfo(private[this] val typeInfo: Option[TypeInfo]) {

  val recordTypeDeclarationMap: RecordTypeDeclarationMap = typeInfo match {
    case Some(ti) => ti.recordTypeDeclarationMap
    case None => mutable.Map.empty
  }

  val subtypeDeclarationMap: SubtypeDeclarationMap = typeInfo match {
    case Some(ti) => ti.subtypeDeclarationMap
    case None => mutable.Map.empty
  }

  val arrayTypeDeclarationMap: ArrayTypeDeclarationMap = typeInfo match {
    case Some(ti) => ti.arrayTypeDeclarationMap
    case None => mutable.Map.empty
  }

  def +=(id: VRecordType, items: RecordInfoSeq): Unit = {
    VCompositeType.recordTypes += id.s
    recordTypeDeclarationMap += (id -> items)
  }

  def +=(id: VSubtype, items: VSubtypeIndication): Unit = {
    VCustomizedType.subTypes += id.s
    subtypeDeclarationMap += (id -> items)
  }

  def +=(id: VArrayType, items: VConstrainedArrayDefinition): Unit = {
    VCompositeType.arrayTypes += id.s
    arrayTypeDeclarationMap += (id -> items)
  }
}