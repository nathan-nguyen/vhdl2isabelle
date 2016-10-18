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
sealed abstract class VTypeDefinition {
  val s: String
}

object VTypeDefinition {
  def apply(s: String): VTypeDefinition = {
    if (VBaseType.scalars(s)) {
      VScalarType(s)
    } else if (VBaseType.vectors(s)) {
      VVectorType(s)
    } else if (VCustomizedType.recordTypes(s)) {
      VRecordType(s)
    } else if (VCustomizedType.subTypes(s)){
      VSubtype(s)
    } else handler(s)
  }
}

abstract class VBaseType extends VTypeDefinition {
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

// CustomizedType does not exist in VHDL Parser
sealed abstract class VCustomizedType extends VTypeDefinition

object VCustomizedType {
  var subTypes = Set("");
  var recordTypes = Set("");
}

case class VRecordType(s: String) extends VCustomizedType {

  def guessInitVals(typeDeclarationMap: TypeDeclarationMap): List[MetaData] = {
    val recordInfo = typeDeclarationMap(this)
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
          case recordType : VRecordType => MetaData(itemId, valType, IExp_RecordConstant(recordType, IConstRecord_gen(recordType), ExpRecordKind))
          case subtype : VSubtype => handler(s"${subtype}")
        }
      }
    }
    iVals.toList
  }

  // expOption is taken from definition; but type information should be record type declaration
  def getInitVals(typeDeclarationMap: TypeDeclarationMap, expOption: Option[VExp])(defInfo: DefInfo): List[MetaData] = {
    val iVals: Seq[MetaData] = expOption match {
      case Some(vExp) => {
        vExp.getPrimary match {
          case Some(VPrimaryAggregate(aggregate)) => {
            val recordInfo = typeDeclarationMap(this)
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
            guessInitVals(typeDeclarationMap)
          }
        }
      }
      case None => guessInitVals(typeDeclarationMap)
    }
    iVals.toList
  }

}

// VSubType should be separate with VTypeDefinition
case class VSubtype(s: String) extends VCustomizedType

class TypeInfo(private[this] val typeInfo: Option[TypeInfo]) {

  val typeDeclaration: TypeDeclarationMap = typeInfo match {
    case Some(ti) => ti.typeDeclaration
    case None => mutable.Map.empty
  }

  val subtypeDeclaration: SubtypeInfoMap = typeInfo match {
    case Some(ti) => ti.subtypeDeclaration
    case None => mutable.Map.empty
  }

  def updateRecordType(id: VRecordType, items: RecordInfoSeq): Unit = {
    VCustomizedType.recordTypes += id.s
    typeDeclaration += (id -> items)
  }

  def updateSubType(id: VSubtype, items: VSubtypeIndication): Unit = {
    VCustomizedType.subTypes += id.s
    subtypeDeclaration += (id -> items)
  }
}