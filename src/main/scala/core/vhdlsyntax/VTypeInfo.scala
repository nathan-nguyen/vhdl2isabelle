package core.vhdlsyntax

import core._
import core.isabellesyntax._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
    if (VBaseType.scalars(s)) VScalarType(s)
    else if (VBaseType.vectors(s)) VVectorType(s)
    else if (VCustomizedType.subTypes(s))VSubtype(s)
    else if (VCompositeType.recordTypes(s)) VRecordType(s)
    else if (VCompositeType.arrayTypes(s)) VArrayType(s)
    else handler(s"${s}")
  }

  // [TN] This method is not complete because VSubtype might have original type is VRecordType
  def getOriginalType(vSubtypeIndication: VSubtypeIndication)(typeInfo: VTypeInfo): VBaseType = {
    VTypeDefinition(vSubtypeIndication.getSimpleName) match {
      case vBaseType: VBaseType => vBaseType
      case vSubtype: VSubtype => getOriginalType(typeInfo.subtypeDeclarationMap(vSubtype))(typeInfo)
      case _ => ???
    }
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
    VVectorType(s + vectorFlag)
  }

  def guessInitialValue: IExpression_constantBaseType = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", "0")
      case "real" => IConstS("val_r", "0.0")
      case "character" => IConstS("val_c", "(CHR ''0'')")
      case "boolean" => IConstS("val_b", "True")
      case "std_ulogic" => IConstS("val_c", "(CHR ''0'')")
      case "std_logic" => IConstS("val_c", "(CHR ''0'')")
      case _ => handler(s"scalar unknown ${s}")
    }
    IExpression_constantBaseType(this, iconstS, ExpScalarKind)
  }

  def getInitValFromLiteral(v: String): IExpression_constantBaseType = {
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
    IExpression_constantBaseType(this, iconstS, ExpScalarKind)
  }

  def getInitialValue(vExpressionOption: Option[VExpression])(defInfo: DefInfo): IExpression = vExpressionOption match {
    case Some(vExpression) => {
      val refined = vExpression.toIExp(defInfo) match {
        case iExpression: IExpression_constantBaseType => IExpression_constantBaseType(this, iExpression.iConst, ExpScalarKind)
        case o => o
      }
      val expRepr = refined.toString
      if (expRepr.contains(unknownString)) {
        logger.warn(s"unknown exp === ${expRepr}")
        guessInitialValue
      } else {
        refined
      }
    }
    case None => guessInitialValue
  }
}

case class VVectorType(s: String) extends VBaseType {
  require(s.endsWith(vectorFlag))

  def scalarize: VScalarType = VScalarType(s.substring(0, s.length - vectorFlag.length))

  def getInitialValue(vExplicitRange: VExplicitRange, expOption: Option[VExpression]): IExpression_constantBaseType = {
    expOption match {
      case Some(vExpression) => {
        val literalS = vExpression.getLiteralS
        val scalarType = scalarize
        literalS.arrayLiteralToExpression(scalarType, vExplicitRange.vDirection)
      }
      case None => guessInitialValue(vExplicitRange)
    }
  }

  // This has something to do with TO/DOWNTO but not about "character"/"std_logic"/"std_ulogic"
  def guessInitialValue(vExplicitRange: VExplicitRange, rawVal: Char = '0'): IExpression_constantBaseType = {
    try {
      vExplicitRange.vDirection match {
        case VDirection.`to` => {
          val length = vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1;
          IExpression_constantBaseType(this, IConstL_gen(this, length.toString, rawVal), ExpVectorKindTo)
        }
        case VDirection.`downto` => {
          val length = vExplicitRange.left.getStringValue.toInt - vExplicitRange.right.getStringValue.toInt + 1
          IExpression_constantBaseType(this, IConstRL_gen(this, length.toString, rawVal), ExpVectorKindDownTo)
        }
      }
    } catch {
      case nfe : NumberFormatException => {
        vExplicitRange.vDirection match {
          case VDirection.`to` => {
            IExpression_constantBaseType(this, IConstL_gen(this, s"(${vExplicitRange.right.getStringValue} - ${vExplicitRange.left.getStringValue} + 1)", rawVal), ExpVectorKindTo)
          }
          case VDirection.`downto` => {
            IExpression_constantBaseType(this, IConstRL_gen(this, s"(${vExplicitRange.left.getStringValue} - ${vExplicitRange.right.getStringValue} + 1)", rawVal), ExpVectorKindDownTo)
          }
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

  def guessInitialValue(itemId: String, typeInfo : VTypeInfo): MetaData = {
    val vSubtypeIndication = typeInfo.subtypeDeclarationMap(this)
    val valType = VTypeDefinition(vSubtypeIndication.getSimpleName)
    valType match {
      case baseType: VBaseType => {
        val initVal = baseType match {
          case scalarType: VScalarType => scalarType.guessInitialValue
          case vectorType: VVectorType => {
            vSubtypeIndication.getExplicitRangeOption match {
              case Some(vExplicitRange) => vectorType.guessInitialValue(vExplicitRange)
              case None => ???
            }
          }
        }
        MetaData(itemId, baseType, initVal)
      }
      case customizedType : VCustomizedType => customizedType match{
        case subtype : VSubtype => {
          subtype.guessInitialValue(itemId, typeInfo)
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

  def guessInitVals(typeInfo : VTypeInfo): List[MetaData] = {
    val recordInfo = typeInfo.recordTypeDeclarationMap(this)
    val iVals = for {
      (itemId, vSubtypeIndication) <- recordInfo
    } yield {
      val valType = VTypeDefinition(vSubtypeIndication.getSimpleName)
      valType match {
        case bt: VBaseType => {
          val initVal = bt match {
            case st: VScalarType => st.guessInitialValue
            case vt: VVectorType => {
              vSubtypeIndication.getExplicitRangeOption match {
                case Some(vExplicitRange) => vt.guessInitialValue(vExplicitRange)
                case None => ???
              }
            }
          }
          MetaData(itemId, bt, initVal)
        }
        case ct: VCustomizedType => ct match {
          case subtype : VSubtype => subtype.guessInitialValue(itemId, typeInfo)
          case recordType : VRecordType => MetaData(itemId, recordType, IExpression_constantRecordType(recordType, IConstRecord_gen(recordType), ExpRecordKind))
          case arrayType : VArrayType => MetaData(itemId, arrayType, arrayType.guessInitialValue(itemId, typeInfo))
        }
      }
    }
    iVals.toList
  }

  // [HC] vExpressionOption is taken from definition; but type information should be record type declaration
  def getInitialValue(typeInfo: VTypeInfo, vExpressionOption: Option[VExpression])(defInfo: DefInfo): List[MetaData] = {
    val iVals: Seq[MetaData] = vExpressionOption match {
      case Some(vExpression) => {
        vExpression.getPrimary match {
          case Some(vAggregate: VAggregate) => {
            val recordInfo = typeInfo.recordTypeDeclarationMap(this)
            val aggregateIdExpMap = vAggregate.getAssoc
            for {
              (itemId, vSubtypeIndication) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = VTypeDefinition(vSubtypeIndication.getSimpleName)
              val initVal: IExpression = itemValType match {
                case bt: VBaseType => bt match {
                  case st: VScalarType => {
                    st.getInitialValue(Option(itemExp))(defInfo)
                  }
                  case vt: VVectorType => {
                    itemExp.getAggregate match {
                      case Some(itemAggregate) => {
                        val (fieldName, innerExp) = itemAggregate.getFirstMap
                        if (fieldName == "others") {
                          val vExplicitRange = vSubtypeIndication.getExplicitRangeOption match {
                            case Some(vExplicitRange) => vExplicitRange
                            case None => ???
                          }
                          val numericVal = innerExp.getPrimary
                          numericVal match {
                            case Some(p) => {
                              val rawVal = p.getStringValue
                              require(rawVal.length == 3 && rawVal.head == '\'' && rawVal.last == '\'')
                              vt.guessInitialValue(vExplicitRange, rawVal(1))
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
  def guessInitialValue(itemId: String, typeInfo : VTypeInfo): IExpression = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val vSubtypeIndication = constrainedArrayDefinition.vSubtypeIndication
    val iExpression_constant = VTypeDefinition(vSubtypeIndication.getSimpleName) match {
      case scalarType : VScalarType => scalarType.guessInitialValue.iConst
      case vectorType : VVectorType => {
        vSubtypeIndication.getExplicitRangeOption match {
          case Some(vExplicitRange) => vectorType.guessInitialValue(vExplicitRange).iConst
          case None => ???
        }

      }
      case subType : VSubtype => subType.guessInitialValue(itemId, typeInfo).iExpression.asInstanceOf[IExpression_constant].iConst
      case recordType : VRecordType => handler(s"${recordType}")
      case arrayType : VArrayType => handler(s"${arrayType}")
    }

    val range = constrainedArrayDefinition.indexConstraint.getExplicitRange
    range.vDirection match {
      case VDirection.`to` => {
        val length = range.right.getStringValue.toInt - range.left.getStringValue.toInt + 1;
        IExpression_constantArrayType(this, IConstArrayTo_generate(this, length, iExpression_constant), ExpArrayKindTo)
      }
      case VDirection.`downto` => {
        val length = range.left.getStringValue.toInt - range.right.getStringValue.toInt + 1
        IExpression_constantArrayType(this, IConstArrayDownTo_generate(this, length, iExpression_constant), ExpArrayKindDownTo)
      }
    }
  }

  def getInitialValue(id:String, typeInfo: VTypeInfo, vExpressionOption: Option[VExpression])(defInfo: DefInfo): IExpression = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)

    val arrayRange = constrainedArrayDefinition.indexConstraint.getExplicitRange
    var startIndex: Int  = arrayRange.left.getStringValue.toInt
    var multiplyIndex: Int = 1
    val arrayLength = arrayRange.vDirection match {
      case VDirection.`to` => {
        multiplyIndex = 1
        arrayRange.right.getStringValue.toInt - arrayRange.left.getStringValue.toInt + 1
      };
      case VDirection.`downto` => {
        multiplyIndex = -1
        arrayRange.left.getStringValue.toInt - arrayRange.right.getStringValue.toInt + 1
      }
    }

    vExpressionOption match{
      case Some(vExpression) => {
        val vSubtypeIndication = typeInfo.arrayTypeDeclarationMap(this).vSubtypeIndication
        val typeDefinition = VTypeDefinition(vSubtypeIndication.getSimpleName)
        vExpression.getAggregate match {
          case Some(vAggregate) => {
            var elementCount = 0;
            val iValList = new ListBuffer[IVal]
            val iArrayAttributeMap = mutable.Map.empty[String, Int]
            for (elementAssociation <- vAggregate.vElementAssociationList) {
              val vChoiceList = elementAssociation.choices match {
                case Some(vChoices) => vChoices.vChoiceList
                case None => ???
              }
              for (vChoice <- vChoiceList){
                if (!vChoice.getId.equals("others")) {
                  iValList += IVal(elementAssociation.vExpression)
                  iArrayAttributeMap += (vChoice.getId -> (startIndex + elementCount * multiplyIndex))
                  elementCount += 1;
                }
                else iValList += IVal_Val_array((arrayLength - elementCount), elementAssociation.vExpression)
              }
            }
            IdentifierMap.iArrayVariableMap += id -> iArrayAttributeMap
            arrayRange.vDirection match {
              case VDirection.to => IExpression_constantArrayType(this, IConstArrayTo_initialValue(IVal_Val_list(iValList.toList)), ExpArrayKindTo)
              case VDirection.downto => IExpression_constantArrayType(this, IConstArrayDownTo_initialValue(IVal_Val_rlist(iValList.toList)), ExpArrayKindDownTo)
            }
          }
          case None => ???
        }
      }
      case None => guessInitialValue(id, typeInfo)
    }
  }
}

class VTypeInfo(private[this] val typeInfo: Option[VTypeInfo]) {

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