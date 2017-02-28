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
sealed abstract class VVariableType {
  val s: String
}

object VVariableType {
  def apply(s: String): VVariableType = {
    if (VBaseType.scalars(s)) VScalarType(s)
    else if (VBaseType.vectors(s)) VVectorType(s)
    else if (VCustomizedType.subTypes(s))VSubtype(s)
    else if (VCompositeType.recordTypes(s)) VRecordType(s)
    else if (VCompositeType.arrayTypes(s)) VArrayType(s)
    else handler(s"${s}")
  }

  // [TN] This method is not complete because VSubtype might have original type is VRecordType
  def getOriginalType(vSubtypeIndication: VSubtypeIndication)(typeInfo: VTypeInfo): VBaseType = {
    VVariableType(vSubtypeIndication.getSimpleName) match {
      case vBaseType: VBaseType => vBaseType
      case vSubtype: VSubtype => getOriginalType(typeInfo.subtypeDeclarationMap(vSubtype))(typeInfo)
      case _ => ???
    }
  }
}

sealed abstract class VBaseType extends VVariableType {
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

  def guessInitialValue: IExpression_constantVBaseType = {
    val iconstS = s match {
      case "integer" => IConstS("val_i", "0")
      case "real" => IConstS("val_r", "0.0")
      case "character" => IConstS("val_c", "(CHR ''0'')")
      case "boolean" => IConstS("val_b", "True")
      case "std_ulogic" => IConstS("val_c", "(CHR ''0'')")
      case "std_logic" => IConstS("val_c", "(CHR ''0'')")
      case _ => handler(s"scalar unknown ${s}")
    }
    IExpression_constantVBaseType(this, iconstS, ExpScalarKind)
  }

  def getInitValFromLiteral(v: String): IExpression_constantVBaseType = {
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
    IExpression_constantVBaseType(this, iconstS, ExpScalarKind)
  }

  def getInitialValue(vExpressionOption: Option[VExpression])(defInfo: DefInfo): IExpression = vExpressionOption match {
    case Some(vExpression) => {
      val refined = vExpression.toIExp(defInfo) match {
        case iExpression: IExpression_constantVBaseType => IExpression_constantVBaseType(this, iExpression.iConst, ExpScalarKind)
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

// [TN] VVectorType is basically VArrayType
// [TN] We should merge VVectorType with VArrayType
case class VVectorType(s: String) extends VBaseType {
  require(s.endsWith(vectorFlag))

  def getVScalarType: VScalarType = VScalarType(s.substring(0, s.length - vectorFlag.length))

  def getInitialValue(id: String, vExplicitRange: VExplicitRange, vExpressionOption: Option[VExpression])(typeInfo: VTypeInfo): IExpression_constantVBaseType = {
    val startIndex = vExplicitRange.left.getStringValue
    val isRangeNumberFormat = V2IUtils.isAllDigits(vExplicitRange.left.getStringValue) && V2IUtils.isAllDigits(vExplicitRange.right.getStringValue)
    var multiplyIndex: Int = 1
    val arrayLength = vExplicitRange.vDirection match {
      case VDirection.`to` => {
        multiplyIndex = 1
        if (isRangeNumberFormat) vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1
        else s"(${vExplicitRange.right.getStringValue} - ${vExplicitRange.left.getStringValue} + 1)"
      }
      case VDirection.`downto` => {
        multiplyIndex = -1
        if (isRangeNumberFormat) vExplicitRange.left.getStringValue.toInt - vExplicitRange.right.getStringValue.toInt + 1
        else s"(${vExplicitRange.left.getStringValue} - ${vExplicitRange.right.getStringValue} + 1)"
      }
    }

    vExpressionOption match{
      case Some(vExpression) => {
        vExpression.getVAggregateOption match {
          case Some(vAggregate) => {
            var elementCount = 0;
            val iValList = new ListBuffer[IVal]
            val iArrayAttributeMap = mutable.Map.empty[String, String]
            for (elementAssociation <- vAggregate.vElementAssociationList) {
              val vChoiceList = elementAssociation.choices match {
                case Some(vChoices) => vChoices.vChoiceList
                case None => List.empty
              }
              if (vChoiceList != List.empty){
                for (vChoice <- vChoiceList){
                  if (!vChoice.getId.equals("others")) {
                    iValList += IVal(elementAssociation.vExpression)
                    if (isRangeNumberFormat) iArrayAttributeMap += (vChoice.getId -> (startIndex.toInt + elementCount * multiplyIndex).toString)
                    else if (multiplyIndex == 1) iArrayAttributeMap += (vChoice.getId -> s"${startIndex} + ${elementCount}")
                    else iArrayAttributeMap += (vChoice.getId -> s"${startIndex} - ${elementCount}")
                    elementCount += 1
                  }
                  else {
                    if (isRangeNumberFormat) iValList += IVal_Val_array((arrayLength.toString.toInt - elementCount).toString, elementAssociation.vExpression)
                    else iValList += IVal_Val_array(s"(${arrayLength} - ${elementCount})", elementAssociation.vExpression)
                  }
                }
              } else {
                iValList += IVal(elementAssociation.vExpression)
                elementCount += 1
              }
            }
            IdentifierMap.iArrayVariableMap += id -> iArrayAttributeMap
            vExplicitRange.vDirection match {
              case VDirection.to => IExpression_constantVBaseType(this, IConstArrayTo_initialValue(IVal_Val_list(iValList.toList)), ExpArrayKindTo)
              case VDirection.downto => IExpression_constantVBaseType(this, IConstArrayDownTo_initialValue(IVal_Val_rlist(iValList.toList)), ExpArrayKindDownTo)
            }
          }
          case None => vExpression.getLiteralS.arrayLiteralToExpression
        }
      }
      case None => guessInitialValue(vExplicitRange)
    }
  }

  // This has something to do with TO/DOWNTO but not about "character"/"std_logic"/"std_ulogic"
  def guessInitialValue(vExplicitRange: VExplicitRange, rawVal: Char = '0'): IExpression_constantVBaseType = {
    try {
      vExplicitRange.vDirection match {
        case VDirection.`to` => {
          val length = vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1;
          IExpression_constantVBaseType(this, IConstL_gen(this, length.toString, rawVal), ExpVectorKindTo)
        }
        case VDirection.`downto` => {
          val length = vExplicitRange.left.getStringValue.toInt - vExplicitRange.right.getStringValue.toInt + 1
          IExpression_constantVBaseType(this, IConstRL_gen(this, length.toString, rawVal), ExpVectorKindDownTo)
        }
      }
    } catch {
      case nfe : NumberFormatException => {
        vExplicitRange.vDirection match {
          case VDirection.`to` => {
            IExpression_constantVBaseType(this, IConstL_gen(this, s"(${vExplicitRange.right.getStringValue} - ${vExplicitRange.left.getStringValue} + 1)", rawVal), ExpVectorKindTo)
          }
          case VDirection.`downto` => {
            IExpression_constantVBaseType(this, IConstRL_gen(this, s"(${vExplicitRange.left.getStringValue} - ${vExplicitRange.right.getStringValue} + 1)", rawVal), ExpVectorKindDownTo)
          }
        }
      }
      case e : Exception => ???
    }
  }

  def getNullValue(): IExpression_constantVBaseType = {
    IExpression_constantVBaseType(this, IConstL_gen(this, s"0", '0'), ExpVectorKindTo)
  }
}

// CustomizedType does not exist in VHDL Parser
sealed abstract class VCustomizedType extends VVariableType

object VCustomizedType {
  var subTypes = Set("");
}

// VSubtype should be separate with VVariableType
// VSubtype should not exist because it always be able to convert to one of the other standard type
case class VSubtype(s: String) extends VCustomizedType {

  def getOriginalType(typeInfo: VTypeInfo): VVariableType = {
    val vSubtypeIndication = typeInfo.subtypeDeclarationMap(this)
    val vTypeDefinition = VVariableType(vSubtypeIndication.getSimpleName)
    vTypeDefinition match {
      case vSubType: VSubtype => vSubType.getOriginalType(typeInfo)
      case _ => vTypeDefinition
    }
  }

  def guessInitialValue(itemId: String)(typeInfo : VTypeInfo): MetaData = {
    val vSubtypeIndication = typeInfo.subtypeDeclarationMap(this)
    val vTypeDefinition = VVariableType(vSubtypeIndication.getSimpleName)
    vTypeDefinition match {
      case vBaseType: VBaseType => {
        val iExpression_constantVBaseType = vBaseType match {
          case vScalarType: VScalarType => vScalarType.guessInitialValue
          case vVectorType: VVectorType => {
            vSubtypeIndication.getExplicitRangeOption match {
              case Some(vExplicitRange) => vVectorType.guessInitialValue(vExplicitRange)
              case None => ???
            }
          }
        }
        MetaData(itemId, vBaseType, iExpression_constantVBaseType)
      }
      case vCustomizedType : VCustomizedType => vCustomizedType match{
        case vSubtype : VSubtype => vSubtype.guessInitialValue(itemId)(typeInfo)
        case vRecordType : VRecordType => handler(s"${vRecordType}")  // Subtype of a record is not implemented
        case vArrayType : VArrayType => handler(s"${vArrayType}")
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
      val valType = VVariableType(vSubtypeIndication.getSimpleName)
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
          case subtype : VSubtype => subtype.guessInitialValue(itemId)(typeInfo)
          case recordType : VRecordType => MetaData(itemId, recordType, IExpression_constantRecordType(recordType, IConstRecord_gen(recordType), ExpRecordKind))
          case vArrayType : VArrayType => {
            vArrayType.getIExpressionOption(itemId, typeInfo) match {
              case Some(iExpression) => MetaData(itemId, vArrayType, iExpression)
              case None => ???
            }
          }
        }
      }
    }
    iVals.toList
  }

  // [HC] vExpressionOption is taken from definition; but type information should be record type declaration
  def getInitialValue(typeInfo: VTypeInfo, vExpressionOption: Option[VExpression])(defInfo: DefInfo): List[MetaData] = {
    val iVals: Seq[MetaData] = vExpressionOption match {
      case Some(vExpression) => {
        vExpression.getVPrimaryOption match {
          case Some(vAggregate: VAggregate) => {
            val recordInfo = typeInfo.recordTypeDeclarationMap(this)
            val aggregateIdExpMap = vAggregate.getAssoc
            for {
              (itemId, vSubtypeIndication) <- recordInfo
            } yield {
              val itemExp = aggregateIdExpMap(itemId)
              val itemValType = VVariableType(vSubtypeIndication.getSimpleName)
              val initVal: IExpression = itemValType match {
                case bt: VBaseType => bt match {
                  case st: VScalarType => {
                    st.getInitialValue(Option(itemExp))(defInfo)
                  }
                  case vt: VVectorType => {
                    itemExp.getVAggregateOption match {
                      case Some(itemAggregate) => {
                        val (fieldName, innerExp) = itemAggregate.getFirstMap
                        if (fieldName == "others") {
                          val vExplicitRange = vSubtypeIndication.getExplicitRangeOption match {
                            case Some(vExplicitRange) => vExplicitRange
                            case None => ???
                          }
                          val numericVal = innerExp.getVPrimaryOption
                          numericVal match {
                            case Some(p) => {
                              val rawVal = p.getStringValue
                              require(rawVal.length == 3 && rawVal.head == '\'' && rawVal.last == '\'')
                              vt.guessInitialValue(vExplicitRange, rawVal(1))
                            }
                            case None => handler(s"${numericVal}")
                          }
                        } else handler(s"${itemValType}")
                      }
                      case None => handler(s"${itemExp}")
                    }
                  }
                }
                case vCustomizedType: VCustomizedType => handler(s"${vCustomizedType}")
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

  def getVExplicitRange (typeInfo : VTypeInfo) : VExplicitRange = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    constrainedArrayDefinition.indexConstraint.getExplicitRange
  }

  def getElementType(typeInfo: VTypeInfo): VVariableType = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val vSubtypeIndication = constrainedArrayDefinition.vSubtypeIndication
    val vTypeDefinition = VVariableType(vSubtypeIndication.getSimpleName)
    vTypeDefinition match {
      case vSubType: VSubtype => vSubType.getOriginalType(typeInfo)
      case _ => vTypeDefinition
    }
  }

  // TODO : This implementation only consider 1 dimensional array - Need to implement multi-dimensional array
  def getIExpressionOption(itemId: String, typeInfo : VTypeInfo): Option[IExpression] = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val vSubtypeIndication = constrainedArrayDefinition.vSubtypeIndication
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    val iExpression_constantOption = vVariableType match {
      case scalarType : VScalarType => Some(scalarType.guessInitialValue.iConst)
      case vectorType : VVectorType => {
        vSubtypeIndication.getExplicitRangeOption match {
          case Some(vExplicitRange) => Some(vectorType.guessInitialValue(vExplicitRange).iConst)
          case None => ???
        }
       }
      case subType : VSubtype => Some(subType.guessInitialValue(itemId)(typeInfo).iExpression.asInstanceOf[IExpression_constant].iConst)
      case vRecordType : VRecordType => None
      case arrayType : VArrayType => handler(s"${arrayType}")
    }

    iExpression_constantOption match {
      case Some(iExpression_constant) => {
        val vExplicitRange = constrainedArrayDefinition.indexConstraint.getExplicitRange
        val isVExplicitRangeNumberFormat = V2IUtils.isAllDigits(vExplicitRange.left.toString) && V2IUtils.isAllDigits(vExplicitRange.right.toString)
        vExplicitRange.vDirection match {
          case VDirection.`to` => {
            val length =
              if (isVExplicitRangeNumberFormat) vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1;
              else s"(${vExplicitRange.right.getStringValue} - ${vExplicitRange.left.getStringValue} + 1)"
            Some(IExpression_constantArrayType(this, IConstArrayTo_generate(this, length.toString, iExpression_constant), ExpArrayKindTo))
          }
          case VDirection.`downto` => {
            val length =
              if (isVExplicitRangeNumberFormat) vExplicitRange.left.getStringValue.toInt - vExplicitRange.right.getStringValue.toInt + 1
              else s"(${vExplicitRange.left.getStringValue} - ${vExplicitRange.right.getStringValue} + 1)"
            Some(IExpression_constantArrayType(this, IConstArrayDownTo_generate(this, length.toString, iExpression_constant), ExpArrayKindDownTo))
          }
        }
      }
      case None => None
    }
  }

  def guessInitialValue(itemId: String, typeInfo : VTypeInfo)(defInfo : DefInfo): V_IDef = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val vSubtypeIndication = constrainedArrayDefinition.vSubtypeIndication
    val vVariableType = VVariableType(vSubtypeIndication.getSimpleName)
    val vExplicitRange = constrainedArrayDefinition.indexConstraint.getExplicitRange
    vVariableType match {
      case vRecordType : VRecordType => {
        val elementNumber = getVExplicitRange(typeInfo).vDirection match {
          case VDirection.`to` => vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1
          case VDirection.`downto` => ???
        }
        var iVl_VnlList = for {
          index <- 0 to (elementNumber-1)
          metaDataList = vRecordType.guessInitVals(typeInfo)
          // [TN] Might change the name to s"${itemId}_${index}"
          iVl_Vnl = IVl_Vnl.generate(itemId, metaDataList)(typeInfo, defInfo)
        } yield iVl_Vnl
        IVl_Vnl(itemId, iVl_VnlList.toList)
      }
      case _ => {
        getIExpressionOption(itemId, typeInfo) match {
          case Some(iExpression) => IVariable(itemId, VVariableType.getOriginalType(vSubtypeIndication)(typeInfo), iExpression)
          case None => ???
        }
      }
    }
  }

  def getInitialValue(id:String, typeInfo: VTypeInfo, vExpressionOption: Option[VExpression])(defInfo: DefInfo): V_IDef = {
    val constrainedArrayDefinition = typeInfo.arrayTypeDeclarationMap(this)
    val vExplicitRange = constrainedArrayDefinition.indexConstraint.getExplicitRange
    val startIndex = vExplicitRange.left.getStringValue
    val isRangeNumberFormat = V2IUtils.isAllDigits(vExplicitRange.left.getStringValue) && V2IUtils.isAllDigits(vExplicitRange.right.getStringValue)
    var multiplyIndex: Int = 1
    val arrayLength = vExplicitRange.vDirection match {
      case VDirection.`to` => {
        multiplyIndex = 1
        if (isRangeNumberFormat) vExplicitRange.right.getStringValue.toInt - vExplicitRange.left.getStringValue.toInt + 1
        else s"${vExplicitRange.right.getStringValue} - ${vExplicitRange.left.getStringValue} + 1"
      }
      case VDirection.`downto` => {
        multiplyIndex = -1
        if (isRangeNumberFormat) vExplicitRange.left.getStringValue.toInt - vExplicitRange.right.getStringValue.toInt + 1
        else s"${vExplicitRange.left.getStringValue} - ${vExplicitRange.right.getStringValue} + 1"
      }
    }

    vExpressionOption match{
      case Some(vExpression) => {
        val iExpression = vExpression.getVAggregateOption match {
          case Some(vAggregate) => {
            var elementCount = 0;
            val iValList = new ListBuffer[IVal]
            val iArrayAttributeMap = mutable.Map.empty[String, String]
            for (elementAssociation <- vAggregate.vElementAssociationList) {
              val vChoiceList = elementAssociation.choices match {
                case Some(vChoices) => vChoices.vChoiceList
                case None => List.empty
              }
              if (vChoiceList != List.empty){
                for (vChoice <- vChoiceList){
                  if (!vChoice.getId.equals("others")) {
                    iValList += IVal(elementAssociation.vExpression)
                    if (isRangeNumberFormat) iArrayAttributeMap += (vChoice.getId -> (startIndex.toInt + elementCount * multiplyIndex).toString)
                    else if (multiplyIndex == 1) iArrayAttributeMap += (vChoice.getId -> s"${startIndex} + ${elementCount}")
                    else iArrayAttributeMap += (vChoice.getId -> s"${startIndex} - ${elementCount}")
                    elementCount += 1
                  }
                  else {
                    if (isRangeNumberFormat) iValList += IVal_Val_array((arrayLength.toString.toInt - elementCount).toString, elementAssociation.vExpression)
                    else iValList += IVal_Val_array(s"(${arrayLength} - ${elementCount})", elementAssociation.vExpression)
                  }
                }
              } else {
                iValList += IVal(elementAssociation.vExpression)
                elementCount += 1
              }
            }
            IdentifierMap.iArrayVariableMap += id -> iArrayAttributeMap
            vExplicitRange.vDirection match {
              case VDirection.to => IExpression_constantArrayType(this, IConstArrayTo_initialValue(IVal_Val_list(iValList.toList)), ExpArrayKindTo)
              case VDirection.downto => IExpression_constantArrayType(this, IConstArrayDownTo_initialValue(IVal_Val_rlist(iValList.toList)), ExpArrayKindDownTo)
            }
          }
          case None => ???
        }
        val vSubtypeIndication = constrainedArrayDefinition.vSubtypeIndication
        IVariable(id, VVariableType.getOriginalType(vSubtypeIndication)(typeInfo), iExpression)
      }
      case None => guessInitialValue(id, typeInfo)(defInfo)
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