package core

import core.isabellesyntax._
import core.vhdlsyntax._

import scala.collection.mutable

/**
  * Created by Hongxu Chen.
  */
abstract class Keeper(vInfo: Option[VInfo]) {

  var entity: IEntity = _

  val conc_stmt_complexes = mutable.ArrayBuffer.empty[IConc_stmt_complex]

  val defInfo = new DefInfo(vInfo.map(_.defInfo))

  val typeInfo = new VTypeInfo(vInfo.map(_.typeInfo))

  val definedEntities = mutable.ArrayBuffer.empty[String]

  def generateIVariable(id: String, vExpressionOption: Option[VExpression], vSubtypeIndication: VSubtypeIndication): Unit = {
    val vTypeDefinition = VTypeDefinition(vSubtypeIndication.getSimpleName)
    vTypeDefinition match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.getInitialValue(vExpressionOption)(defInfo)
          case vVectorType: VVectorType => vSubtypeIndication.getExplicitRangeOption match {
            case Some(vExplicitRange) => vVectorType.getInitialValue(vExplicitRange, vExpressionOption)
            case None => ???
          }
        }
        val iVariable = IVariable(id, vBaseType, iExpression)
        defInfo += (id, iVariable)
      }
      case vCustomizedType : VCustomizedType => vCustomizedType match{
        case vSubtype : VSubtype => {
          val vSubtypeIndication = typeInfo.subtypeDeclarationMap(vSubtype)
          generateIDefinition(id, vExpressionOption, vSubtypeIndication, defInfo, typeInfo) match{
            case iVariable : IVariable => defInfo += (id, iVariable)
            case iVl_Vnl : IVl_Vnl => defInfo += (id, iVl_Vnl)
            case _ => throw VIError
          }
        }
        case vRecordType : VRecordType => {
          val initVals = vRecordType.getInitialValue(typeInfo, vExpressionOption)(defInfo)
          val iVl_Vnl = IVl_Vnl.gen(id, initVals)
          defInfo +=(id, iVl_Vnl)
        }
        case vArrayType : VArrayType => {
          val vSubtypeIndication = typeInfo.arrayTypeDeclarationMap(vArrayType).vSubtypeIndication
          val iExpression = vArrayType.getInitialValue(id, typeInfo, vExpressionOption)(defInfo)
          val iVariable = IVariable(id, VTypeDefinition.getOriginalType(vSubtypeIndication)(typeInfo), iExpression)
          defInfo += (id, iVariable)
        }
      }
    }
  }

  def generateIPort(id: String, expressionOption: Option[VExpression], vSubtypeIndication: VSubtypeIndication, portMode: PortMode.Value, portConnection: PortConnection.Value): Unit = {
    val valType = VTypeDefinition(vSubtypeIndication.getSimpleName)
    valType match {
      case bt: VBaseType => {
        val initVal = bt match {
          case st: VScalarType => st.getInitialValue(expressionOption)(defInfo)
          case vVectorType: VVectorType => vSubtypeIndication.getExplicitRangeOption match {
            case Some(r) => {
              vVectorType.getInitialValue(r, expressionOption)
            }
            case None => ???
          }
        }
        val port = Port_baseType(id, bt, initVal, portMode, portConnection)
        defInfo +=(id, port)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.getInitialValue(typeInfo, expressionOption)(defInfo)
          val spnl = ISpl_Spnl(id, initVals, portMode, portConnection, typeInfo)
          defInfo +=(id, spnl)
        }
        case arrayType :VArrayType => handler(s"${arrayType}")
      }
    }
  }

  def genISignal(name: String, vSubtypeIndication: VSubtypeIndication, signalKind: SignalKind.Value): Unit = {
    val valType = VTypeDefinition(vSubtypeIndication.getSimpleName)
    valType match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.guessInitialValue
          case vVectorType: VVectorType => {
            vSubtypeIndication.getExplicitRangeOption match {
              case Some(vExplicitRange) => vVectorType.guessInitialValue(vExplicitRange)
              case None => ???
            }
          }
        }
        val signal = Signal(name, vBaseType, iExpression, signalKind)
        defInfo +=(name, signal)
      }
      case ct: VCustomizedType => ct match {
        case subtype : VSubtype => handler(s"${subtype}")
        case recordType : VRecordType => {
          val initVals = recordType.guessInitVals(typeInfo)
          val iSpl_Spnl = ISpl_Spnl(name, initVals, signalKind)
          defInfo +=(name, iSpl_Spnl)
        }
        case arrayType : VArrayType => handler(s"${arrayType}")
      }
    }
  }

  // [TN] This method is called when trying to get subtype definition
  def generateIDefinition(id: String, vExpressionOption: Option[VExpression], subtypeIndication: VSubtypeIndication, defInfo: DefInfo, vTypeInfo: VTypeInfo) : V_IDef = {
    val vTypeDefinition = VTypeDefinition(subtypeIndication.getSimpleName)
    vTypeDefinition match {
      case vBaseType: VBaseType => {
        val iExpression = vBaseType match {
          case vScalarType: VScalarType => vScalarType.getInitialValue(vExpressionOption)(defInfo)
          case vVectorType: VVectorType => subtypeIndication.getExplicitRangeOption match {
            case Some(vExplicitRange) => vVectorType.getInitialValue(vExplicitRange, vExpressionOption)
            case None => ???
          }
        }
        IVariable(id, vBaseType, iExpression)
      }
      case vCustomizedType : VCustomizedType => vCustomizedType match{
        case subtype : VSubtype => {
          val subtypeIndication = vTypeInfo.subtypeDeclarationMap(subtype)
          generateIDefinition(id, vExpressionOption, subtypeIndication, defInfo, vTypeInfo)
        }
        case vRecordType : VRecordType => {
          val initVals = vRecordType.getInitialValue(vTypeInfo, vExpressionOption)(defInfo)
          IVl_Vnl.gen(id, initVals)
        }
        case vArrayType : VArrayType => handler(s"${vArrayType}")
      }
    }
  }
}